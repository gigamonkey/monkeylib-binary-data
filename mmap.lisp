(in-package :binary-io)

(defclass mmap-stream (sb-gray:fundamental-binary-stream)
  ((fd :initarg :fd :reader mmap-stream-fd)
   (address :initarg :address :reader mmap-stream-address)
   (offset :initform 0 :accessor mmap-stream-offset)))

(defun make-mmap-stream (fd &optional (direction :input))
  (let* ((prot (ecase direction
                 (:input sb-posix:prot-read)
                 ((:output :io) (logior sb-posix:prot-read sb-posix:prot-write))))
         (flags (ecase direction
                  (:input sb-posix:map-private)
                  ((:output :io) sb-posix:map-shared)))
         (address (sb-posix:mmap nil (file-length fd) prot flags fd 0)))
    (make-instance 'mmap-stream :fd fd :address address)))

(defun open-mmap (filename &key (direction :input) (if-exists :overwrite) (if-does-not-exist :error))
  (let ((fd (open filename :direction direction :element-type '(unsigned-byte 8)
                           :if-exists if-exists
                           :if-does-not-exist if-does-not-exist)))
    (make-mmap-stream fd direction)))

;;; some gray-stream methods

(defmethod close ((stream mmap-stream) &key abort)
  (let ((fd (mmap-stream-fd stream)))
    (sb-posix:munmap (mmap-stream-address stream) (file-length fd))
    (close fd :abort abort)))

(defmethod sb-gray:stream-file-position ((stream mmap-stream) &optional position)
  (with-accessors ((offset mmap-stream-offset)) stream
    (if position
        (setf offset position)
        offset)))

(defmethod sb-gray:stream-read-byte ((stream mmap-stream))
  (prog1
      (sb-sys:sap-ref-8 (mmap-stream-address stream) (mmap-stream-offset stream))
    (incf (mmap-stream-offset stream))))

(defmethod sb-gray:stream-read-sequence ((stream mmap-stream)
                                 (seq sequence)
                                 &optional (start 0) (end nil))
  (with-accessors ((address mmap-stream-address)
                   (offset mmap-stream-offset)) stream
    (loop with n = (- (or end (length seq)) start)
          for i from start
          for j below n
          do (setf (elt seq i) (sb-sys:sap-ref-8 address (+ offset j)))
          finally (incf offset n)
                  (return n))))

(defmethod sb-gray:stream-write-sequence ((stream mmap-stream)
                                  (seq sequence)
                                  &optional (start 0) (end nil))
  (with-accessors ((address mmap-stream-address)
                   (offset mmap-stream-offset)) stream
    (loop with n = (- (or end (length seq)) start)
          for i from start
          for j below n
          do (setf (sb-sys:sap-ref-8 address (+ offset j)) (elt seq i))
          finally (incf offset n)
                  (return seq))))

;;; methods for some common datatypes

(defmacro make-read-write-value (type-name accessor)
  (with-gensyms (type stream value)
    `(progn
       (defmethod read-value ((,type (eql ,type-name)) (,stream mmap-stream) &key)
         (with-accessors ((address mmap-stream-address)
                          (offset mmap-stream-offset)) ,stream
           (prog1
               (,accessor address offset)
             (incf offset (type-size ,type-name)))))
       (defmethod write-value ((,type (eql ,type-name)) (,stream mmap-stream) ,value &key)
         (with-accessors ((address mmap-stream-address)
                          (offset mmap-stream-offset)) ,stream
           (setf (,accessor address offset) ,value)
           (incf offset (type-size ,type-name)))))))

(make-read-write-value :u8 sb-sys:sap-ref-8)
(make-read-write-value :u16 sb-sys:sap-ref-16)
(make-read-write-value :u32 sb-sys:sap-ref-32)
(make-read-write-value :u64 sb-sys:sap-ref-64)

(make-read-write-value :s8 sb-sys:signed-sap-ref-8)
(make-read-write-value :s16 sb-sys:signed-sap-ref-16)
(make-read-write-value :s32 sb-sys:signed-sap-ref-32)
(make-read-write-value :s64 sb-sys:signed-sap-ref-64)

(make-read-write-value :float32 sb-sys:sap-ref-single)
(make-read-write-value :float64 sb-sys:sap-ref-double)

;; Redefines write for floats to include casting
(defmethod write-value ((type (eql :float32)) (stream mmap-stream) value &key)
  (with-accessors ((address mmap-stream-address)
                   (offset mmap-stream-offset)) stream
    (setf (sb-sys:sap-ref-single address offset) (float value 0f0))
    (incf offset 4)))

(defmethod write-value ((type (eql :float64)) (stream mmap-stream) value &key)
  (with-accessors ((address mmap-stream-address)
                   (offset mmap-stream-offset)) stream
    (setf (sb-sys:sap-ref-double address offset) (float value 0d0))
    (incf offset 8)))

;; Vectors
(defmethod read-value ((vector (eql :vector)) (stream mmap-stream) &key size type)
  (loop with arr = (make-array size)
        for i below size do (setf (aref arr i) (read-value type stream))
        finally (return arr)))
