(in-package :binary-io)

(defclass mmap-stream (sb-gray:fundamental-binary-stream)
  ((fd :initarg :fd :reader mmap-stream-fd)
   (address :initarg :address :reader mmap-stream-address)))

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

(defgeneric read-value-at (type stream offset &key)
  (:documentation "Read a value of the given TYPE from the STREAM at OFFSET."))

(defgeneric write-value-at (type stream offset value &key)
  (:documentation "Write a VALUE of the given TYPE to the STREAM at OFFSET."))

;;; gray-stream method for closing

(defmethod close ((stream mmap-stream) &key abort)
  (let ((fd (mmap-stream-fd stream)))
    (sb-posix:munmap (mmap-stream-address stream) (file-length fd))
    (close fd :abort abort)))

;;; methods for some common datatypes

(defmacro make-read-write-value-at (type-name accessor)
  (with-gensyms (type stream value offset)
    `(progn
       (defmethod read-value-at ((,type (eql ,type-name)) (,stream mmap-stream) ,offset &key)
         (with-accessors ((address mmap-stream-address)) ,stream
           (,accessor address ,offset)))
       (defmethod write-value-at ((,type (eql ,type-name)) (,stream mmap-stream) ,offset ,value &key)
         (with-accessors ((address mmap-stream-address)) ,stream
           (setf (,accessor address ,offset) ,value))))))

(make-read-write-value-at :u8 sb-sys:sap-ref-8)
(make-read-write-value-at :u16 sb-sys:sap-ref-16)
(make-read-write-value-at :u32 sb-sys:sap-ref-32)
(make-read-write-value-at :u64 sb-sys:sap-ref-64)

(make-read-write-value-at :s8 sb-sys:signed-sap-ref-8)
(make-read-write-value-at :s16 sb-sys:signed-sap-ref-16)
(make-read-write-value-at :s32 sb-sys:signed-sap-ref-32)
(make-read-write-value-at :s64 sb-sys:signed-sap-ref-64)

(make-read-write-value-at :float32 sb-sys:sap-ref-single)
(make-read-write-value-at :float64 sb-sys:sap-ref-double)

;; Redefines write for floats to include casting
(defmethod write-value-at ((type (eql :float32)) (stream mmap-stream) offset value &key)
  (with-accessors ((address mmap-stream-address)) stream
    (setf (sb-sys:sap-ref-single address offset) (float value 0f0))))

(defmethod write-value-at ((type (eql :float64)) (stream mmap-stream) offset value &key)
  (with-accessors ((address mmap-stream-address)) stream
    (setf (sb-sys:sap-ref-double address offset) (float value 0d0))))

;; Vectors
(defmethod read-value-at ((outer-type (eql :vector)) (stream mmap-stream) offset &key size type)
  (declare (ignore outer-type))
  (loop with arr = (make-array size)
        with sz = (type-size type)
        for off = offset then (+ off sz)
        for i below size
        do (setf (aref arr i) (read-value-at type stream off))
        finally (return arr)))

(defmethod write-value-at ((outer-type (eql :vector)) (stream mmap-stream) offset value &key type)
  (declare (ignore outer-type))
  (loop with sz = (type-size type)
        for elt across value
        for off = offset then (+ off sz)
        do (write-value-at type stream off elt)))
