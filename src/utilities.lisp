(in-package utilities)

(defmacro dofor ((var iterable) &body body)
  "Similar to dolist, but for iterators. Run BODY once for each value
in the iterator with the symbol given in VAR bound to the current value."
  (with-gensyms (it)
    `(loop with ,it = (iterator ,iterable)
           until (endp ,it)
           do
              (let ((,var (at ,it)))
                (advance ,it)
                ,@body))))
