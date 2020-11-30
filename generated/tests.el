(ert-deftest format-conf()
(let ((numerical-parameters '("number-of-CPU-cores"))
      (number-of-CPU-cores 123))
  (should (string= "number-of-CPU-cores=123"
     (format-conf "number-of-CPU-cores")))))
