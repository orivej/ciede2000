(asdf:defsystem ciede2000
  :description "CIEDE2000 color difference function"
  :author "Orivej Desh <orivej@gmx.fr>"
  :licence "Unlicense" ; http://unlicense.org/UNLICENSE
  :depends-on (let-plus)
  :components ((:file "ciede2000")))

(asdf:defsystem ciede2000-test
  :depends-on (ciede2000 fiveam)
  :components ((:file "tests")))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :ciede2000))))
  (asdf:load-system :ciede2000-test)
  (funcall (intern (string '#:run!) :fiveam) :ciede2000))
