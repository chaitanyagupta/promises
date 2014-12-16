(asdf:defsystem "promises"
  :depends-on (:iolib)
  :serial t
  :components ((:file "packages")
               (:file "runloop")
               (:file "promise")
               (:file "pcl")
               (:file "test")
               (:file "test1")
               (:file "test2")))
