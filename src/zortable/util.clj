(ns zortable.util)

;; WIP
(defmacro go-clean
  "Like (go (loop ...))"
  [exit-ch bindings & body]
  `(go-loop bindings
     (when-not (.-closed exit-ch)
        ~@body)))
