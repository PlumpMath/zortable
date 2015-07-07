(ns test.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [test.zortable.editable]))

(doo-tests 'test.zortable.editable)
