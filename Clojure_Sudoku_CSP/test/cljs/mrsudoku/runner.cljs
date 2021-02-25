(ns mrsudoku.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [mrsudoku.core-test]))

(doo-tests 'mrsudoku.core-test)
