(ns ^:figwheel-hooks mrsudoku.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [mrsudoku.events :as events]
   [mrsudoku.views :as views]
   [mrsudoku.config :as config]
   ))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "<mrsudoku> in dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (re-frame/dispatch-sync [:initialize])
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:after-load re-render []
  (when config/debug?
    (println "<mrsudoku> re-render hook"))
  (mount-root))

(defonce start-up
  (do (dev-setup)
      (mount-root)
      true))
