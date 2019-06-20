;; When executed, this file will run a basic web server
;; on http://localhost:8080, which will tell you how many
;; times you have visited the page.

(ns clj-notes.ring-session
  (:use ring.middleware.session
        ring.util.response
        ring.adapter.jetty))

(defn handler [{session :session, uri :uri}]
  (let [n (session :n 1)]
    (if (= uri "/")
      (-> (response (str "You have visited " n " times"))
          (content-type "text/plain")
          (assoc-in [:session :n] (inc n)))
      (-> (response "Page not found")
          (status 404)))))


(defn -main
  "I start a server which counts lines of text in your .txt files :)"
  []
  (run-jetty (-> handler wrap-session)
                   {:port 3000}))