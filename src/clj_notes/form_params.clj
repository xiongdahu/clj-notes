;; When executed, this file will run a basic web server
;; on http://localhost:8080.

(ns clj-notes.form-params
  (:use ring.middleware.params
        ring.util.response
        ring.adapter.jetty))

(defn page [name]
  (str "<html><body>"
       (if name
         (str "Nice to meet you, " name "!")
         (str "<form>"
              "Name: <input name='name' type='text'>"
              "<input type='submit'>"
              "</form>"))
       "</body></html>"))

(defn handler [{{name "name"} :params}]
  (-> (response (page name))
      (content-type "text/html")))

(def app
  (-> handler
      wrap-params))

(defn -main
  "start the server..."
  []
  (run-jetty app {:port 8080}))