(ns learnclj.file-upload
  (:require [ring.middleware.params :refer :all]
            [ring.util.response :refer :all]
            [ring.middleware.multipart-params :refer :all]
            [ring.adapter.jetty :as jetty])
  (:gen-class))

(defn- num-lines
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (count (line-seq rdr))))

(defn file-handler
  [{{{tempfile :tempfile filename :filename} "file"} :params :as request}]
  (println request)
  (let [n (num-lines tempfile)]
    (response (str "File " filename " has " n " lines "))))


(defn -main
  "I start a server which counts lines of text in your .txt files :)"
  [& args]
  (jetty/run-jetty (-> file-handler
                       wrap-params
                       wrap-multipart-params)
                   {:port 3000}))