(ns weather
  (:refer-clojure :exclude [format])
  (:require [clojure.string    :as str]
            [clojure.edn       :as edn]
            [clojure.data.json :as json]))


(def config (-> ".scripts-config.edn" slurp edn/read-string))

(def city (:city config))
(def app-id (:weather-app-id config))
(def weather-api-url (:weather-api-url config))


(defn fetch-weather-info [city]
  (-> (str weather-api-url
           "?q=" city
           "&appid=" app-id
           "&units=metric")
      (slurp)
      (json/read-str :key-fn keyword)))


(defn- try-simplify [city]
  (case city
    "Yekaterinburg" "EKB"
    "Verkhnyaya Pyshma" "VP"
    city))


(defn- colorize [temp color]
  (case color
    :blue   (str "<fc=#006ccc>" temp "</fc>")
    :yellow (str "<fc=#fcf304>" temp "</fc>")
    :orange (str "<fc=#fc6b04>" temp "</fc>")
    :red    (str "<fc=#ff0000>" temp "</fc>")))


(defn- try-colorize [temp]
  (cond 
    (>= 0 temp)      (colorize temp :blue)
    (>= 19 temp 15)  (colorize temp :yellow)
    (>= 25 temp 20)  (colorize temp :orange)
    (>= temp 26)     (colorize temp :red)
    :else            (str temp)))


(defn format [info]
  (let [city           (-> info :name)
        temperature    (-> info :main :temp)
        weather-status (-> info :weather first :main)]
   (str (try-simplify city) ": "
        (try-colorize (int temperature)) "°C, "
        (str/lower-case weather-status))))


(defn -main []
  (try 
    (-> city
        fetch-weather-info
        format)
  (catch Exception e "Can't update weather")))


(println (-main))