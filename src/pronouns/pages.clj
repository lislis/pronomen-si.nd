;; pronoun.is - a website for pronoun usage examples
;; Copyright (C) 2014 - 2016 Morgan Astra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

(ns pronouns.pages
  (:require [clojure.string :as s]
            [pronouns.config :refer [*pronouns-table*]]
            [pronouns.util :as u]
            [hiccup.core :refer :all]
            [hiccup.util :refer [escape-html]]))

(defn prose-comma-list
  [items]
  (let [c (count items)]
    (cond
      (<= c 1) (or (first items) "")
      (= c 2) (s/join " und " items)
      :else (str (s/join ", " (butlast items)) ", and " (last items)))))

(defn href
  [url text]
  [:a {:href url} text])

(defn wrap-pronoun
  [pronoun]
  [:b pronoun])

(defn render-sentence [& content]
  [:p [:span.sentence content]])

; Fälle Beispiele
(defn nominativ-beispiel
  [nominativ]
  (render-sentence (wrap-pronoun (s/capitalize nominativ)) " kocht eine leckere Suppe."))

; (defn genetiv-beispiel
;   [genetiv]
;   (render-sentence "I went with " (wrap-pronoun genetiv) "."))

(defn akkusativ-beispiel
  [nominativ akkusativ]
  (render-sentence (wrap-pronoun (s/capitalize nominativ))
                   " hat "
                   (wrap-pronoun akkusativ)
                   " gestern gesehen."))

(defn dativ-beispiel
  [dativ]
  (render-sentence "Ich möchte mit "
                   (wrap-pronoun dativ)
                   " ins Kino gehen."))

(defn possessiv-beispiel
  [nominativ possessiv]
  (render-sentence (wrap-pronoun (s/capitalize possessiv))
                   "e Katze hat auf dem Sofa gepinkelt."))
(defn relativ-n-beispiel
  [nominativ relativ-n]
  (render-sentence (wrap-pronoun (s/capitalize relativ-n))
                   " zieht nächstes Jahr nach Berlin. "))
(defn relativ-d-beispiel
  [nominativ relativ-d]
  (render-sentence "Er hat "
                   (wrap-pronoun relativ-d)
                   " ein Bademantel zum Geburtstag geschenkt."))

; Divs
(defn header-block [header]
  [:div {:class "section title"}
   (href "/" [:h1 header])])

(defn examples-block
  [nominativ akkusativ dativ genetiv possessiv relativ-n relativ-d]
  (let [sub-obj (s/join "/" [nominativ akkusativ])
        header-str (str "Hier sind einige Beispiele wie die Pronomen ("
                        sub-obj
                        ") benutze:")]
    [:div {:class "section examples"}
     [:h2 header-str]
     [:p (nominativ-beispiel nominativ)
         ; (genetiv-beispiel genetiv)
         (akkusativ-beispiel nominativ akkusativ)
         (dativ-beispiel dativ)
         (possessiv-beispiel nominativ possessiv)
         (relativ-n-beispiel nominativ relativ-n)
         (relativ-d-beispiel nominativ relativ-d)]]))

(defn usage-block []
  [:div {:class "section usage"}
   [:p ""
       [:tt "http://pronoun.is/nominativ/genetiv/akkusativ/dativ/possessiv/relativ-nominativ/relativ-dativ"]
       " zeigt Beispielesätze für ein beliebiges Pronomen an."]
   [:p "Das ist noch etwas umständlich. Wenn das Pronomen, das du verwendest, bereits in unserem System enthalten ist, reicht es, lediglich den Nominativ einzugaben."]])

(defn contact-block []
  (let [twitter-name (fn [handle] (href (str "https://www.twitter.com/" handle)
                                       (str "@" handle)))]
    [:div {:class "section contact"}
    ;  [:p "This is "
    ;      (twitter-name "morganastra")
    ;      ", whose "
    ;      (href "http://pronoun.is/ze/zir?or=she" "pronoun.is/ze/zir?or=she")]
     [:p "Pronomen-si.nd ist freie Software unter der "
         (href "https://www.gnu.org/licenses/agpl.html" "AGPLv3")
         "! Besuche das Projekt auf "
         (href "https://github.com/eribloodlust/pronomen-si.nd" "github")]
     [:p "<3"]]))

(defn footer-block []
  [:footer (usage-block) (contact-block)])

; Format
(defn format-pronoun-examples
  [pronoun-declensions]
  (let [sub-objs (map #(s/join "/" (take 2 %)) pronoun-declensions)
        title (str "Pronomen-si.nd: " (prose-comma-list sub-objs) " Beispiele")]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       (map #(apply examples-block %) pronoun-declensions)
       (footer-block)]])))

(defn lookup-pronouns [pronouns-string]
  (let [inputs (s/split pronouns-string #"/")
        n (count inputs)]
    (if (>= n 7)
      (take 7 inputs)
      (u/table-lookup inputs *pronouns-table*))))

(defn make-link [path]
  (let [link (str "/" path)
        label path]
    [:li (href link label)]))

(defn front []
  (let [abbreviations (u/abbreviate *pronouns-table*)
        links (map make-link abbreviations)
        title "Pronomen-si.nd"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
       [:div {:class "section table"}
       [:p "pronomen-si.nd ist eine Webseite für Benutzungsbeispiele von Personalpronomen."]
       [:p "Hier sind ein paar Pronomen die diese Seite kennt:"]
       [:ul links]]]
      (footer-block)])))

(defn not-found []
  (let [title "Pronomen-si.nd: Deutsche Sprachbeispiele"]
    (html
     [:html
      [:head
       [:title title]
       [:meta {:name "viewport" :content "width=device-width"}]
       [:link {:rel "stylesheet" :href "/pronouns.css"}]]
      [:body
       (header-block title)
      [:div {:class "section examples"}
       [:p [:h2 (str "Wir konnten diese Pronomen nicht in unserer Datenbank finden. "
                     "Wenn du denkst wir sollten sie haben dann gib uns bitte Bescheid!")]]]
       (footer-block)]])))

(defn pronouns [params]
  (let [path (params :*)
        alts (or (params "oder") [])
        pronouns (concat [path] (u/vec-coerce alts))
        pronoun-declensions (filter some? (map #(lookup-pronouns
                                                 (escape-html %))
                                               pronouns))]
    (if (seq pronoun-declensions)
      (format-pronoun-examples pronoun-declensions)
      (not-found))))
