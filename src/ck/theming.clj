(ns ck.theming
  (:require [instaparse.core :as insta]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes
            Attribute Attributes Comment DataNode
            Document Element TextNode]
           [com.googlecode.htmlcompressor.compressor HtmlCompressor]))

(defn empty-state []
  (atom {:pages {}
         :components {}
         :context {}
         :paths {}}))

(def parser
  (insta/parser
    "annotation = expr[<'%'>config] | ignore
    ignore = #'[^\\^\\[@&$#\\?].*'
    config = <#'\\{\\s*'>settings<#'\\s*\\}'>
    <settings> = setting[<#'\\s*,\\s*'>settings*]
    setting = #'[a-zA-Z][a-zA-Z0-9_\\-]+' <#'\\s*:\\s*'> settingValue
    <settingValue> = #'[^,\\]\\}\"]+' | field | action | config | settingValueArray
    settingValueArray =  <'['> settingValue[<#'\\s*,\\s*'>settingValue]*  <']'>
    expr = model | array | action | page
    array = <'['> model <']'>
    <model> = inComponent | exComponent | field
    page = <'^'>
    inComponent = <'@'>#'[a-zA-Z][a-zA-Z0-9]+'
    exComponent = <'&'>#'[a-zA-Z][a-zA-Z0-9]+'
    field = local | global
    local = <'$'>#'[a-zA-Z][a-zA-Z0-9]+'
    global = <'$$'>#'[a-zA-Z][a-zA-Z0-9]+'
    action = ['js']actiontype
    <actiontype> = <'#'>actionname | <'?'>actionname
    <actionname> = #'[a-zA-Z][\\.\\-a-zA-Z0-9]+'
    "))

;; NOTE: the 'page' annotation would be used similar to

(defmulti visit (fn [tree]
                   (first tree)))


(defmethod visit :annotation [tree]
  (if-let [config-tree (get tree 2)]
    (assoc (visit (second tree)) :config (visit config-tree))
    (visit (second tree))))

(defmethod visit :config [tree]
  (reduce merge (map #(visit %) (rest tree))))

(defmethod visit :setting [[_ key val]]
  {(keyword key) (if (string? val) val (visit val))})

(defmethod visit :settingValueArray [[_ & vals]]
  vals)

(defmethod visit :expr [[_ tree]]
  {:type (first tree)
   :data (visit tree)})

(defmethod visit :array [[_ tree]]
  {:type (first tree)
   :data (visit tree)})

(defmethod visit :field [[_ [field-type field-name]]]
  {:type field-type
   :name field-name})

(defmethod visit :inComponent [[_ component-id]]
  {:id (keyword component-id)})

(defmethod visit :exComponent [[_ component-id]]
  {:id (keyword component-id)})

(defmethod visit :action [[_ action-id]]
  {:id (keyword action-id)})

(defmethod visit :page [_] nil)

(defmethod visit :ignore [[_ val]]
  (str "<!--" val "-->"))

(defmethod visit :default [tree]
  (println (pr-str "No visit method found for " tree)))

(def test-file (io/file (io/resource "templates/test.html")))


(def state (atom {:components {}
                  :scope {}}))

(defprotocol ToComponent
  (^:private to-component [doc] "Turn an annotated HTML tree into a ck.theming component"))


(defn new-scope! []
  (swap! state assoc :scope {:parent (:scope @state)}))

(defn exit-scope! []
  (swap! state assoc :scope {:parent (:scope @state)}))

(defmulti handle-item
          (fn [irep _]
             (:type irep)))

(defmethod handle-item :inComponent [irep target]
  (let [id (get-in irep [:data :id])
        sym (symbol (str/lower-case (name id)))]
    (when (not (get-in @state [:components id]))
      (swap! state assoc-in [:components id]
             (concat '(defn)
                     [sym '[title body]
                      (to-component target)])))
    [sym '(:title fields) '(:body fields)]))

(defmethod handle-item :array [irep target]
  (concat '(for [item ])))

(defmethod handle-item :default [irep _]
  irep)

(extend-protocol ToComponent
  Document
  (to-component [doc]
    (-> doc .children first (to-component )))
  Element
  (to-component [element]
    (let [tag-name (-> element .tagName keyword)
          attrs (-> element .attributes (to-component ))]
      (into [tag-name attrs]
            (loop [children (->> element .childNodes)
                   elements []]
              (if (empty? children)
                elements
                (let [result (to-component (first children))]
                  (if (and (map? result))
                    (recur (drop 2 children)
                           (conj elements (handle-item result (second children))))
                    (recur (rest children)
                           (conj elements result)))))))
      ;(condp = tag-name
      ;  :head (into [tag-name attrs]
      ;              (->> element
      ;                   .childNodes
      ;                   (map #(-> % (to-component )))
      ;                   (remove nil?)
      ;                   vec
      ;                   not-empty))
      ;  (into [tag-name attrs]
      ;        (->> element
      ;             .childNodes
      ;             (map #(-> % (to-component )))
      ;             (remove nil?)
      ;             vec
      ;             not-empty)))
      ))
  Attributes
  (to-component [attrs]
    (into {} (map #(-> % (to-component )) attrs)))
  Attribute
  (to-component [attr]
    [(keyword (.getKey attr))
     (.getValue attr)])
  TextNode
  (to-component [text-node]
    (let [value (.text text-node)]
      (if (not (str/blank? value))
        value)))
  DataNode
  (to-component [data-node]
    (.getWholeData data-node))
  Comment
  (to-component [comment]
    (visit (parser (-> comment .getData)))))

(defn- compress [^String s]
  (let [compressor (HtmlCompressor.)]
    (.setRemoveComments compressor false)
    (.setPreserveLineBreaks compressor false)
    (.replaceAll (.compress compressor s)
                 ">\\s+<" "><")))

(defn parse
  [source]
  (let [component-name (str/replace (.getName source) #"[.][^.]+$" "")
        result (-> source
                   slurp
                   compress
                   Jsoup/parse
                   to-component)]
    (concat '(defn)
          [(symbol component-name) []
           {:component-will-mount '(fn [] (+ 2 a))
            :reagent-render      result }])))

