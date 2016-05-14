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
  {:type :comment
   :data val})

(defmethod visit :default [tree]
  (println (pr-str "No visit method found for " tree)))

(def test-file (io/file (io/resource "templates/test.html")))


(defn push-on-stack [stack value]
  (swap! stack conj value))

(defn pop-off-stack [stack]
  (let [res (peek @stack)]
    (reset! stack (pop @stack))
    res))

(defn peek-in-stack [stack] (peek @stack))

(defn handle-comment [stack irep]
  (println (:type irep)))

(defprotocol ToComponent
  (^:private to-component [doc stack] "Turn an annotated HTML tree into a ck.theming component"))

(extend-protocol ToComponent
  Document
  (to-component [doc stack]
    (-> doc .children first (to-component stack)))
  Element
  (to-component [element stack]
    (let [tag-name (-> element .tagName keyword)
          attrs (-> element .attributes (to-component stack))
          some-val (peek-in-stack stack)]
      (condp = tag-name
        :head (into [tag-name attrs]
                    (->> element
                         .childNodes
                         (map #(-> % (to-component stack)))
                         (remove nil?)
                         vec
                         not-empty))
        (into [tag-name attrs]
              (->> element
                   .childNodes
                   (map #(-> % (to-component stack)))
                   (remove nil?)
                   vec
                   not-empty)))))
  Attributes
  (to-component [attrs stack]
    (into {} (map #(-> % (to-component stack)) attrs)))
  Attribute
  (to-component [attr _]
    [(keyword (.getKey attr))
     (.getValue attr)])
  TextNode
  (to-component [text-node stack]
    (let [value (.text text-node)]
      (if (not (str/blank? value))
        value)))
  DataNode
  (to-component [data-node stack]
    (.getWholeData data-node))
  Comment
  (to-component [comment stack]
    (handle-comment stack
                    (visit (parser (-> comment .getData))))
    nil))

(defn parse
  [source]
  (let [component-name (str/replace (.getName source) #"[.][^.]+$" "")
        stack (atom [{:type :component}])
        result (-> source
                   slurp
                   ;compress
                   Jsoup/parse
                   (to-component stack))]
    (println @stack)
    (concat '(defn)
          [(symbol component-name) []
           {:component-will-mount '(fn [] (+ 2 a))
            :reagent-render      result }])))

