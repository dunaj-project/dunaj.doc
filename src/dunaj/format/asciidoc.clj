;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns dunaj.format.asciidoc
  "Asciidoc printer. Think hiccup for asciidoc."
  {:authors ["Jozef Wagner"]}
  (:api dunaj)
  (:require 
   [dunaj.host :refer
    [Class class? set! keyword->class class-instance?]]
   [dunaj.format.helper :refer [string-to-batch! string-cat-batch!]]
   [dunaj.format.printer :refer
    [IContainerPrinterMachine -printer-to-type
     IPrinterMachineFactory printer-engine -indent
     invalid-item-handler print-single-element!
     print-element! print-batch! print-finish! prev-indent
     print-single-batch! print-batch-escaped! print!
     IIndentedMachine next-indent base-indent]]
   [dunaj.resource.host :refer [coll-reader coll-writer]]))


;;;; Implementation details

(warn-on-reflection!)

;; block/inline
;; styles, options, functions

;; (list foo bar) -> eval normal clj function

;; "string" -> renders contents as it is -> string
;; :keyword -> attribute reference -> {keyword}


;;;; Normalized syntax
;; {
;; :attributes [
;;   :positional-attribute
;;   :options [:opt1 :opt2 :opt3]
;;   [:author []]
;;   [:foo-attribute] -> unset the attribute
;;   [:foo-attribute nil] -> unset the attribute
;;   [:foo-attribute false] -> unset the attribute
;; ]
;; }

(deftype AdTopContainer
  "Top level printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    ;;(clojure.core/println "top is" coll)
    (if (map? coll) [(assoc coll ::root true)] coll))
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(deftype AdHeaderContainer
  "Header printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents]
    ;; attributes
    ;; id="d"
    ;; style="dd" ???
    ;; role=["asdasd", "asdasd"]
    ;; options=["asdasdsad","asdasdsad"]
    (let [title (->str "= " (name (:title coll)) \newline)
          authors (when-let [authors (:authors coll)]
                    (let [authors (provide-sequential authors)]
                      (->str (str (interpose "; "
                                             (map name authors)))
                             \newline)))
          version (when-let [ver (:version coll)]
                    (->str ver \newline))
          am (dissoc coll :title :authors :type :version)
          amf (fn [[k v]] (->str ":" (name k)
                                (if v ": " "!:")
                                (if v (if (named? v) (name v) v) "")
                                \newline))
          attrs (concat [title authors version] (map amf am))
          attrs (seq (remove nil? attrs))
          attrs (string-to-batch! (str attrs))]
      (print! batch bm state attrs)))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(deftype AdBlockContainer
  "Block printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents]
    ;; attributes
    ;; id="d"
    ;; style="dd" ???
    ;; role=["asdasd", "asdasd"]
    ;; options=["asdasdsad","asdasdsad"]
    ;; positional
    ;;(clojure.core/println "printing" coll)
    (let [style (when-let [style (:style coll)]
                  (name style))
          pos (when-let [pos (:pos coll)]
                (let [pos (provide-sequential pos)]
                  (str (interpose \, (map name pos)))))
          id (when-let [id (:id coll)]
               (->str "id=\"" (name id) "\""))
          title (when-let [title (:title coll)]
                  (->str "title=\"" (name title) "\""))
          role (when-let [roles (:roles coll)]
                 (let [roles (provide-sequential roles)]
                   (->str "role=\""
                          (str (interpose " " (map name roles)))
                          "\"")))
          options (when-let [options (:options coll)]
                    (let [options (provide-sequential options)]
                      (->str "options=\""
                             (str (interpose \space (map name options)))
                             "\"")))
          attrs [style pos title id role options]
          attrs (seq (remove nil? attrs))
          attrs (if (empty? attrs)
                  ""
                  (->str "[" (str (interpose \, attrs)) "]\n"))
          attrs (string-to-batch! attrs)]
      (print! batch bm state attrs \- \- \newline)))
  (-print-after! [this bm batch parents]
    (print! batch bm state \- \- \newline))
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(deftype AdSectionContainer
  "Section printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents]
    (let [levels (str (repeat (:level coll) \=))
          title (->str levels " " (:title coll) \newline)
          style (when-let [style (:style coll)]
                  (->str "[" (name style) "]\n"))
          id (when-let [id (:id coll)]
               (->str "[id=\"" (name id) "\"]\n"))
          role (when-let [roles (:roles coll)]
                 (let [roles (provide-sequential roles)]
                   (->str "[role=\""
                          (str (interpose " " (map name roles)))
                          "\"]\n")))
          attrs [style role id title]
          attrs (str (remove nil? attrs))
          attrs (string-to-batch! attrs)]
      (print! batch bm state attrs)))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(defn pop-delim!
  [state ordered?]
  (let [s @state
        ns (update s (if ordered? :ordered :unordered) pop)
        ns (update ns :delim pop)]
    (reset! state ns)
    nil))

(defn ^:private oname
  [x]
  (if (named? x) (name x) x))

(defn ^:private attr->str
  [k v]
  (let [vs (provide-sequential v)]
    (->str (oname k) "=\"" (str (interpose \space (map oname vs))) "\"")))

(defn ^:private m->str
  ([m] (m->str m nil))
  ([m ex]
     (let [ms (map attr->str (unpacked (apply dissoc m ex)))]
       (when-not (empty? ms)
         (->str "[" (str (interpose \, ms)) "]")))))

(defn ^:private print-icon
  [config state m]
  (string-to-batch!
   (->str "icon:"
          (name (:style m))
          (or (m->str m #{:style :type}) "[]"))))

(deftype AdInlineContainer
  "printer container for Asciidoc printer."
  {:predicate 'inline-container?}
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents])
  (-print-after! [this bm batch parents]
    (print! batch bm state \newline))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(defprotocol IAdPrinter
  (-print-ad!
    "Returns result or printing `this` as an asciidoc. Return value
     follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(extend-protocol! IAdPrinter
  java.lang.String
  (-print-ad! [this config state bm batch parents]
    (if (inline-container? (first parents))
      (print! batch bm state (string-to-batch! this))
      (print! batch bm state (string-to-batch! this) \newline)))
  clojure.lang.IPersistentVector
  (-print-ad! [this config state bm batch parents]
    (let [m {:type :block
             :style (first this)
             :content (next this)}]
      (-print-ad! m config state bm batch parents)))
  clojure.lang.IPersistentSet
  (-print-ad! [this config state bm batch parents]
    )
  clojure.lang.IPersistentMap
  (-print-ad! [this config state bm batch parents]
    (condp identical? (:type this)
      :block (->AdBlockContainer config state this)
      :header (->AdHeaderContainer config state this)
      :section (->AdSectionContainer config state this)
      :inline (->AdInlineContainer config state this)
      :icon (print-icon config state this)
      (->AdBlockContainer config state this)))
  clojure.lang.Keyword
  (-print-ad! [this config state bm batch parents]
    ))

(defrecord AsciidocPrinterFactory
  "Asciidoc Printer Factory record."
  []
  IPrinterMachineFactory
  (-printer-config [this]
    {})
  (-printer-from-type [this]
    (keyword->class :object))
  (-printer-to-type [this]
    (keyword->class :char))
  (-top-container [this config state coll]
    (->AdTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    ;;(clojure.core/println "dispatching" item)
    (-print-ad! item config state bm batch parents))
  IPrinterFactory
  (-print [this]
    (printer-engine this))
  (-print [this coll]
    (printer-engine this coll)))

(defn ^:private ad->html*
  [coll o]
  (let [[wr ocoll] (coll-writer)
        ad (org.asciidoctor.Asciidoctor$Factory/create)]
    (.render
     ad (coll-reader coll) ^java.io.Writer wr ^java.util.Map o)
    (.close ^java.io.Writer wr)
    ocoll))
  
(defn ^:private ad->html
  [coll o]
  (let [input (str coll)
        ad (org.asciidoctor.Asciidoctor$Factory/create)]
    (.render ad input ^java.util.Map o)))

(defn get-safe-mode
  [k]
  (condp identical? k
    :unsafe org.asciidoctor.SafeMode/UNSAFE
    :safe org.asciidoctor.SafeMode/SAFE
    :server org.asciidoctor.SafeMode/SERVER
    :secure org.asciidoctor.SafeMode/SECURE))
  
(defrecord ConverterPrinterFactory
  "Asciidoc converter Printer Factory record."
  [attributes embedded? safe-mode backend doctype base-dir opts
   fallback?]
  IPrinterFactory
  (-print [this coll]
    (let [o {"header_footer" (not embedded?)
             "backend" (name backend)
             "doctype" (name doctype)
             "safe" (get-safe-mode safe-mode)}
          o (if base-dir (assoc o "base_dir" base-dir) o)
          o (if attributes (assoc o "attributes" attributes) o)
          o (merge opts o)]
      (if fallback? (ad->html coll o) (ad->html* coll o)))))

(defn ^:private provide-vec
  [x]
  (if (vector? x) x [x]))

(defn ^:private provide-ovec
  [x]
  (if (vector? x) x [{} x]))


;;;; Public API

(defn block
  [style opts contents]
  (let [o (cond (map? opts) opts
                (nil? opts) {}
                :else {:title opts})
        contents contents]
    (merge o {:type :block
              :style style
              :content (vec contents)})))

(defn section
  [level opts title]
  (merge opts {:type :section :level level} {:title title}))

(defn h1
  ([title] (h1 nil title))
  ([opts title] (section 1 opts title)))

(defn h2
  ([title] (h2 nil title))
  ([opts title] (section 2 opts title)))

(defn h3
  ([title] (h3 nil title))
  ([opts title] (section 3 opts title)))

(defn h4
  ([title] (h4 nil title))
  ([opts title] (section 4 opts title)))

(defn h5
  ([title] (h5 nil title))
  ([opts title] (section 5 opts title)))

(defn h6
  ([title] (h6 nil title))
  ([opts title] (section 6 opts title)))

(defn tip
  [opts & contents]
  (block :TIP opts contents))

(defn note
  [opts & contents]
  (block :NOTE opts contents))

(defn important
  [opts & contents]
  (block :IMPORTANT opts contents))

(defn warning
  [opts & contents]
  (block :WARNING opts contents))

(defn caution
  [opts & contents]
  (block :CAUTION opts contents))

(defn example
  [opts & contents]
  (block :example opts contents))

(defn listing
  [opts & contents]
  (block :listing opts contents))

(defn source
  [opts & contents]
  (block :source opts contents))

(defn literal
  [opts & contents]
  (block :literal opts contents))

(defn sidebar
  [opts & contents]
  (block :sidebar opts contents))

(defn verse
  [opts & contents]
  (block :verse opts contents))

(defn quote
  [opts & contents]
  (block :quote opts contents))

(defn pass
  [opts & contents]
  (block :pass opts contents))

(defn inline
  [opts & contents]
  (merge opts {:type :inline :content contents}))

(defn icon
  [name & optmap]
  (let [m (if (single? optmap) (first optmap) (apply ->map optmap))]
    (merge m {:type :icon :style name})))

;; image
;; marked
;; include file
;; labelled
;; table
;; link
;; anchors, cross reference
;; callouts

(def hr "'''")

(def page-break "<<<")

(def asciidoc
  "Asciidoc formatter factory."
  {:added v1}
  (->AsciidocPrinterFactory))

(def convert
  (->ConverterPrinterFactory
   nil false :unsafe :html5 :article nil nil true))
