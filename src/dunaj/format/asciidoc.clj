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
  "Asciidoc printer. Rudimentary features only."
  {:authors ["Jozef Wagner"]}
  (:api dunaj :exclude [print! section quote])
  (:require [dunaj.host :refer [keyword->class]]
            [dunaj.identifier :refer [named?]]
            [dunaj.format :refer [IPrinterFactory]]
            [dunaj.format.helper :refer [string-to-batch!]]
            [dunaj.format.printer :refer
             [IContainerPrinterMachine IPrinterMachineFactory
              printer-engine print!]]
            [dunaj.resource.host :refer [coll-reader coll-writer]]))


;;;; Implementation details

(defn ^:private mname :- (Maybe String)
  "Returns name of `_x_`, or nil if `_x_` is `nil`."
  [x :- (Maybe INamed)]
  (if (named? x) (name x) x))

(defn ^:private attr->str
  [k v]
  (when v
    (let [v (provide-sequential v)
          vs (interpose \space (map mname v))]
      (->str (mname k) "=\"" (str vs) "\""))))

(defn ^:private m->str
  ([m] (m->str m nil))
  ([m ex]
     (let [ms (map attr->str (unpacked (apply dissoc m ex)))]
       (if (empty? ms)
         "[]"
         (->str "[" (str (interpose \, ms)) "]")))))

(deftype AdTopContainer
  "Top level printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    ;;(println! "top is" coll)
    (if (map? coll) [(assoc coll ::root true)] coll))
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(defn ^:private print-icon
  [config state m]
  (string-to-batch!
   (->str "icon:" (name (:style m)) (m->str m #{:style :type}))))

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

(deftype AdHeaderContainer
  "Header printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents]
    (let [title (->str "= " (name (:title coll)) \newline)
          authors (when-let [authors (:authors coll)]
                    (let [authors (provide-sequential authors)]
                      (->str (str (interpose "; " (map name authors)))
                             \newline)))
          version (when-let [v (:version coll)] (->str v \newline))
          am (dissoc coll :title :authors :type :version)
          amf (fn [[k v]] (->str ":" (name k) (if v ": " "!:")
                                (if v (mname v) "") \newline))
          attrs (concat [title authors version] (map amf am))
          attrs (string-to-batch! (str (remove nil? attrs)))]
      (print! batch bm state attrs)))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

(deftype AdBlockContainer
  "Block printer container for Asciidoc printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] (provide-sequential (:content coll)))
  (-print-before! [this bm batch parents]
    (let [style (when-let [style (:style coll)] (name style))
          pos (when-let [pos (:pos coll)]
                (let [pos (provide-sequential pos)]
                  (str (interpose \, (map name pos)))))
          title (attr->str :title (:title coll))
          id (attr->str :id (:id coll))
          role (attr->str :role (:roles coll))
          os (attr->str :options (:options coll))
          attrs (seq (remove nil? [style pos title id role os]))
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
          style (when-let [style (:style coll)] (name style))
          role (attr->str :role (:roles coll))
          id (attr->str :id (:id coll))
          attrs (seq (remove nil? [style role id]))
          attrs (if (empty? attrs)
                  ""
                  (->str "[" (str (interpose \, attrs)) "]\n"))
          attrs (string-to-batch! (->str attrs title))]
      (print! batch bm state attrs)))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (print! batch bm state \newline)))

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
    (let [m {:type :block, :style (first this), :content (next this)}]
      (-print-ad! m config state bm batch parents)))
  clojure.lang.IPersistentMap
  (-print-ad! [this config state bm batch parents]
    (condp identical? (:type this)
      :block (->AdBlockContainer config state this)
      :header (->AdHeaderContainer config state this)
      :section (->AdSectionContainer config state this)
      :inline (->AdInlineContainer config state this)
      :icon (print-icon config state this)
      (->AdBlockContainer config state this))))

(defrecord AsciidocPrinterFactory
  "Asciidoc Printer Factory record."
  []
  IPrinterMachineFactory
  (-printer-config [this] {})
  (-printer-from-type [this] (keyword->class :object))
  (-printer-to-type [this] (keyword->class :char))
  (-top-container [this config state coll]
    (->AdTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    ;;(println! "dispatching" item)
    (-print-ad! item config state bm batch parents))
  IPrinterFactory
  (-print [this] (printer-engine this))
  (-print [this coll] (printer-engine this coll)))

;;; Converter

(defn ^:private ad->html*
  [coll o]
  (let [[wr ocoll] (coll-writer)
        cr (coll-reader coll)
        ad (org.asciidoctor.Asciidoctor$Factory/create)]
    (.render ad cr ^java.io.Writer wr ^java.util.Map o)
    (.close ^java.io.Writer wr)
    ocoll))

(defn ^:private ad->html
  [coll o]
  (let [ad (org.asciidoctor.Asciidoctor$Factory/create)]
    (.render ad (str coll) ^java.util.Map o)))

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

(def hr "'''")

(def page-break "<<<")

(defn pass-html
  [& xs]
  (->str "++++\n" (str (print html xs)) "\n++++"))

(def asciidoc
  "Asciidoc formatter factory."
  (->AsciidocPrinterFactory))

(def convert
  (->ConverterPrinterFactory
   nil false :unsafe :html5 :article nil nil true))
