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

(ns dunaj.doc
  "Dunaj documentation facilities."
  {:authors ["Jozef Wagner"]}
  (:require [dunaj.core :refer [dunaj-ns]]))

(dunaj-ns
 (:require
  [dunaj.boolean :refer [xor]]
  [dunaj.string :as ds]
  [dunaj.identifier :refer [named?]]
  [dunaj.namespace :as dn :refer [publics]]
  [dunaj.state.var :refer [find-var]]
  [dunaj.coll.recipe :refer [concat*]]
  [dunaj.coll.util :refer [prewalk-replace unpacked prewalk]]
  [dunaj.macro :refer [macro?]]
  [dunaj.format.helper :refer [string-to-batch! string-cat-batch!]]
  [dunaj.format.clj :refer [pretty-clj]]
  [dunaj.resource.host :refer [coll-reader coll-writer]]
  [dunaj.format.asciidoc :refer [h2 h3 convert asciidoc pass-html]]))


;;;; Implementation details

(defn ^:private mname :- (Maybe String)
  "Returns name of `_x_`, or nil if `_x_` is `nil`."
  [x :- (Maybe INamed)]
  (if (named? x) (name x) x))

(defn ^:private protocol-var? :- Boolean
  "Returns `true` is var `_v_` holds a public protocol."
  [v :- Var]
  (boolean (and (:added (meta v)) (not (macro? v))
                (:defprotocol (meta v))
                (protocol? @v))))

(defn ^:private protocol-method-var? :- Boolean
  "Returns true is var `_v_` holds a public protocol method."
  [v :- Var]
  (boolean (:protocol (meta v))))

(defn ^:private protocols :- []
  "Returns collection recipe of public protocols in a given namespace
   `_ns-sym_`."
  [ns-sym :- Symbol]
  (->> ns-sym publics vals (filter protocol-var?)))

(defn ^:private ns-list :- []
  "Returns collection recipe of namespace symbols sorted
   alphabetically."
  ([config :- {}]
     (ns-list config false))
  ([config :- {}, spi? :- Boolean]
     (let [nl (->> (dn/all)
                   (remove (:ns-blacklist config))
                   (filter #(matches (:ns-regex config) (name %)))
                   (filter dn/ns?)
                   sort)]
       (if spi? (remove #(empty? (protocols %)) nl) nl))))

(defn api-var?
  [v]
  (let [m (meta v)]
    (and (:added m) (not (:protocol m)) (not (:defprotocol m)))))

(defn api-groups
  [ns-sym]
  (->> ns-sym
       publics
       vals
       (filter api-var?)
       (group-by #(:category (meta %)))))

(defn ns-vars?
  [ns-sym]
  (not (empty? (api-groups ns-sym))))

(defn ^:private string-indent
  [s]
  (let [cf (fn [l] (max 0 (count (take-while #(= \space %) l))))]
    (reduce min 1000 (map cf (remove empty? (rest (lines s)))))))

(defn ^:private trim-max
  [s tm]
  (let [tf #(if (blank? (take tm %)) (str (drop tm %)) %)]
    (str (interpose \newline (map tf (lines s))))))

(defn ^:private doc-string
  [s]
  (if-let [indent (string-indent s)] (trim-max s indent) s))

(defn first-para
  [s]
  (str (interpose "\n" (take-while #(not (empty? %)) (lines s)))))

(defn ns-examples
  [config ns-sym]
  (let [fname (->str (:examples-path config) "/"
                     (name ns-sym) ".edn")]
    (try (with-scope (first (parse edn (slurp fname))))
         (catch java.lang.Exception e nil))))

(defn referred?
  [config v]
  (when-let [rns (:refers-ns config)]
    (boolean (some #(identical? v %) (seq (vals (dn/refers rns)))))))

(defn var-example
  [config var]
  (when-let [ex (ns-examples config (symbol (namespace var)))]
    (provide-sequential (get ex (symbol (name var))))))

(defn ns-example
  [config ns-sym]
  (when-let [ex (ns-examples config ns-sym)]
    (provide-sequential (get ex nil))))

(def ex-ids (atom 0))

(defn show-ex
  ([exs] (show-ex exs ""))
  ([exs s]
   (when-not (empty? exs)
     (let [i (alter! ex-ids inc)
           sf #(->str "[source,clojure,linenums]\n--\n" % "\n--")]
       (->str
        "++++\n<input type=\"checkbox\" class=\"dd-example-check\""
        " id=\"toggle-" i
        "\"><label class=\"dd-example-label\" for=\"toggle-" i "\">"
        (count exs) " " s "example" (if (single? exs) "" "s")
        "</label>\n++++\n"
        (str (interpose \newline (map #(sf (doc-string %)) exs))))))))

(defn munge
  [s]
  (clojure.core/munge s))

(defn ad-munge
  [s]
  (-> s munge (ds/replace "_" "{under}")))

(defn html-munge
  [s]
  (-> s (ds/replace "&" "&amp;") (ds/replace "<" "&lt;")
      (ds/replace "\"" "&quot;") (ds/replace "'" "&apos;")))

(defn ad-escape
  [s]
  (-> s (ds/replace ">" "&#62;") (ds/replace "<" "&#60;")
      (ds/replace "=" "&#61;") (ds/replace "-" "&#45;")))

(defn ^:private strip-ns
  [form]
  (let [sm {'java.lang.Boolean 'Boolean}
        sym-map #(get sm % %)
        sf (fn [x] (if (symbol? x) (sym-map (symbol (name x))) x))]
    (prewalk sf form)))

(defn link-for
  ([config v on ns-sym]
     (link-for config v ns-sym nil))
  ([config v on ns-sym label]
     (let [var-ns-name (namespace v)
           var-name (name v)
           good? (and (or (api-var? v)
                          (protocol-var? v)
                          (protocol-method-var? v))
                      (not (= "clojure.core" var-ns-name))
                      (matches (:ns-regex config) var-ns-name))
           local? (and good? (= (name ns-sym) var-ns-name))
           alias? (not (= on var-name))
           line (or (:line (meta v))
                    (:line (meta (:protocol (meta v)))))
           l (or label
                 (->str (if var-ns-name (->str var-ns-name "/") "")
                        var-name))
           source-for
           #(let [np (ds/replace (ds/replace % \. \/) \- \_)]
              (->str (:sources-url config) np ".clj"))]
       (cond
        (and local? alias?) ;; local reference
        (->str "<<" (ad-munge var-name) "," (or label var-name) ">>")
        (and good? line) ;; link to source
        (->str (source-for var-ns-name) "#L" line "[" l "]")
        :else l))))

(defn pretty-gensym
  [sym]
  (if (red? sym)
    sym
    (if-let [i (ds/index-of (name sym) "__" )]
      (symbol (slice (name sym) 0 i))
      sym)))

(defn type-extenders
  [config v]
  (when (and (not (macro? v)) (or (type? @v) (record? @v)))
    (let [ff #(dunaj.poly/extends? % @v)
          ns-protocols #(map dunaj.state/deref (protocols %))
          protocols (mapcat ns-protocols (ns-list config))
          x (sort-by name (map :var (filter ff protocols)))]
      (if (record? @v)
        (remove #{#'dunaj.compare/IHash #'dunaj.compare/IEquiv
                  #'dunaj.coll/IAssociative #'dunaj.coll/ICounted,
                  #'dunaj.coll/IEmptyable #'dunaj.feature/IMeta,
                  #'dunaj.coll/IPersistentCollection
                  #'dunaj.coll/IPersistentMap #'dunaj.coll/ISeqable,
                  #'dunaj.feature/IPersistentMeta} x)
        (remove #{#'dunaj.compare/IHash #'dunaj.compare/IEquiv} x)))))

(defn print-type-extenders
  [config v]
  (let [prp #(->str "`<<" (namespace %) ".spi.html#"
                    (ad-munge (name %)) "," (name %) ">>`")]
    (when-let [x (seq (type-extenders config v))]
      (apply ->str "icon:plug[title=\"Extends\"] Extends: "
             (interpose ", " (map prp x))))))

(defn protocol-methods
  [ns-sym v]
  (map #(dn/resolve ns-sym (symbol (name %)))
       (clojure.core/keys (:sigs @v))))

(defn sym-api?
  [ns-sym sym]
  (let [qs (if (namespace sym) sym (symbol (name ns-sym) (name sym)))
        v (find-var qs)]
    (not (or (protocol-var? v) (protocol-method-var? v)))))

(defn var-doc
  "Returns asciidoc code for var documentation."
  [config spi? ns-sym v]
  (let [ppp #(str (print pretty-clj %))
        pps #(str (print-one pretty-clj %))
        ap #(->str \` (pps %) \`)
        apm #(if (single? %)
               (->str " `(pass:[" (ppp %) "])`")
               (->str " `(pass:[" (pps (first %)) "] _"
                      (ppp (map pretty-gensym (rest %))) "_)`"))
        apa #(if (single? %)
               (->str "* `(pass:[" (ppp %) "])`")
               (->str "* `(pass:[" (pps (first %)) "] _"
                      (ppp (map pretty-gensym (rest %))) "_)`"))
        apfx #(ds/replace (pps %) "]" "&#93;")
        apf #(if (single? %)
               (->str "* `() → pass:["
                      (ds/replace (pps (first %)) "]" "&#93;") "]`")
               (->str "* `(pass:["
                      (str (interpose " ⨯ " (map apfx (rest %))))
                      "]) → pass:["
                      (ds/replace (pps (first %)) "]" "&#93;") "]`"))
        apu #(->str "`<<" (ad-munge (mname %)) "," (mname %) ">>`")
        nv (name v)
        m (meta v)
        m? (macro? v)
        t? (when-not m? (type? @v))
        f? (when-not m? (and (fn? @v) (:arglists m)))
        v? (when-not m? (not f?))
        dv? (and v? (or (= clojure.lang.Var (:tsig m))
                        (= clojure.lang.Var (:on-class (:tsig m)))))
        r? (when-not m? (record? @v))
        p? (when-not m? (protocol-var? v))
        tr? (when-not m? (:transducer m))
        al (cond t? [(or (:fields m) [])]
                 r? [(or (:fields m) [])]
                 :else (map #(cons (symbol nv) %) (:arglists m)))
        pm (when p? (protocol-methods ns-sym v))
        see (concat (provide-sequential
                     (when-let [n (mname (:protocol m))] (symbol n)))
                    (provide-sequential (:predicate m))
                    (:see m))
        sf (fn [sym]
             (let [sapi? (sym-api? ns-sym sym)]
               (cond (namespace sym)
                     (->str "`<<" (namespace sym)
                            (if sapi? ".api.ad#" ".spi.ad#")
                            (ad-munge (name sym))
                            "," (ad-escape (namespace sym))
                            "/" (ad-escape (name sym)) ">>`")
                     (xor sapi? (not spi?))
                     (->str "`<<" (name ns-sym)
                            (if sapi? ".api.ad#" ".spi.ad#")
                            (ad-munge (name sym))
                            "," (ad-escape (name sym)) ">>`")
                     :else (->str "`<<" (ad-munge (name sym))
                                  "," (ad-escape (name sym)) ">>`"))))
        ex (show-ex (var-example config v))]
    [(h3 {:roles [:dd-var] :id (munge (name v))}
         (html-munge (name v)))
     (->str
      "[.dd-added]\nAvailable since version "
      (or (:added m) (:added (meta (:protocol m))))
      (if-let [alias (:alias m)]
        (let [av (dn/resolve ns-sym alias)
              alias (symbol (namespace av) (name av))
              an (if (namespace alias)
                   (->str (namespace alias) "/" (name alias))
                   (name alias))
              an (when (= (namespace alias) (name nv)) (name an))]
          (->str " (alias of " (link-for config av nv ns-sym an) ")"))
        (->str " (" (link-for config v nv ns-sym "view source") ")")))
     (when (false? (referred? config v))
       (->str "`icon:unlink[title=\"Not referred\"]"
              " not referred automatically`"))
     (cond
      m? (apply ->str "`icon:magic[title=\"Macro!\"] MACRO` "
                (interpose "&#160; " (map apm al)))
      t? "`icon:archive[title=\"Type\"] TYPE` "
      p? (apply ->str "`icon:plug[title=\"Protocol\"]"
                (cond (empty? pm) " MARKER PROTOCOL`"
                      (single? pm) " PROTOCOL` with method "
                      :else " PROTOCOL` with methods ")
                (when p? (interpose ", " (map apu pm))))
      r? (apply ->str "`icon:th-list[title=\"Record\"] RECORD` "
                (interpose "&#160; " (map ap al)))
      dv? (->str "`icon:inbox[title=\"Var\"] VAR` of type "
                 (ap (strip-ns (:qtsig m))))
      v? (->str "`icon:cube[title=\"Var\"] VAR` of type "
                (ap (strip-ns (:qtsig m))))
      tr? (apply ->str
                 "`icon:puzzle-piece[title=\"Transducer\"]"
                 " RETURNS TRANSDUCER`\n"
                 "[.dd-usage]\nUsage: ::\n"
                 (ad-escape (interpose "\n" (map apa al))))
      :else (apply ->str "[.dd-usage]\nUsage: ::\n"
                   (ad-escape (interpose "\n" (map apa al)))))
     (when (and f? (:qtsig m))
       (let [ts (strip-ns (:qtsig m))]
         (->str "[.dd-usage]\n"
                (if (and (red? ts) (= 'Fn (first ts)))
                  (->str (if (single? (rest ts))
                           "Type signature: ::\n"
                           "Type signatures: ::\n")
                         (str (interpose "\n" (map apf (rest ts)))))
                  (->str "Type signature: `" (ppp ts) "`")))))
     (when (or r? v?) (print-type-extenders config v))
     (when (and p? (:forbid-extensions (meta v)))
       (->str "`icon:close[title=\"Extensions forbidden\"]"
              " extensions of this protocol on existing types are"
              " *forbidden* (performance and/or backwards"
              " compatibility reasons).`"))
     (->str "[.docstring]\n" (doc-string (:doc m)))
     (when-not (empty? ex) (str ex))
     (when-not (empty? see)
       (->str "[.see]\nicon:share[title=\"See also\"]"
              " See also: " (str (interpose ", " (map sf see)))))]))

;;; Nav

(defn nav-ns-var
  "Returns nav code for one var"
  [config v]
  (let [m (meta v)
        m? (macro? v)
        t? (when-not m? (type? @v))
        f? (when-not m? (and (fn? @v) (:arglists m)))
        v? (when-not m? (not f?))
        dv? (and v? (or (= clojure.lang.Var (:tsig m))
                        (= clojure.lang.Var (:on-class (:tsig m)))))
        r? (when-not m? (record? @v))
        p? (when-not m? (protocol-var? v))
        tr? (when-not m? (:transducer m))
        c (conj [:fa :fa-fw]
                (cond m? :fa-magic
                      t? :fa-archive
                      p? :fa-plug
                      tr? :fa-puzzle-piece
                      r? :fa-th-list
                      dv? :fa-inbox
                      v? :fa-cube
                      :else :fa-chevron-right)
                (if (or m? t? p? r? v? tr?) :dd-mmsh :dd-vn))
        cr (when (referred? config v) [:dd-referred])]
    [:div :dd-nav-ns-var
     [:a {:href (->str "#" (munge (name v)))}
      [:i {:class (vec (concat cr c))}]
      [:span {:class (conj cr :dd-nav-ns-var-text)}
       (html-munge (name v))]]]))

(defn group-ns-nav
  "Returns nav code for one var group"
  [config group-name groups-map]
  (let [vars (sort-by name (get groups-map group-name nil))
        x (map #(nav-ns-var config %) vars)]
    (if (= "Primary" group-name)
      x
      (cons [:div :dd-nav-ns-group
             [:a {:href (->str "#" (munge group-name))}
              group-name]]
            x))))

(defn missing-nav
  "Returns nav code for ns vars wihtout category"
  [config group-names groups-map]
  (let [vars (concat* (vals (apply dissoc groups-map group-names)))
        x (when-not (empty? vars)
            (map #(nav-ns-var config %) (sort-by name vars)))]
    (cond
     (empty? x) nil
     ;; do not put "Other" label when no categories are defined
     (empty? group-names) x
     :else
     (cons [:div :dd-nav-ns-group [:a {:href "#Other"} "Other"]] x))))

(defn ns-nav-api
  "Returns nav code for api"
  [config ns-sym]
  (let [group-names (map #(first (provide-sequential %))
                         (:categories (dn/meta ns-sym)))
        groups-map (api-groups ns-sym)]
    (vec (concat
          [:div :dd-over
           [:div :dd-nav-ns-header
            [:a {:href "#dd-top"} (mname ns-sym)]]]
          (mapcat #(group-ns-nav config % groups-map) group-names)
          (missing-nav config group-names groups-map)
          [[:br]]))))

(defn ns-nav-proto
  "Returns nav code for one protocol"
  [config v]
  (let [cr (when (referred? config v) [:dd-referred])
        pmf (fn [m]
              [:div :dd-nav-ns-var
               [:a {:href (->str "#" (munge m))}
                [:i {:class [:fa :fa-fw :fa-chevron-right :dd-vn]}]
                [:span {:class [:dd-nav-ns-var-text]}
                 (html-munge m)]]])
        pn [:div {:class [:dd-nav-ns-var :dd-nav-ns-proto]}
            [:a {:href (->str "#" (munge (mname (:var @v))))}
             [:i {:class
                  (vec (concat cr [:fa :fa-fw :fa-plug :dd-mmsh]))}]
             [:span {:class (conj cr :dd-nav-ns-var-text)}
              (html-munge (mname (:var @v)))]]]]
    (cons pn (map pmf (map name (clojure.core/keys (:sigs @v)))))))

(defn ns-nav-spi
  "Returns nav code for ns spi"
  [config ns-sym]
  (let [pl (sort-by #(mname (:var (deref %))) (protocols ns-sym))]
    (vec (concat [:div :dd-over
                  [:div :dd-nav-ns-header
                   [:a {:href "#dd-top"} (mname ns-sym)]]]
                 (mapcat #(ns-nav-proto config %) pl)
                 [[:br]]))))

(defn ^:private side-ns-list :- []
  "Returns collection recipe of namespace symbols sorted
   alphabetically with added fake namespaces where appropriate."
  [ns-list :- []]
  (let [lf (fn [[before cur]]
              (let [nc (name cur)
                    i (ds/last-index-of nc \.)
                    prefix (when i (slice nc 0 i))
                    cp (count prefix)]
                (if (or (nil? prefix)
                        (and (not (nil? before))
                             (= prefix (slice (mname before) 0 cp))))
                  [cur]
                  [(symbol prefix) cur])))]
    (->> ns-list sort (cons nil) (partition 2 1) (mapcat lf))))

(defn ^:private side-ns-item :- Any
  "Returns code for one side ns item."
  [ns-sym :- Symbol, cur-ns :- Symbol, spi? :- Boolean]
  (let [n (name ns-sym)
        nh (->str n (if spi? ".spi.html"  ".api.html"))
        indent (count (filter #(= \. %) n))
        ln (slice n (or (ds/last-index-of n \.) 0))
        aclass [:dd-nav-doc-ns (->str "dd-nav-doc-item-" indent)]
        aclass (if (= ns-sym cur-ns)
                 (cons :dd-nav-doc-item-cur aclass)
                 aclass)]
    [:div {:class aclass}
     (cond (= 'clojure.core ns-sym)
           [:a {:href "https://clojure.github.io/clojure/"}
            [:i "official docs"]]
           (dn/ns? ns-sym) [:a {:href nh} ln]
           :else ln)]))

(defn ^:private side-ns-nav :- Any
  "Returns html code for doc navigation."
  [config :- {}, spi? :- Boolean, ns-sym :- Symbol]
  (let [ff #(or (ns-vars? %) (ds/index-of (name %) ".core"))
        nl (ns-list config spi?)
        nl (if spi? nl (filter ff nl))
        nl (side-ns-list nl)
        x [[:div :dd-over
            [:div :dd-nav-doc-header
             [:a {:href (if spi? "spi.html" "api.html")}
              (if spi? "SPI" "API")]]]
           (map #(side-ns-item % ns-sym spi?) nl)
           [[:br]]]]
    (vec (concat* x))))

(defn ^:private static-nav :- Any
  "Returns code for static side nav"
  [config :- {}, ml :- [], aname :- String]
  (let [mf (fn [m]
             (let [aclass [:dd-nav-doc-ns :dd-nav-doc-item-0]
                   aclass (if (= aname (:name m))
                            (cons :dd-nav-doc-item-cur aclass)
                            aclass)]
               [:div {:class aclass}
                [:a {:href (:url m)} (:name m)]]))
        x [[:div :dd-over
            [:div :dd-nav-doc-header
             [:a {:href (:url (first ml))} (:name (first ml))]]]
           (map mf (rest ml)) [[:br]]]]
    (vec (mapcat identity x))))

;;; Headers

(def default-header
  {:type :header
   :under "_"
   :equal "="
   :title "placeholder"
   :icons :font
   :linkcss true
   :stylesheet "dd.css"
   :coderay-linenums-mode "table"
   :coderay-css "class"
   :source-highlighter "coderay"})

(defn page-header
  "Top page header containing logo and main menu"
  [config cur]
  (let [{:keys [proj-url proj-name logo-url header-menu]} config
        print-item
        (fn [{:keys [url name icon]}]
          (let [c (if (= name cur) [:dd-mmlink :dd-mmcur] :dd-mmlink)]
            [:a {:class c :href url}
             [:i {:class [:fa icon :dd-mmsh]}] " "
             [:span name]]))]
    [:header {:id :dd-header}
     [:div 'dd-logo
      [:a {:href proj-url} 
       [:img {:src logo-url}] proj-name]]
     (apply ->vec :div 'dd-mmenu (map print-item header-menu))]))

(defn content-header
  "Page content header, containing title and copyright."
  ([config n] (content-header config false n))
  ([config spi? ns-sym]
     (let [n (if (string? ns-sym) ns-sym (name ns-sym))
           ns-sym (when-not (string? ns-sym) ns-sym)
           cv (:current-version config)
           ns-meta (when ns-sym (dn/meta ns-sym))
           pa #(->str "<span class=\"author\">" % "</span><br>")]
       [:div 'header
        [:h1 'dd-top n (when ns-sym (if spi? " SPI" " API"))]
        [:div :details
         "Copyright © " (:copy-years config) " &nbsp;"
         (str (map pa (or (:authors ns-meta) (:authors config))))
         (when-not (empty? cv)
           [:span 'revdate (->str "version " cv)])
         (when (and ns-sym (not (empty? (protocols ns-sym))))
           [:div 'dd-switch
            [:i {:class [:fa :fa-fw :fa-info-circle]}]
            " See "
            [:a {:href (->str n (if spi? ".api" ".spi") ".html")}
             (->str (if spi? "API" "SPI") " documentation")]
            " for this namespace."])]
        (when (:additional-copyright ns-meta)
          [:div :details
           "Additional copyright for parts of documentation and/or underlying implementation: Copyright © "
           (if (true? (:additional-copyright ns-meta))
             (:additional-copyright config)
             (:additional-copyright ns-meta)) "."])])))

;;; Content

(defn proto-doc
  "Returns asciidoc code for protocol contents for one ns"
  [config ns-sym v]
  (->> (protocol-methods ns-sym v)
       (cons v)
       (mapcat #(var-doc config true ns-sym %))))

(defn group-doc
  "Returns asciidoc code for content of one var group"
  [config ns-sym group-pair groups-map]
  (let [[group-name group-docstring] (provide-sequential group-pair)
        vars (sort-by name (get groups-map group-name nil))
        vlf #(->str "`<<" (ad-munge (name %)) ","
                    (ad-escape (name %)) ">>`")]
    (apply ->vec
           (h2 {:id (munge group-name)} (html-munge group-name))
           group-docstring
           (->str "In this section: "
                  (str (interpose " " (map vlf vars))))
           (mapcat #(var-doc config false ns-sym %) vars))))

(defn missing
  "Returns asciidoc code for content of uncategorized vars"
  [config ns-sym group-names groups-map]
  (let [miss (apply dissoc groups-map group-names)
        vars (sort-by name (mapcat identity (vals miss)))
        x (when-not (empty? vars)
            (mapcat #(var-doc config false ns-sym %) vars))
        vlf #(->str "`<<" (ad-munge (name %)) ","
                         (ad-escape (name %)) ">>`")]
    (cond (empty? x) nil
          (empty? group-names) x
          :else (apply ->vec
                       (h2 {:id "Other"} "Other")
                       (->str "In this section: "
                              (str (interpose " " (map vlf vars))))
                       x))))

(defn list-refers
  "Returns asiidoc code listing all refers"
  [config]
  (let [ns-sym (:refers-ns config)
        rf #(->str "`<<" (namespace %)
                   (if (sym-api? ns-sym %) ".api.ad#" ".spi.ad#")
                   (ad-munge (name %)) "," (ad-escape (name %)) ">>`")
        x (->> (dn/refers ns-sym)
               (map (comp symbol second))
               (sort-by name)
               (remove #(= "clojure.core" (namespace %))))]
    [(h2
      (->str "List of automatically referred vars (" (count x) ")"))
     (str (interpose " " (map rf x)))]))

(defn content
  "Returns code for ns content"
  [config spi? ns-sym]
  (let [group-pairs (:categories (dn/meta ns-sym))
        group-names (map #(first (provide-sequential %)) group-pairs)
        groups-map (api-groups ns-sym)
        ps (sort-by #(mname (:var (deref %))) (protocols ns-sym))
        ds (doc-string (:doc (dn/meta ns-sym)))
        x (concat
           [(assoc default-header
              :title (->str (name ns-sym) (if spi? " SPI" " API"))
              :authors (:authors (dn/meta ns-sym))
              :version (:current-version config)
              :sectlinks true)
            (if spi? (first-para ds) ds)
            (when-not spi? 
              (show-ex (ns-example config ns-sym) "usage "))]
           (if spi?
             (mapcat #(proto-doc config ns-sym %) ps)
             (concat
              (mapcat #(group-doc config ns-sym % groups-map)
                      group-pairs)
              (missing config ns-sym group-names groups-map))))
        x (if (= ns-sym (:core-ns config))
            (concat x (list-refers config))
            x)
        ad (str (print asciidoc (keep identity x)))]
    #_(with-scope (spit! "temp.ad" ad [:create :truncate]))
    (str (print (assoc convert :embedded? true) ad))))

(defn api-list-content
  "Returns rendered content for api/spi list page."
  [config spi?]
  (let [ff #(or (ns-vars? %) (ds/index-of (name %) ".core"))
        nl (sort-by name (ns-list config spi?))
        nl (remove #(= 'clojure.core %) nl)
        nl (if spi? nl (filter ff nl))
        as (if spi? " SPI" " API")
        lf #(->str "`<<" (name %) (if spi? ".spi" ".api")
                   ".ad#," (name %) ">>` "
                   (first-para (doc-string (:doc (dn/meta %)))))
        x (concat [(assoc default-header
                     :title (->str (:proj-name config) as)
                     :sectlinks true)]
                  [(->str (:proj-name config) as
                          " comprises following namespaces:\n\n")]
                  (interpose "\n\n" (map lf nl)))
        ad (str (print asciidoc (vec x)))]
    #_(with-scope (spit! "temp.ad" ad [:create :truncate]))
    (str (print (assoc convert :embedded? true) ad))))

;;; Generators

(defn gen-static
  "Generates static page."
  ([config]
   (dored [x (:static-pages config)]
     (println! "Generating" (:filename x) "static page")
     (gen-static config x)))
  ([config sc]
   (let [title (if (:no-doc-title config)
                 (:name sc)
                 (->str (:proj-name config) " " (:name sc)))
         out-header (assoc default-header :title title)
           inner-header (assoc default-header
                          :no-header-footer true
                          :noheader true
                          :current-version (:current-version config))
           fp (->str (:static-path config) "/" (:filename sc) ".ad")
           ad-inner (->str (str (print asciidoc [inner-header])) "\n"
                           (with-scope (str (slurp fp))))
           html-inner (print (assoc convert :embedded? true) ad-inner)
           out-content
           (pass-html
            (page-header config (:head sc))
            [:div 'dd-flex
             (when (:menu sc)
               [:nav 'dd-side-nav
                (static-nav
                 config (get config (:menu sc)) (:section sc))])
             [:section 'dd-content
              [:div {:class :dd-over :tabindex -1 :id 'dd-focus}
               ;; original asciidoc header is hidden with css
               (when (:name sc)
                 (content-header
                  config 
                  (if (:no-doc-title config)
                    (:name sc)
                    (->str (:proj-name config) " " (:name sc)))
                  ))
               (str html-inner)
               (when (and (:disqus config) (:disqus-id sc))
                 [:div 'disqus_thread])
               (when (and (:disqus config) (:disqus-id sc))
                 [:script
                  (->str
                   "var disqus_shortname = '" (:disqus config)
                   "'; var disqus_identifier = '" (:disqus-id sc)
                   "'; (function() {
                    var dsq = document.createElement('script');
                    dsq.type = 'text/javascript'; dsq.async = true;
                    dsq.src = '//' + disqus_shortname +
                              '.disqus.com/embed.js';
                    (document.getElementsByTagName('head')[0] ||
                     document.getElementsByTagName('body')[0]).appendChild(dsq);})();")])]]]
            [:script "window.onload = function() {document.getElementById('dd-focus').focus();}"])
           ad-out (print asciidoc out-header out-content)
           html-out (print convert ad-out)
           path (->str (:target-path config) "/" (:filename sc))
           ad-path (->str path ".ad")
           html-path (->str path ".html")
           ;; hack to require newer font awesome
           html-out (ds/replace html-out "/4.1.0/" "/4.2.0/")]
       (with-scope
         #_(spit! ad-path ad-out [:create :truncate])
         (spit! html-path html-out [:create :truncate])))))

(defn gen-api-list
  "Generates API/SPI listing page"
  [config spi?]
  (let [as (if spi? " SPI" " API")
        title (->str (:proj-name config) as)
        out-header (assoc default-header :title title)
        out-content
        (pass-html
         (page-header config (:api-header-item config))
         [:div 'dd-flex
          [:nav 'dd-side-nav
           (static-nav
            config (:doc-menu config) (if spi? "SPI" "API"))]
          [:section 'dd-content
           [:div {:class :dd-over :tabindex -1 :id 'dd-focus}
            ;; original asciidoc header is hidden with css
            (content-header
             config spi? (->str (:proj-name config) as))
            (api-list-content config spi?)]]]
         [:script "window.onload = function() {document.getElementById('dd-focus').focus();}"])
        ad-out (print asciidoc out-header out-content)
        html-out (print convert ad-out)
        path (->str (:target-path config) "/" (if spi? "spi" "api"))
        ad-path (->str path ".ad")
        html-path (->str path ".html")
        ;; hack to require newer font awesome
        html-out (ds/replace html-out "/4.1.0/" "/4.2.0/")]
    (with-scope
      #_(spit! ad-path ad-out [:create :truncate])
      (spit! html-path html-out [:create :truncate]))))

(defn gen-api
  "Generates API/SPI page for one namespace."
  [config spi? ns-sym]
  (let [ns-name (name ns-sym)
        title (->str ns-name (if spi? " SPI" " API"))
        out-header (assoc default-header
                     :title title
                     :authors (:authors (dn/meta ns-sym))
                     :version (:current-version config))
        out-content
        (pass-html
         (page-header config (:api-header-item config))
         [:div 'dd-flex
          [:nav 'dd-side-nav (side-ns-nav config spi? ns-sym)]
          (when (ns-vars? ns-sym)
            [:nav 'dd-ns-nav
             ((if spi? ns-nav-spi ns-nav-api) config ns-sym)])
          [:section 'dd-content
           [:div {:class :dd-over :tabindex -1 :id 'dd-focus}
            ;; original asciidoc header is hidden with css
            (content-header config spi? ns-sym)
            (content config spi? ns-sym)]]]
         [:script "window.onload = function() {document.getElementById('dd-focus').focus();}"])
        ad-out (print asciidoc out-header out-content)
        html-out (print convert ad-out)
        path (->str (:target-path config) "/" ns-name
                    (if spi? ".spi" ".api"))
        ad-path (->str path ".ad")
        html-path (->str path ".html")
        ;; hack to require newer font awesome
        html-out (ds/replace html-out "/4.1.0/" "/4.2.0/")]
    (with-scope
      #_(spit! ad-path ad-out [:create :truncate])
      (spit! html-path html-out [:create :truncate]))))


;;;; Public API

(defn gen-doc
  "Generates documentation based on `_config_` map."
  ([config]
   (gen-static config)
   (gen-doc config false)
   (gen-doc config true))
  ([config spi?] ;; can supply ns-sym instead of 'spi?'
   (if (boolean? spi?)
     (time
      (let [as (if spi? "SPI" "API")]
        (println! "Generating" as "list")
        (gen-api-list config spi?)
        (dored [ns-sym (ns-list config spi?)]
          (println! "Generating" as "docs for:" (name ns-sym))
          (gen-api config spi? ns-sym))))
     (do (gen-api config true spi?)
         (when-not (empty? (protocols spi?))
           (gen-api config false spi?)))))
  ([config spi? ns-sym]
   (gen-api config spi? ns-sym)))
