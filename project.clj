(defproject org.dunaj/dunaj.doc-lite "0.1.0-SNAPSHOT"
  :description "Dunaj documentation tools."
  :url "https://github.com/dunaj-project/dunaj.doc"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.dunaj/dunaj-lite "0.3.8-SNAPSHOT"]
                 [org.asciidoctor/asciidoctorj "1.5.2"]])
