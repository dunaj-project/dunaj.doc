(defproject org.dunaj/dunaj.doc "0.1.0-SNAPSHOT"
  :description "Dunaj documentation tools."
  :url "https://github.com/dunaj-project/dunaj.doc"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* true}
  :profiles {:provided
             {:dependencies
              [[org.dunaj/dunaj "0.7.0-SNAPSHOT"]]}}
  :dependencies [[org.asciidoctor/asciidoctorj "1.5.2"]])
