;; Copyright (C) 2015, Jozef Wagner. All rights reserved.

;;; Configuration

(def version "0.7.0")

;;; Boot script

(set-env!
 :resource-paths #{"src"}
 :dependencies '[[org.asciidoctor/asciidoctorj "1.5.2"]])

(task-options!
 pom {:project 'org.dunaj/dunaj.doc
      :version version
      :description "Dunaj documentation tools"
      :url "https://github.com/dunaj-project/dunaj.doc"
      :scm {:url "https://github.com/dunaj-project/dunaj.doc"}
      :license {"Eclipse Public License - v 1.0"
                "http://www.eclipse.org/legal/epl-v10.html"}}
 push {:gpg-sign true
       :gpg-user-id "Jozef Wagner (Dunaj Project) <wagjo@wagjo.com>"
       :gpg-keyring "/home/wagjo/.gnupg/secring.gpg"
       :repo "clojars-upload"})

(ns-unmap *ns* 'install)

(deftask build
  "Dunaj doc build"
  []
  (comp (pom) (jar)))

(deftask install
  "Dunaj doc install"
  []
  (comp (build) (boot.task.built-in/install)))

(deftask clojars
  "Dunaj doc clojars"
  []
  (print "Enter Clojars password: ")
  (set-env!
   :repositories
   #(conj % ["clojars-upload"
             {:url "https://clojars.org/repo"
              :username "wagjo"
              :password (apply str (.readPassword (System/console)))}]))
  (comp (build) (push)))
