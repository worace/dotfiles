;;Hotloading deps with refactor-nrepl:
;;https://gist.github.com/worace/742d73e8d27a46ad759c

{:user {:plugins [[lein-exec "0.3.4"]
                  [lein-try "0.4.3"]
	          [lein-create-template "0.1.1"]
                  [org.clojure/tools.namespace "0.2.11"]
                  [refactor-nrepl "1.1.0"]
                  [com.jakemccrary/lein-test-refresh "0.6.0" :exclusions [org.clojure/tools.namespace]]]
        :injections [(require ['clojure.repl :refer ['doc]])]
        :repl-options {
                       :init (do (println "HI")
                                 (defn hotload-dependency [dep-name version-string]
                                   (refactor-nrepl.artifacts/hotload-dependency
                                    {:coordinates (str "[" dep-name " " "\"" version-string "\"]")})))
                       }
        :dependencies [[alembic "0.3.2"]
                       [org.clojure/tools.namespace "0.2.11"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :jvm-opts ["-Djava.net.preferIPv4Stack=true"]}
 :dev {:dependencies [[debugger "0.1.7"]]}
 :repl {:plugins [[cider/cider-nrepl "0.10.1"]]
        :dependencies [[debugger "0.1.7"]]}}
