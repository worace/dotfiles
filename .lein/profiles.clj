;;Hotloading deps with refactor-nrepl:
;;https://gist.github.com/worace/742d73e8d27a46ad759c
#_(comment
  (defn hotload-dep [package-name version-str]
    (refactor-nrepl.artifacts/hotload-dependency
     {:coordinates (str "[" package-name "\"" version-str "\"" "]")}))
  (hotload-dep "selmer" "1.10.7"))


{:user {:plugins [[lein-exec "0.3.7"]
                  [lein-try "0.4.3"]
                  [lein-open "0.1.0"]
                  [cider/cider-nrepl "0.17.0"]
                  [lein-create-template "0.1.1"]
                  [org.clojure/tools.namespace "0.2.11"]
                  [com.jakemccrary/lein-test-refresh "0.21.1" :exclusions [org.clojure/tools.namespace]]]
        :dependencies [[alembic "0.3.2"]
                       [pjstadig/humane-test-output "0.8.3"]
                       [org.clojure/tools.namespace "0.2.11"]
                       ;; [org.clojure/tools.nrepl "0.2.12"]
                       [org.clojure/tools.namespace "0.2.11"]]
        :repl-options {:init (set! *print-length* 50)}
        :injections [(require '[clojure.repl :refer [doc]])
                     (require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
        :signing {:gpg-key "F7B92822"}}}
