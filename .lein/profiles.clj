;;Hotloading deps with refactor-nrepl:
;;https://gist.github.com/worace/742d73e8d27a46ad759c
{:user {:plugins [[lein-exec "0.3.4"]
	          [lein-create-template "0.1.1"]
                  [cider/cider-nrepl "0.9.1"]
                  [org.clojure/tools.namespace "0.2.11"]
                  [refactor-nrepl "1.1.0"]
                  [com.jakemccrary/lein-test-refresh "0.6.0" :exclusions [org.clojure/tools.namespace]]]
        :dependencies [[alembic "0.3.2"]
                       [org.clojure/tools.namespace "0.2.11"]
                       [org.clojure/tools.nrepl "0.2.7"]]
        :jvm-opts ["-Djava.net.preferIPv4Stack=true"]}
 }
