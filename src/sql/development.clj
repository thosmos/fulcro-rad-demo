(ns development
  (:require
    [clojure.java.jdbc :as jdbc]
    [clojure.pprint :refer [pprint]]
    [clojure.repl :refer [doc source]]
    [clojure.tools.namespace.repl :as tools-ns :refer [disable-reload! refresh clear set-refresh-dirs]]
    [com.example.components.middleware]
    [com.example.components.server]
    [com.example.components.connection-pools]
    [com.example.components.database-queries :as queries]
    [com.example.model.account :as account]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.database-adapters.sql :as sql]
    [com.fulcrologic.rad.ids :refer [new-uuid]]
    [com.fulcrologic.rad.resolvers :as res]
    [mount.core :as mount]
    [taoensso.timbre :as log]))

(set-refresh-dirs "src/main" "src/sql" "src/dev" "src/shared")

(def seed
  (let [u (fn [n] (new-uuid n))]
    [{:id       (u 1)
      :name     "Joe Blow"
      :email    "joe@example.com"
      :active   true
      :password (attr/encrypt "letmein" "some-salt"
                  (::attr/encrypt-iterations
                    (attr/key->attribute ::account/password)))}
     {:id       (u 2)
      :name     "Sam Hill"
      :email    "sam@example.com"
      :active   false
      :password (attr/encrypt "letmein" "some-salt"
                  (::attr/encrypt-iterations
                    (attr/key->attribute ::account/password)))}
     {:id       (u 3)
      :name     "Jose Haplon"
      :email    "jose@example.com"
      :active   true
      :password (attr/encrypt "letmein" "some-salt"
                  (::attr/encrypt-iterations
                    (attr/key->attribute ::account/password)))}
     {:id       (u 4)
      :name     "Rose Small"
      :email    "rose@example.com"
      :active   true
      :password (attr/encrypt "letmein" "some-salt"
                  (::attr/encrypt-iterations
                    (attr/key->attribute ::account/password)))}]))

(defn start []
  (mount/start-with-args {:config "config/dev.edn"})
  ;(seed)
  :ok)

(defn stop
  "Stop the server."
  []
  (mount/stop))

(def go start)

(defn restart
  "Stop, refresh, and restart the server."
  []
  (stop)
  (tools-ns/refresh :after 'development/start))

(def reset #'restart)

(comment
  (let [db {:dbtype   "postgresql"
            :dbname   "example"
            :host     "localhost"
            :user     "postgres"
            :password ""}]
    (queries/get-all-accounts {:sql/databases {:production db}} {:ui/show-inactive? true})
    #_(jdbc/query db ["SELECT * FROM account"])
    #_(jdbc/execute! db [(sql/automatic-schema :production account/attributes)]))

  )