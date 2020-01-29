(ns com.example.model.account
  (:refer-clojure :exclude [name])
  (:require
    #?@(:clj
        [[com.wsscode.pathom.connect :as pc :refer [defmutation]]
         [com.example.model.authorization :as exauth]
         [com.example.components.database-queries :as queries]]
        :cljs
        [[com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]])
    [com.wsscode.pathom.connect :as pc]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.attributes :as attr :refer [defattr]]
    [com.fulcrologic.rad.authorization :as auth]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.ui-state-machines :as uism]))

(defattr id :account/id :uuid
  {::attr/identity?                                      true
   ;; NOTE: These are spelled out so we don't have to have either on classpath, which allows
   ;; independent experimentation. In a normal project you'd use ns aliasing.
   :com.fulcrologic.rad.database-adapters.datomic/schema :production
   :com.fulcrologic.rad.database-adapters.sql/schema     :production
   :com.fulcrologic.rad.database-adapters.sql/tables     #{"account"}
   ::auth/authority                                      :local})

(defattr email :account/email :string
  {:com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   :com.fulcrologic.rad.database-adapters.sql/schema         :production
   :com.fulcrologic.rad.database-adapters.sql/tables         #{"account"}
   :db/unique                                                :db.unique/value
   ::attr/required?                                          true
   ::auth/authority                                          :local})

(defattr active? :account/active? :boolean
  {::auth/authority                                          :local
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   :com.fulcrologic.rad.database-adapters.sql/schema         :production
   :com.fulcrologic.rad.database-adapters.sql/column-name    "active"
   :com.fulcrologic.rad.database-adapters.sql/tables         #{"account"}
   ::form/default-value                                      true})

(defattr password :password/hashed-value :string
  {::auth/authority                                          :local
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   ::attr/required?                                          true})

(defattr password-salt :password/salt :string
  {::auth/authority                                          :local
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   ::attr/required?                                          true})

(defattr password-iterations :password/iterations :int
  {::auth/authority                                          :local
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   ::attr/required?                                          true})

(defattr role :account/role :enum
  {::auth/authority                                          :local
   ::attr/enumerated-values                                  #{:superuser :user}
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}})

(defattr name :account/name :string
  {::auth/authority                                          :local
   :com.fulcrologic.rad.database-adapters.datomic/schema     :production
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids #{:account/id}
   :com.fulcrologic.rad.database-adapters.sql/schema         :production
   :com.fulcrologic.rad.database-adapters.sql/tables         #{"account"}
   ::attr/required?                                          true})

(defattr addresses :account/addresses :ref
  {::attr/target                                                   :address/id
   ::attr/cardinality                                              :many
   :com.fulcrologic.rad.database-adapters.datomic/schema           :production
   :com.fulcrologic.rad.database-adapters.datomic/intended-targets #{:com.example.model.address/id}
   :com.fulcrologic.rad.database-adapters.datomic/entity-ids       #{:account/id}
   :com.fulcrologic.rad.database-adapters.sql/schema               :production
   :com.fulcrologic.rad.database-adapters.sql/tables               #{"account"}
   :db/isComponent                                                 true
   ::auth/authority                                                :local})

(defattr all-accounts :account/all-accounts :ref
  {::attr/target    :account/id
   ::auth/authority :local
   ::pc/output      [{:account/all-accounts [:account/id]}]
   ::pc/resolve     (fn [{:keys [query-params] :as env} _]
                      #?(:clj
                         {:account/all-accounts (queries/get-all-accounts env query-params)}))})

#?(:clj
   (defmutation login [env params]
     {::pc/params #{:username :password}}
     (exauth/login! env params))
   :cljs
   (defmutation login [params]
     (ok-action [{:keys [app state]}]
       (let [status (log/spy :info (some-> state deref ::auth/authorization :local ::auth/status))]
         (if (= status :success)
           (auth/logged-in! app :local)
           (auth/failed! app :local))))
     (error-action [{:keys [app]}]
       (log/error "Login failed.")
       (auth/failed! app :local))
     (remote [env]
       (m/returning env auth/Session))))

#?(:clj
   (defmutation check-session [env _]
     {}
     (exauth/check-session! env))
   :cljs
   (defmutation check-session [_]
     (ok-action [{:keys [app result]}]
       (let [{::auth/keys [provider]} (get-in result [:body `check-session])]
         (uism/trigger! app auth/machine-id :event/session-checked {:provider provider})
         (log/info "session result" result)))
     (remote [env]
       (m/returning env auth/Session))))

(def attributes [id name role email password password-iterations password-salt active? addresses all-accounts])

(def resolvers [login check-session])
