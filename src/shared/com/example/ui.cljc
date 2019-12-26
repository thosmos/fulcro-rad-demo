(ns com.example.ui
  (:require
    [clojure.string :as str]
    [com.example.model.account :as acct]
    [com.example.model.address :as address]
    [cljc.java-time.local-time :as local-time]
    [com.example.ui.login-dialog :refer [LoginForm]]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    #?@(:clj  [[com.fulcrologic.fulcro.dom-server :as dom :refer [div label input]]]
        :cljs [[goog.object :as gobj]
               [com.fulcrologic.fulcro.dom :as dom :refer [div label input]]])
    [com.fulcrologic.fulcro.routing.dynamic-routing :refer [defrouter]]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.dom.events :as evt]
    [com.fulcrologic.rad :as rad]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.authorization :as auth]
    [com.fulcrologic.rad.controller :as controller]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.ids :refer [new-uuid]]
    [com.fulcrologic.rad.rendering.semantic-ui.semantic-ui-controls]
    [com.fulcrologic.rad.report :as report]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]))

;; NOTE: Limitation: Each "storage location" requires a form. The ident of the component matches the identity
;; of the item being edited. Thus, if you want to edit things that are related to a given entity, you must create
;; another form entity to stand in for it so that its ident is represented.  This allows us to use proper normalized
;; data in forms when "mixing" server side "entities/tables/documents".
(form/defsc-form AddressForm [this props]
  {::form/id                address/id
   ::form/attributes        [address/street address/city address/state address/zip]
   ::form/enumeration-order {::address/state (sort-by #(get address/states %) (keys address/states))}
   ::form/cancel-route      ["landing-page"]
   ::form/route-prefix      "address"
   ::form/title             "Edit Address"
   ::form/layout            [[::address/street]
                             [::address/city ::address/state ::address/zip]]})

(form/defsc-form AccountForm [this props]
  {::form/id           acct/id
   ::form/attributes   [acct/name acct/email acct/active? acct/addresses]
   ::form/read-only?   {::acct/email true}
   ::form/cancel-route ["landing-page"]
   ::form/route-prefix "account"
   ::form/title        "Edit Account"
   ;; NOTE: any form can be used as a subform, but when you do so you must add addl config here
   ;; so that computed props can be sent to the form to modify its layout. Subforms, for example,
   ;; don't get top-level controls like "Save" and "Cancel".
   ::form/subforms     {::acct/addresses {::form/ui              AddressForm
                                          ::form/can-delete-row? (fn [parent item] (< 1 (count (::acct/addresses parent))))
                                          ::form/can-add-row?    (fn [parent] true)
                                          ::form/add-row-title   "Add Address"
                                          ;; Use computed props to inform subform of its role.
                                          ::form/subform-style   :inline}}})

(defsc AccountListItem [this {::acct/keys [id name active? last-login] :as props}]
  {::report/columns         [::acct/name ::acct/active? ::acct/last-login]
   ::report/column-headings ["Name" "Active?" "Last Login"]
   ::report/row-actions     {:delete (fn [this id] (form/delete! this ::acct/id id))}
   ::report/edit-form       AccountForm
   :query                   [::acct/id ::acct/name ::acct/active? ::acct/last-login]
   :ident                   ::acct/id}
  #_(dom/div :.item
      (dom/i :.large.github.middle.aligned.icon)
      (div :.content
        (dom/a :.header {:onClick (fn [] (form/edit! this AccountForm id))} name)
        (dom/div :.description
          (str (if active? "Active" "Inactive") ". Last logged in " last-login)))))

(def ui-account-list-item (comp/factory AccountListItem {:keyfn ::acct/id}))

(report/defsc-report AccountList [this props]
  {::report/BodyItem         AccountListItem
   ::report/source-attribute ::acct/all-accounts
   ::report/parameters       {:ui/show-inactive? :boolean}
   ::report/route            "accounts"})


(defn StringBufferedInput
  "Create a new type of input that can be derived from a string. `kw` is a fully-qualified keyword name for the new
  class, and model->string and string->model are functions that can do the conversions (and MUST tolerate nil as input).
  `model->string` MUST return a string (empty if invalid), and `string->model` can return nil if invalid.

  `string-filter` is an optional (fn [string?] string?) that can be used to rewrite incoming strings (i.e. filter
  things).
  "
  [kw {:keys [model->string
              string->model
              string-filter]}]
  (let [cls (fn [props]
              #?(:cljs
                 (cljs.core/this-as this
                   (if-let [init-state (fn [this]
                                         (let [{:keys [value]} (comp/props this)]
                                           {:oldPropValue value
                                            :on-change    (fn [evt]
                                                            (let [{:keys [value]} (comp/props this)
                                                                  nsv (evt/target-value evt)
                                                                  nv  (string->model nsv)
                                                                  {:keys [onChange]} (comp/props this)]
                                                              (comp/set-state! this {:stringValue  nsv
                                                                                     :oldPropValue value
                                                                                     :value        nv})
                                                              (when (and onChange (not= value nv))
                                                                (onChange nv))))
                                            :stringValue  (model->string value)}))]
                     (set! (.-state this) (cljs.core/js-obj "fulcro$state" (init-state this (gobj/get props "fulcro$value"))))
                     (set! (.-state this) (cljs.core/js-obj "fulcro$state" {})))
                   nil)))]
    (comp/configure-component! cls kw
      {:getDerivedStateFromProps
       (fn [latest-props state]
         (let [{:keys [value]} latest-props
               {:keys [oldPropValue stringValue]} state
               ignorePropValue?  (or (= oldPropValue value) (= value (:value state)))
               stringValue       (cond-> (if ignorePropValue?
                                           stringValue
                                           (model->string value))
                                   string-filter string-filter)
               new-derived-state (merge state {:stringValue stringValue})]
           #js {"fulcro$state" new-derived-state}))
       :render
       (fn [this]
         #?(:cljs
            (let [{:keys [value onBlur] :as props} (comp/props this)
                  {:keys [stringValue on-change]} (comp/get-state this)]
              (dom/create-element "input"
                (clj->js
                  (merge props
                    (cond->
                      {:value    stringValue
                       :onChange on-change}
                      onBlur (assoc :onBlur (fn [evt]
                                              (onBlur (-> evt evt/target-value string->model)))))))))))})
    cls))

(def ui-keyword-input (comp/factory (StringBufferedInput ::KeywordInput {:model->string #(str (some-> % name))
                                                                         :string->model #(some-> % keyword)})))
(defn to-int [s]
  #?(:cljs
     (let [n (js/parseInt s)]
       (when-not (js/isNaN n)
         n))))

(let [digits (into #{} (map str) (range 10))]
  (defn just-digits [s]
    (str/join
      (filter digits (seq s)))))

(def ui-int-input (comp/factory (StringBufferedInput ::IntInput {:model->string str
                                                                 :string->model to-int
                                                                 :string-filter just-digits})))


(defn parse-int [x]
  #?(:clj  (Integer/parseInt x)
     :cljs (js/Number x)))

(defn parse-local-time
  "Parse the given time string and return a cljc.java-time.local-time."
  [s]
  (condp re-matches (some-> s (str/lower-case))
    #"^\s*(\d{1,2})\s*(a|p|am|pm)?\s*$"
    :>> (fn [[_ h ap]] (let [h (parse-int h)]
                         (local-time/of
                           (cond-> h
                             (and (#{"a" "am"} ap) (= 12 h)) (- 12)
                             (and (#{"p" "pm"} ap) (not= 12 h)) (+ 12))
                           0)))
    #"^\s*(\d{1,2}):(\d{1,2})\s*(a|p|am|pm)?\s*$"
    :>> (fn [[_ h m ap]] (let [h (parse-int h)
                               m (parse-int m)]
                           (local-time/of
                             (cond-> h
                               (and (#{"a" "am"} ap) (= 12 h)) (- 12)
                               (and (#{"p" "pm"} ap) (not= 12 h)) (+ 12))
                             m)))
    #".*" nil))

(comment
  (str (parse-local-time "12p")))
(def ui-time-input
  (comp/factory
    (StringBufferedInput ::LocalTimeInput {:model->string str
                                           :string->model parse-local-time})))

(defsc LandingPage [this {:keys [current]}]
  {:query         [:current]
   :ident         (fn [] [:component/id ::LandingPage])
   :initial-state {:current nil}
   :route-segment ["landing-page"]}
  (dom/div :.ui.form
    (dom/button :.ui.button {:onClick (fn [] (m/set-value! this :current (local-time/of 16 45)))} "Set!")
    (div :.ui.field
      (dom/label "Try ME!")
      (ui-time-input {:value    current
                      :type     "time"
                      :onBlur   (fn [k] (log/info "blur" k))
                      :onChange (fn [k]
                                  (log/info "set" k)
                                  (m/set-value! this :current k))}))))

;; This will just be a normal router...but there can be many of them.
(defrouter MainRouter [this props]
  {:router-targets [LandingPage AccountList AccountForm]})

(def ui-main-router (comp/factory MainRouter))

(auth/defauthenticator Authenticator {:local LoginForm})

(def ui-authenticator (comp/factory Authenticator))

(defsc Root [this {:keys [authenticator router]}]
  {:query         [{:authenticator (comp/get-query Authenticator)}
                   {:router (comp/get-query MainRouter)}]
   :initial-state {:router        {}
                   :authenticator {}}}
  (div
    (div :.ui.top.menu
      (div :.ui.item "Demo Application")
      ;; TODO: Show how we can check authority to hide UI
      (dom/a :.ui.item {:onClick (fn [] (form/edit! this AccountForm (new-uuid 1)))} "My Account")
      (dom/a :.ui.item {:onClick (fn []
                                   (form/delete! this :com.example.model.account/id (new-uuid 2)))}
        "Delete account 2")
      (dom/a :.ui.item {:onClick (fn []
                                   (controller/route-to! this :main-controller ["accounts"]))} "List Accounts"))
    (div :.ui.container.segment
      (ui-authenticator authenticator)
      (ui-main-router router))))

(def ui-root (comp/factory Root))

