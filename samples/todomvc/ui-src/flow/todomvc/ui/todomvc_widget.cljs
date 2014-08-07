(ns flow.todomvc.ui.todomvc-widget
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            [clojure.string :as s]
            [goog.events.KeyCodes :as kc]))

(defn toggle-all-widget [!todos events-ch]
  (f/el
    (let [all-done? (every? :done? (<< !todos))]
      [:input#toggle-all {:type "checkbox"
                          :checked all-done?
                          ::f/on {:change #(a/put! events-ch {:type :toggle-all
                                                              :done? (not all-done?)})}}])))

(defn on-enter [f]
  (fn [e]
    (when (= kc/ENTER (.-keyCode e))
      (f e)
      (.preventDefault e))))

(defn edit-input [!todo-entry events-ch]
  (f/el
    (let [{:keys [caption id !editing?]} (<< !todo-entry)
          !input-value (atom caption)]
      [:input.edit {:value caption,
                    :autofocus true
                    :type "text"
                    ::f/on {:keyup (juxt (f/bind-value !input-value)
                                         (on-enter #(do
                                                      (a/put! events-ch {:type :update
                                                                         :caption @!input-value
                                                                         :updated-id id})
                                                      (swap! !todo-entry assoc-in [:editing?] false))))}}])))

(defn todo-item-widget [!todo-entry events-ch]
  (let [!editing? (atom false)]
    (f/el
      (let [{:keys [id caption done? editing?]} (<< !todo-entry)]
        [:li {::f/classes [(when done? "completed")
                           (when editing? "editing")]}
         (if-not editing?
           [:div.view
            [:input.toggle {:type "checkbox",
                            :checked done?
                            ::f/on {:change #(a/put! events-ch {:type :toggle
                                                                :toggled-id id})}}]

            [:label {::f/on {:dblclick #(reset! !editing? true)}}
             caption]

            [:button.destroy {::f/on {:click #(a/put! events-ch {:type :delete
                                                                 :deleted-id id})}}]]
           
           (edit-input !todo-entry events-ch))]))))

(defn new-todo-widget [events-ch]
  (let [!input-value (atom nil)]
    (f/el
      [:input#new-todo {:placeholder "What needs to be done?"
                        :type "text"
                        :value (<< !input-value)
                        ::f/on {:keyup (juxt (f/bind-value! !input-value)
                                             
                                             (on-enter #(do (a/put! events-ch {:type :new-todo
                                                                               :caption @!input-value})
                                                            (reset! !input-value nil))))}}])))

(def filter-todos
  {:all identity
   :active (complement :done?)
   :completed :done?})

(defn prn-todo []
  )

(defn todo-list-widget [!todos !todo-filter events-ch]
  (prn !todos)
  (f/el
    [:ul#todo-list
     (for [{:keys [id todo] :as todo-item} (doto (->> (<< !todos)
                                                      (filter (filter-todos (<< !todo-filter))))
                                             prn)]
       (todo-item-widget (!<< todo-item) events-ch))]))

(defn stats-widget [!todos]
  (f/el
    (let [todo-count (->> (<< !todos)
                          (remove :done?)
                          count)]
      [:span#todo-count
       [:strong todo-count]
       [:span " items left"]])))

(def filter-label
  {:all "All"
   :active "Active"
   :completed "Completed"})

(defn filters-widget [!todo-filter]
  (f/el
    [:ul#filters
     (let [todo-filter (<< !todo-filter)]
       (for [filter-option [:all :active :completed]]
         [:li {::f/style {:cursor :pointer}}
          [:a {::f/classes [(when (= todo-filter filter-option)
                              "selected")]
               ::f/on {:click #(reset! !todo-filter filter-option)}}
           (filter-label filter-option)]]))]))

(defn clear-completed-widget [!todos events-ch]
  (f/el
    (let [completed-count (->> (<< !todos)
                               (filter :done?)
                               count)]
      [:div
       (when-not (zero? completed-count)
         [:button#clear-completed {::f/on {:click #(a/put! events-ch {:type :clear-completed})}}
          (str "Clear completed " completed-count)])])))

(defn make-todomvc [!todos events-ch]
  (let [!todo-filter (atom :all)]
    (f/el
      [:section#todoapp
       [:header#header
        [:h1 "todos"]
        (new-todo-widget events-ch)]

       [:section#main
        (toggle-all-widget !todos events-ch)
        [:label {:for "toggle-all"}
         "Mark all as complete"]

        (todo-list-widget !todos !todo-filter events-ch)]

       [:footer#info
        [:p "Double-click to edit a todo"]]

       [:footer#footer
        (stats-widget !todos)
        (filters-widget !todo-filter)
        (clear-completed-widget !todos events-ch)]])))
