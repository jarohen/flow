(ns flow.todomvc.ui.todomvc-widget
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            [clojure.string :as s]
            [goog.events.KeyCodes :as kc])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn toggle-all-widget [!todos events-ch]
  (f/el
    (let [all-done? (every? :done? (vals (<< !todos)))]
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
                    ::f/on {:keyup (juxt (f/bind-value! !input-value)
                                         (on-enter #(do
                                                      (a/put! events-ch {:type :update
                                                                         :caption @!input-value
                                                                         :updated-id id})
                                                      (swap! !todo-entry assoc-in [:editing?] false))))}}])))

(defn render-todo [!todo-entry events-ch]
  (f/el
    (let [{:keys [id caption done? editing?]} (<< !todo-entry)]
      [:div.view
       [:input.toggle {:type "checkbox",
                       :checked done?
                       ::f/on {:change #(a/put! events-ch {:type :toggle
                                                           :toggled-id id})}}]

       [:label {::f/on {:dblclick (let [!editing? (!<< !todo-entry [:editing?])]
                                    #(reset! !editing? true))}}
        caption]

       [:button.destroy {::f/on {:click #(a/put! events-ch {:type :delete
                                                            :deleted-id id})}}]])))

(defn todo-item-widget [!todo-entry events-ch]
  (f/el
    (let [{:keys [id caption done? editing?]} (<< !todo-entry)]
      [:li {::f/classes [(when done? "completed")
                         (when editing? "editing")]}
       (if-not editing?
         [render-todo !todo-entry events-ch]
         
         [edit-input !todo-entry events-ch])])))

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

(defn todo-list-widget [!todos !todo-filter events-ch]
  (f/el
    [:ul#todo-list
     (for [[id todo] (->> (<< !todos)
                          (filter (comp (filter-todos (<< !todo-filter)) val))
                          (f/keyed-by key))]
       [todo-item-widget (!<< todo) events-ch])]))

(defn stats-widget [!todos]
  (f/el
    (let [todo-count (->> (<< !todos)
                          vals
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
                               vals
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
        [new-todo-widget events-ch]]

       [:section#main
        [toggle-all-widget !todos events-ch]
        [:label {:for "toggle-all"}
         "Mark all as complete"]

        [todo-list-widget !todos !todo-filter events-ch]]

       [:footer#info
        [:p "Double-click to edit a todo"]]

       [:footer#footer
        [stats-widget !todos]
        [filters-widget !todo-filter]
        [clear-completed-widget !todos events-ch]]])))
