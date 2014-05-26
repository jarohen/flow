(ns todomvc.cljs.todomvc-widget
  (:require [dommy.core :as d]
            [cljs.core.async :as a]
            [clojure.string :as s]
            [goog.events.KeyCodes :as kc]
            [flow.core :refer-macros [el<< for<< let<<]])
  (:require-macros [dommy.macros :refer [node sel1]]))

(defn listen! [$el type f]
  (if (.-addEventListener $el)
    (.addEventListener $el (name type) f)
    ;; For IE < 9
    (.attachEvent $el (name type) f))
  $el)

(defn toggle-all-widget [!todos events-ch]
  (el<<
    (let<< [all-done? (every? :done? (vals (<< !todos)))]
      (doto (node [:input#toggle-all {:type "checkbox"
                                      :checked all-done?}])
        (listen! :change #(a/put! events-ch {:type :toggle-all
                                               :done? (not all-done?)}))))))

(defn on-enter [$el f]
  (listen! $el :keyup
    (fn [e]
      (when (= kc/ENTER (.-keyCode e))
        (f e)
        (.preventDefault e)))))

(defn with-edit-handler [$input id !editing? events-ch]
  (on-enter $input
            (fn [e]
              (a/put! events-ch {:type :update
                                 :caption (d/value $input)
                                 :updated-id id})
              (reset! !editing? false))))

(defn edit-input [{:keys [caption id]} !editing? events-ch]
  (-> (node [:input.edit {:value caption,
                          :autofocus true
                          :type "text"}])
      (with-edit-handler id !editing? events-ch)))

(defn with-toggle-handler [$checkbox id events-ch]
  (listen! $checkbox :change
    #(a/put! events-ch {:type :toggle
                        :toggled-id id})))

(defn todo-item-widget [todo events-ch]
  (let [!editing? (atom false)]
    (el<<
      (let<< [editing? (<< !editing?)
              {:keys [caption done? id]} todo]
        (node
         [:li {:class (s/join " " [(when done? "completed")
                                   (when editing? "editing")])}
          (if-not editing?
            [:div.view
             (-> (node [:input.toggle {:type "checkbox", :checked done?}])
                 (with-toggle-handler id events-ch))

             (doto (node [:label caption])
               (listen! :dblclick #(reset! !editing? true)))

             (doto (node [:button.destroy])
               (listen! :click #(a/put! events-ch {:type :delete
                                                     :deleted-id id})))]
            
            (edit-input todo !editing? events-ch))])))))

(defn new-todo-widget [events-ch]
  (let [input (node [:input#new-todo {:placeholder "What needs to be done?" :type "text"}])]
    (doto input
      (on-enter #(do (a/put! events-ch {:type :new-todo
                                        :caption (d/value input)})
                     (d/set-value! input nil))))))

(def filter-todos
  {:all identity
   :active (complement :done?)
   :completed :done?})

(defn todo-list-widget [!todos !todo-filter events-ch]
  (node
   [:ul#todo-list
    (el<<
      (for<< [[id todo] (filter (comp (filter-todos (<< !todo-filter)) val) (<< !todos))]
        (todo-item-widget (assoc todo :id id) events-ch)))]))

(defn stats-widget [!todos]
  (el<<
    (let<< [todo-count (count (remove :done? (vals (<< !todos))))]
      (node
       [:span#todo-count
        [:strong todo-count]
        [:span " items left"]]))))

(def filter-label
  {:all "All"
   :active "Active"
   :completed "Completed"})

(defn filters-widget [!todo-filter]
  (node
   [:ul#filters
    (for [filter-option [:all :active :completed]]
      (el<<
        (let<< [todo-filter (<< !todo-filter)]
          (node
           [:li {:style {:cursor "pointer"}}
            (doto (node [:a ^:attrs (when (= todo-filter filter-option)
                                      {:class "selected"})
                         (filter-label filter-option)])
              (listen! :click #(reset! !todo-filter filter-option)))]))))]))

(defn clear-completed-widget [!todos events-ch]
  (el<<
    (let<< [completed-count (count (filter :done? (vals (<< !todos))))]
        (node
         [:div
          (when-not (zero? completed-count)
            (doto (node [:button#clear-completed
                         (str "Clear completed " completed-count)])
              (listen! :click #(a/put! events-ch {:type :clear-completed}))))]))))

(defn make-todomvc [!todos events-ch]
  (let [!todo-filter (atom :all)]
    (node [:section#todoapp
           [:header#header
            [:h1 "todos"]
            (new-todo-widget events-ch)]
           [:section#main
            (toggle-all-widget !todos events-ch)
            [:label {:for "toggle-all"} "Mark all as complete"]

            (todo-list-widget !todos !todo-filter events-ch)]

           [:footer#info
            [:p "Double-click to edit a todo"]]

           [:footer#footer
            (stats-widget !todos)
            (filters-widget !todo-filter)
            (clear-completed-widget !todos events-ch)]])))
