(ns mech-keys-scrape.core
  (:gen-class))

(use 'etaoin.api)
(require '[etaoin.keys :as k])

(defn hover-over
  "Hovers the mouse cursor over an element"
  [driver q]
  (let [element (query driver q)
        mouse (-> (make-mouse-input)
                  (add-pointer-move-to-el element))]
    (perform-actions driver mouse)))

(defn get-product-links
  "Return a list of all product links"
  [driver]
  (hover-over driver {:data-navlink-handle "products"})
  (let [all-links-under-products (query-all driver [{:data-parent-link "products"} {:tag :a}])]
    (filter #(re-find #"/products/" (get-element-attr-el driver % :href)) all-links-under-products)))

(defn get-current-price
  "Return the current price of the product on the page"
  [driver]
  (get-element-text driver {:css ".current_price .money"}))

(defn get-current-product-name
  [driver]
  (get-element-text driver {:css "h1.product_name"}))

(defn link->href
  "Return the href attribute of a link"
  [driver link]
  (get-element-attr-el driver link :href))

(defn visit-product
  "Visits the page of a product and prints its details"
  [driver product-link]
  (click-el driver product-link)
  (println (get-current-product-name driver))
  (println (get-current-price driver))
  (println (get-url driver))
  (println (.toString (java.util.Date.))))

(defn filter-unvisited-links
  [driver visited-href links]
  (filter #(not (visited-href (link->href driver %))) links))

(defn visit-all-products
  "Recursively visits all products"
  [driver visited-href]
  (let [all-links (get-product-links driver)
        unvisited-links (filter-unvisited-links driver visited-href all-links)]
    (if-let [target-link (first unvisited-links)]
      (let [target-href (link->href driver target-link)]
        (visit-product driver target-link)
        (recur driver (conj visited-href target-href))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [driver (firefox)]
    (go driver "https://zealpc.net/")
    (visit-all-products driver (hash-set))
    (quit driver)))
