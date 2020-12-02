(ns mech-keys-scrape.core
  (:require [etaoin.api :refer :all]
            [cheshire.core :refer :all])
  (:gen-class))

(def output-file "zeal-products.json")

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

(defn visible-el?
  "Checks whether an element is visible on the page"
  [driver el unique-attr]
  (visible? driver {unique-attr (get-element-attr-el driver el unique-attr)}))

(defn get-variant-labels
  "Return a list <label> elements for all variants on the product page"
  [driver]
  (filter
   #(visible-el? driver % :for)
   (query-all driver {:css ".swatch_options label"})))

(defn visit-all-variants
  "Visits each variant of the current product"
  [driver visited-variants]
  (let [all-variants (get-variant-labels driver)
        unvisited-variants (filter #(not (visited-variants %)) all-variants)]
    (if-let [target-variant (first unvisited-variants)]
      (do
        (click-el driver target-variant)
        (println (get-element-text-el driver target-variant))
        (println (get-element-text driver {:css ".modal_price"}))
        (recur driver (conj visited-variants target-variant))))))

(defn visit-product
  "Visits the page of a product and prints its details"
  [driver product-link]
  (click-el driver product-link)
  (let [product-map {:name (get-current-product-name driver)
                     :price (get-current-price driver)
                     :url (get-url driver)
                     :last-updated (java.util.Date.)}]
    (println product-map)
    (visit-all-variants driver (hash-set))
    product-map))

(defn filter-unvisited-links
  [driver visited-href links]
  (filter #(not (visited-href (link->href driver %))) links))

(defn visit-all-products
  "Recursively visits all products"
  ([driver]
   (visit-all-products driver (hash-set)))
  ([driver visited-href]
  (let [all-links (get-product-links driver)
        unvisited-links (filter-unvisited-links driver visited-href all-links)]
    (if-let [target-link (first unvisited-links)]
      (let [target-href (link->href driver target-link)
            product-map (visit-product driver target-link)]
        (spit output-file (str (generate-string product-map) "\n") :append true)
        (recur driver (conj visited-href target-href)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [driver (firefox)]
    (go driver "https://zealpc.net/")
    (visit-all-products driver)
    (quit driver)))
