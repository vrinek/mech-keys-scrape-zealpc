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
  "Return the price of the product on the page"
  [driver]
  (get-element-text driver {:css ".modal_price"}))

(defn get-current-product-name
  "Return the product name on the page"
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
  "Visits each variant of the current product and apply fn to them"
  [driver product-map fn visited-variants]
  (let [all-variants (get-variant-labels driver)
        unvisited-variants (filter #(not (visited-variants %)) all-variants)]
    (if-let [target-variant (first unvisited-variants)]
      (do
        (click-el driver target-variant)
        (let [variant-map {:variant (get-element-text-el driver target-variant)
                           :price (get-current-price driver)}
              merged-map (merge product-map variant-map)]
          (fn merged-map))
        (recur driver product-map fn (conj visited-variants target-variant))))))

(defn visit-product
  "Visits the page of a product and its variants"
  [driver product-link fn]
  (click-el driver product-link)
  (let [product-map {:name (get-current-product-name driver)
                     :price (get-current-price driver)
                     :url (get-url driver)
                     :last-updated (java.util.Date.)}]
    (visit-all-variants driver product-map fn (hash-set))
    product-map))

(defn filter-unvisited-links
  [driver visited-href links]
  (filter #(not (visited-href (link->href driver %))) links))

(defn visit-all-products
  "Recursively visits all products and apply fn to them"
  ([driver fn]
   (visit-all-products driver fn (hash-set)))
  ([driver fn visited-href]
  (let [all-links (get-product-links driver)
        unvisited-links (filter-unvisited-links driver visited-href all-links)]
    (if-let [target-link (first unvisited-links)]
      (let [target-href (link->href driver target-link)
            product-map (visit-product driver target-link fn)]
        (recur driver fn (conj visited-href target-href)))))))

(defn append-map-as-json
  "Transform a map to JSON and append it to a file"
  [file map]
  (spit file (str (generate-string map) "\n") :append true))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [driver (firefox)]
    (go driver "https://zealpc.net/")
    (visit-all-products driver (partial append-map-as-json output-file))
    (quit driver)))
