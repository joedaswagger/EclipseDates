(ns assignment3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn filePrint "File print formatting function (for option 1)"[line] 
  (if (not= line "")
    (println line)
    (println "---------------------------------------------------")))

(defn extractInfo [line]
  (let [keys [:date :location :type :significance] info (zipmap keys line)] info)) ;;definition of vector map

(defn vectorAssembly "Makes vector of the maps of the extracted information of the file with 4 keys, filtering the empty space in between"[]
  (use 'clojure.java.io) 
  (try (with-open [rdr (clojure.java.io/reader "eclipse_events.txt")] (->> (line-seq rdr) (filter #(not (empty? %))) (partition 4) (map extractInfo) (vec))) (catch Exception e (str "caught exception: " (.getMessage e))))
  ) 

(defn removeDupes "Turns the assembled vector into a set to get rid of duplicates"[]
  (set (vectorAssembly)))

(defn setBackToVec "Turns our set back into a vector to allow for modifications"[] 
  (vec (removeDupes)))

(defn parsing "Parsing our input because we want to turn our string number into a value (to compare with the counter in modEclispeInput)" [x]
  (Integer. (re-find #"\d+" x)))

(defn WriteNewVec "Writes modified vector of maps back into the file" [x]
  (use 'clojure.java.io)
  (def data x)
  (try ((with-open [wrtr (clojure.java.io/writer "eclipse_events.txt")]
          (doseq [eclipse data] ;;Loops thru all the data in the file line-by-line
            (.write wrtr (str (get eclipse :date) "\n"))
            (.write wrtr (str (get eclipse :location) "\n"))
            (.write wrtr (str (get eclipse :type) "\n"))
            (.write wrtr (str (get eclipse :significance) "\n"))
            (.write wrtr (str "\n"))))) (catch Exception e (str "caught exception: " (.getMessage e)))))

(defn modifyEclipse "Modifies (makes a new one, really) the set-turned-back-to-vector with assoc-in" [index date location type significance]
  (def data (setBackToVec))
  (def newIndex (- index 1)) ;; the new index is adjusted for use in vectors (avoids out of bounds errors)
  (def x (-> data (assoc-in [newIndex :date] (str "Date: " date))
              (assoc-in [newIndex :location] (str "Location: " location))
              (assoc-in [newIndex :type] (str "Type: " type))
              (assoc-in [newIndex :significance] (str "Significance: " significance)))) ;; A thread-first macro that iterates thru all the assoc-ins at once
  (WriteNewVec x))

(defn modEclipseInput "Logic to deal with the input of our updated eclipse data" [index]
  (def counter (atom 0)) ;counter to be compared to our index if the doseq loop arrives at the correct line
  (def data (removeDupes))
  (doseq [eclipse data]
    (swap! counter inc)
    (if (= @counter index)
      (do
        (println "Enter updated date [" (last (str/split (get eclipse :date) #": ")) "]: ")
        (def newDate (read-line))
        (println "Enter updated location [" (last (str/split (get eclipse :location) #": ")) "]: ")
        (def newLocation (read-line))
        (println "Enter updated type [" (last (str/split (get eclipse :type) #": ")) "]: ")
        (def newType (read-line))
        (println "Enter updated significance [" (last (str/split (get eclipse :significance) #": ")) "]: ")
        (def newSignificance (read-line))
        (modifyEclipse index newDate newLocation newType newSignificance)
        ))())
  (reset! counter 0)) ;;Always reset the counter (gets rid of a bug)

(defn printResults "Function to print the details of a given eclipse" [eclipse]
  (println (get eclipse :date))
  (println (get eclipse :location))
  (println (get eclipse :type))
  (println (get eclipse :significance))
  (println "------------------------------------------"))

(defn displayEclipseIndex "Displays the eclipses by index"[]
  (def counter (atom 0))
  (def data (removeDupes))
  (doseq [eclipse data]
    (println "Index: " (swap! counter inc))
    (printResults eclipse))
  (println "\nEnter this index of the event you want to modify: ")
  (def index (read-line))
  (modEclipseInput (parsing index))
  (reset! counter 0))

(defn displayEclipseSearch "Searching algorithm that can find eclipses by date and type" [searchType searchQuery]
  (def data (removeDupes))
  (println "\nSearch Results: \n")
  (def counter (atom 0))
  (def functionLength (count data)) ;; if the counter arrives at the last eclipse and doesn't find anything (i.e. arrives at the equivalent of the length of the function) 
  (doseq [eclipse data]
    (swap! counter inc)
    (def date (last (str/split (get eclipse :date) #": "))) ;; Split the date line by the ": " and take the last value (the actual date)
    (def location (last (str/split (get eclipse :location) #": "))) ;; same thing here
    (if (some? (re-matches #"(?i)date" searchType)) ;; if the inputted type matches the regex required to be valid (is not case-sensitive)
      (do (if (str/includes? (str/lower-case date) (str/lower-case searchQuery)) ;; using str/lowercase to see if there are any similarities between our query and the date
            (printResults eclipse) (if (= @counter functionLength) (println "No results found"))) ())
      )
    (if (some? (re-matches #"(?i)location" searchType)) ;; Same principle here
      (do (if (str/includes? (str/lower-case location) (str/lower-case searchQuery))
            (printResults eclipse) (if (= @counter functionLength) (println "No results found"))) ())
      ))
  (reset! counter 0) 
  (println "\n"))


(defn filereading "Simple filereading function" []
  (use 'clojure.java.io)
  (try ((with-open [rdr (clojure.java.io/reader "eclipse_events.txt")]
          (doseq [line (line-seq rdr)]
            (filePrint (str line))))) (catch Exception e (str "caught exception: " (.getMessage e)))))

(defn exitProgram "System exit function"[] 
  (println "Exiting program...")
  (System/exit 0))

(defn addEclipseData "Eclipse data adding function"[] 
  (println "Enter date: ") ;;data collection
  (def date (read-line))
  (println "Enter location: ")
  (def location (read-line))
  (println "Enter type: ")
  (def type (read-line))
  (println "Enter Significance: ")
  (def significance (read-line))

  (use 'clojure.java.io)
  
  (try((with-open [wrtr (clojure.java.io/writer "eclipse_events.txt" :append true)] ;;Writing the data to the file
         (.write wrtr (str "\n"))
         (.write wrtr (str "\nDate: " date"\n"))
         (.write wrtr (str "Location: " location"\n"))
         (.write wrtr (str "Type: " type"\n"))
         (.write wrtr (str "Significance: " significance)))) (catch Exception e (str "caught exception: " (.getMessage e)))) 
  (def vect setBackToVec) ;; it makes a set out of the new file and turns it back into a vector in order to get rid of any duplicates
  (WriteNewVec (vect)) ;;rewrite the new duplicate-free file
  (println "\nEvent added successfully")
  )

(defn search "Simple search data collecting function" []
  (println "Enter search type (date/location): ")
  (def searchType (read-line))
  (println "Enter search query: ")
  (def searchQuery (read-line))
  (displayEclipseSearch searchType searchQuery))



(defn -main "Main menu logic" [& args] (loop [] (println "=== Eclipse History Encyclopedia ===")
                           (println "1. View Eclipse Events")
                           (println "2. Add New Eclipse Event")
                           (println "3. Modify Eclipse Event")
                           (println "4. Search for Eclipse Events")
                           (println "5. Exit")
                           (println "\nEnter your choice (1-5): ")
                           (let [choice (read-line)]
                             (cond
                               (= choice "1") (filereading)
                               (= choice "2") (addEclipseData)
                               (= choice "3") (displayEclipseIndex)
                               (= choice "4") (search)
                               (= choice "5") (exitProgram)
                               :else (println "Invalid Entry"))) (recur)))
 







  

