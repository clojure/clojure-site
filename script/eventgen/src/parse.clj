(ns parse
    (:require
      [clojure.java.io :as jio]
      [clojure.string :as str])
    (:import
      [java.time LocalDate]
      [java.io StringReader]
      [net.fortuna.ical4j.model PropertyListAccessor]
      [net.fortuna.ical4j.data CalendarBuilder]))

(defn coerce-date
      [s]
      (try
        (str (subs s 0 4) "-" (subs s 4 6) "-" (subs s 6 8))
        (catch Throwable _ s)))

(defn extract-props
      [^PropertyListAccessor component]
      (loop [[p & rp] (.getAll (.getPropertyList component))
             m {}]
            (if p
              (let [prop-name (-> p .getName str/lower-case keyword)
                    prop-val (cond-> (.getValue p)
                                     (#{:dtstart :dtend} prop-name) (coerce-date))]
                   (recur rp (cond-> m (not (str/blank? prop-val)) (assoc prop-name prop-val))))
              m)))

(defn trim-punc-r
      [s]
      (let [last (dec (.length s))]
           (if (contains? #{\space \tab \newline \formfeed \backspace \return \: \( \- \& \| \,}
                          (.charAt s last))
             (trim-punc-r (subs s 0 last))
             s)))

(defn title
      [summary limit]
      (if (< (count summary) limit)
        (trim-punc-r summary)
        (let [prefixes (->> [(second (re-matches #"(.[^:]+):.*" summary)) ;; . to skip leading colon
                             (second (re-matches #"(.[^(]+)\(.*" summary)) ;; . to skip leading (
                             (second (re-matches #"(.+)\s-.*" summary)) ;; \s as - is commonly in kebab case names
                             (second (re-matches #"(.+)&ndash.*" summary))
                             (second (re-matches #"(.+)&mdash.*" summary))]
                            ;(#(do (println %) %))
                            (remove nil?)
                            (remove #(>= (count %) limit))
                            (remove #(< (count %) 15))
                            (sort-by count))]
             (if (pos? (count prefixes))
               (trim-punc-r (last prefixes))
               (let [chop (subs summary 0 40)
                     ri (str/last-index-of chop " ")]
                    (trim-punc-r (subs summary 0 ri)))))))

(defn add-title
      [event]
      (if-let [summary (:summary event)]
              (assoc event :title (title summary 40))
              event))

(defn adoc-file
      [event]
      (let [title (:title event)
            year (subs (:dtstart event) 0 4)]
           (str year "/"
                (-> title
                    str/lower-case
                    (str/replace #"[^a-z0-9\-]" (constantly "-"))
                    (str/replace #"[-]{2,}+" "-")
                    trim-punc-r)
                "-"
                (hash (:uid event))
                ".adoc")))

(defn write-event
      [dir {:keys [dtstart dtend title location summary description url] :as event}]
      (let [f (jio/file dir (adoc-file event))
            contents
            (str "= " title "\n"
                 dtstart "\n"
                 ":jbake-type: event\n"
                 ":jbake-edition: \n" ;; not using
                 ":jbake-link: " url "\n"
                 ":jbake-location: " (or location "unknown") "\n"
                 ":jbake-start: " dtstart "\n"
                 ":jbake-end: " dtend "\n"
                 "\n"
                 "== " summary "\n\n"
                 (str/join "\n" (map #(str % " +") (str/split description #"\r?\n|\r"))) "\n\n")]
           (jio/make-parents f)
           (spit f contents)))

(defn parse-feed
      [{:keys [dir]}]
      (let [ics (slurp "https://www.clojurians-zulip.org/feeds/events.ics")
            builder (CalendarBuilder.)
            cal (.build builder (StringReader. ics))
            comps (.getComponents cal)
            year (.getYear (LocalDate/now))
            events (->> comps
                        (map extract-props)
                        (filter #(<= year (parse-long (subs (:dtstart %) 0 4))))
                        (map add-title))]
           (run! #(write-event dir %) events)))

(comment
  (def ics (slurp "https://www.clojurians-zulip.org/feeds/events.ics"))
  (def builder (CalendarBuilder.))
  (def cal (.build builder (StringReader. ics)))
  (def comps (.getComponents cal))
  (count comps)
  (take 50 (map class comps))
  (class (first comps))
  (def c (first comps))
  (class (second (.getProperties c)))
  (count (.getComponentList c))
  (.getProperty c "dtstart")
  (extract-props c)
  (set! *print-length* 100)
  (let [ds (take 100 (map add-title (map extract-props comps)))]
       (run! #(println (adoc-file %)) ds)
       #_(run! #(println (:dtstart %) (:dtend %)) ds)
       #_(run! #(println (title (:summary %) 40) "\t" %) ds))


  (parse-feed {:dir "/Users/alex.miller/code/clojure-site/content/events"})

  )
