(require
  hyrule [unless]
  kodhy.macros [lc amap rmap fmap λ getl])

(import
  collections [OrderedDict]
  collections.abc [Iterable]
  datetime [datetime date]
  numbers [Number]
  hyrule [coll?]
  numpy :as np
  pandas :as pd
  kodhy.util [T F double-quote]
  toolz [first second])

(defn to-el [x]
  "Returns a string for digestion by Daylight in org-babel-execute:hy."
  (cond
    (isinstance x str)
      (if (in "\n" (.rstrip x))
        (double-quote x)
        (el-row [x]))
    (isinstance x Number)
      (el-row [x])
    (isinstance x dict) (do
      (setv ks (list (.keys x)))
      (unless (isinstance x OrderedDict)
        (setv ks (sorted ks)))
      (el-table [
        ["K" ks]
        ["value" (amap (get x it) ks)]]))
    (isinstance x pd.Series)
      (el-table (+
        (pandas-index-as-cols x.index)
        [[(or x.name "value") (.tolist x)]]))
    (isinstance x pd.DataFrame)
      (el-table (+
        (pandas-index-as-cols x.index)
        (rmap [[vname v] (.items x)] [vname (.tolist v)])))
    (isinstance x Iterable) (do
      (setv x (list x))
      (setv head (get x 0))
      (if (coll? head)
        (el-table (lc [col-i (range (len head))]
          [:no-head (amap (get it col-i) x)]))
        (el-row x)))
    T
      (el-table [["Python repr" [(repr x)]]])))

(defn pandas-index-as-cols [ix]
  (if (isinstance ix pd.MultiIndex)
    (lc [[i name] (enumerate ix.names)]
      [(or name (.format "i{}" i)) (amap (get it i) ix)])
    [[(or ix.name "I") (list ix)]]))

(defn el-table [xs]
  (setv nrows (len (second (first xs))))
  ; Adjust trailing 0s in each column.
  (for [col (map second xs)]
    (setv decimals (fmap
      (second (.split (str (float it)) "."))
      (regnum? it)
      col))
    (setv f (if (all (amap (= it "0") decimals))
      ; If the numbers are all integers, remove the decimal point
      ; entirely.
      int
      ; Otherwise, give all the numbers the same number of
      ; decimal digits as the one with the most.
      (do
        (setv n (max (map len decimals)))
        (λ (.__format__ (float it) (.format ".{}f" n))))))
    (for [[row-i x] (enumerate col)]
      (when (regnum? x)
        (setv (get col row-i) (f x)))))
  ; Return the table.
  (.format "({} {})"
    (if (= (first (first xs)) :no-head)
      ""
      (+ (el-row (map first xs)) " hline"))
    (.join " " (lc [row-i (range nrows)]
      (el-row (amap (get (second it) row-i) xs))))))

(defn el-row [xs]
  (.format "({})" (.join " " (list (map el-atom xs)))))

(defn el-atom [x]
  (cond
    (pd.isnull x)
      "\"\""
    (or (is x T) (and (isinstance x np.bool_) x))
      "\"[[cls:boolean-true][True]]\""
    (or (is x F) (and (isinstance x np.bool_) (not x)))
      "\"[[cls:boolean-false][False]]\""
    (isinstance x datetime)
      (double-quote (str (cond
        (= 0 x.hour x.minute x.second)
          (.date x)
        (isinstance x pd.Timestamp)
          (.to-pydatetime x)
        T
          x)))
    (isinstance x date)
      (double-quote (str x))
    (isinstance x Number)
      (str x)
    (isinstance x hy.models.Symbol)
      (str x)
    (isinstance x str)
      (double-quote x)
    T
      (double-quote (repr x))))

(defn regnum? [x]
  (and
    (isinstance x Number)
    (not (np.isnan x))
    (not (isinstance x bool))))
