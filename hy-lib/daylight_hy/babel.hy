(require kodhy.macros)

(import
  [hy [HySymbol]]
  [datetime [datetime date]]
  [numpy :as np]
  [pandas :as pd]
  [kodhy.util [double-quote]])

(defn to-el [x]
  "Returns a string for digestion by Daylight in org-babel-execute:hy."
  (cond
    [(string? x)
      (if (in "\n" (.rstrip x))
        (double-quote x)
        (el-row [x]))]
    [(numeric? x)
      (el-row [x])]
    [(instance? dict x) (do
      (setv ks (sorted (.keys x)))
      (el-table [
        ["K" ks]
        ["value" (amap (get x it) ks)]]))]
    [(instance? pd.Series x)
      (el-table (+
        (pandas-index-as-cols x.index)
        [[(or x.name "value") (list x)]]))]
    [(instance? pd.DataFrame x)
      (el-table (+
        (pandas-index-as-cols x.index)
        (amap [it (list (getl x : it))] x)))]
    [(iterable? x) (do
      (setv x (list x))
      (setv head (get x 0))
      (if (coll? head)
        (el-table (lc [col-i (range (len head))]
          [:no-head (amap (get it col-i) x)]))
        (el-row x)))]
    [True
      (el-table [["Python repr" [(repr x)]]])]))

(defn pandas-index-as-cols [ix]
  (if (instance? pd.MultiIndex ix)
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
      (let [[n (max (map len decimals))]]
        (λ (.--format-- (float it) (.format ".{}f" n))))))
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
    [(pd.isnull x)
      "\"\""]
    [(or (is x True) (and (instance? np.bool_ x) x))
      "\"[[cls:boolean-true][True]]\""]
    [(or (is x False) (and (instance? np.bool_ x) (not x)))
      "\"[[cls:boolean-false][False]]\""]
    [(instance? datetime x)
      (double-quote (str (cond
        [(= 0 x.hour x.minute x.second)
          (.date x)]
        [(instance? pd.tslib.Timestamp x)
          (.to-datetime x)]
        [True
          x])))]
    [(instance? date x)
      (double-quote (str x))]
    [(numeric? x)
      (str x)]
    [(instance? HySymbol x)
      (unicode x)]
    [(string? x)
      (double-quote x)]
    [True
      (double-quote (repr x))]))

(defn regnum? [x]
  (and
    (numeric? x)
    (not (np.isnan x))
    (not (instance? bool x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn matplotlib-prelude []
  (import [matplotlib.pyplot :as plt])
  (kwc plt.tick-params :!right :!top)
  (.set-visible (get (. (plt.gca) spines) "top") False)
  (.set-visible (get (. (plt.gca) spines) "right") False))