(require  kodhy.macros [cbind amap qw $ cached])

(import
  numpy :as np
  pandas :as pd
  kodhy.util [T F valcounts])

(setv mtcars (cbind
  :I ["RX4" "RX4 Wag" "710" "4 Drive" "Sportabout" "Valiant" "360" "240D" "230" "280" "280C" "450SE" "450SL" "450SLC" "Fleetwood" "Continental" "Imperial" "128" "Civic" "Corolla" "Corona" "Challenger" "Javelin" "Z28" "Firebird" "X1-9" "914-2" "Europa" "Pantera L" "Dino" "Bora" "142E" "Batmobile"]
  :mpg [21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7 15.0 21.4 None]
  :cyl (pd.Series (amap (and it (.format "{}c" it)) [6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4 None])
    :dtype "category")
  :disp [160.0 160.0 108.0 258.0 360.0 225.0 360.0 146.7 140.8 167.6 167.6 275.8 275.8 275.8 472.0 460.0 440.0 78.7 75.7 71.1 120.1 318.0 304.0 350.0 400.0 79.0 120.3 95.1 351.0 145.0 301.0 121.0 None]
  :hp [110 110 93 110 175 105 245 62 95 123 123 180 180 180 205 215 230 66 52 65 97 150 150 245 175 66 91 113 264 175 335 109 None]
  :wt [2.620 2.875 2.320 3.215 3.440 3.460 3.570 3.190 3.150 3.440 3.440 4.070 3.730 3.780 5.250 5.424 5.345 2.200 1.615 1.835 2.465 3.520 3.435 3.840 3.845 1.935 2.140 1.513 3.170 2.770 3.570 2.780 None]
  :vs [F F T T F T F T T T T F F F F F F T T T T F F F F T F T F F F T F]
  :am [T T T F F F F F F F F F F F F F F T T T F F F F F T T T T T T T F]
  :gear [4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4 None]
  :date (pd.to-datetime [19850212 19890611 19951017 19860918 20100524 20071206 20081122 19870222 20061003 20101111 19941116 20030412 20090721 20100720 20120110 19790410 19720911 20050721 19800406 20120726 19900519 20200418 19930314 19770425 19720818 20190907 19850215 19740317 19960410 19930113 20030906 20030809 20030809]
    :format "%Y%m%d")))

(setv (get mtcars.loc #("Batmobile" "date")) pd.NaT)
  ; Workaround for https://github.com/pandas-dev/pandas/issues/30011
