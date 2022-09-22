Lab 05
================
ks
2022-09-21

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

``` r
library(dtplyr)
```

## Step 1. Read in the data

First download and then read in with data.table:fread()

``` r
if (!file.exists("../lab03/met_all.gz")) {
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", 
                "met_all.gz", method="libcurl", timeout = 60)  
}

met <- data.table::fread("../lab03/met_all.gz")
```

Remove temperatures less than -17C and change elev 9999 to missing value
code.

``` r
met <- met[temp > -17][elev == 9999.0, elev := NA]
```

Read in the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

Merge met data with stations.

``` r
met <-  
  merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 

nrow(met)
```

    ## [1] 2317204

# Question 1: Representative station for the US

Compute mean temperature, wind speed and atmospheric pressure for each
weather station, and pick the weather station with the average value
closest to the median for the US.

``` r
station_averages <- 
     met[ , .(
       temp      = mean(temp, na.rm=T),
       wind.sp   = mean(wind.sp, na.rm=T),
       atm.press = mean(atm.press,na.rm=T)
     ), by = USAFID]
```

The above computes the mean by weather station. Now let’s compute the
median value for each variable.

``` r
stmeds <- station_averages[ , .(
          temp50     = median(temp, na.rm=T),
          windsp50   = median(wind.sp,na.rm=T),
          atmpress50 = median(atm.press,na.rm=T)
)]
stmeds
```

    ##      temp50 windsp50 atmpress50
    ## 1: 23.68406 2.463685   1014.691

A helpful function we might want to use ‘which.min()’.

``` r
station_averages[ , 
              temp_dist50 := abs(temp - stmeds$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp   wind.sp atm.press  temp_dist50
    ##    1: 720458 23.681730  1.209682       NaN  0.002328907
    ##    2: 725515 23.686388  2.709164       NaN  0.002328907
    ##    3: 725835 23.678347  2.652381       NaN  0.005712423
    ##    4: 724509 23.675100  4.066833  1013.863  0.008959632
    ##    5: 720538 23.665932  1.907897       NaN  0.018127186
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.168399783
    ## 1585: 722787 37.258907  2.847381       NaN 13.574848130
    ## 1586: 723805 37.625391  3.532935  1005.207 13.941331392
    ## 1587: 726130  9.189602 12.239908       NaN 14.494456787
    ## 1588: 720385  8.044959  7.298963       NaN 15.639100105

Let’s use which.min

``` r
station_averages[ which.min(temp_dist50)]
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

It matches the result above.

# Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
station_averages <- 
     met[ , .(
       temp      = mean(temp, na.rm=T),
       wind.sp   = mean(wind.sp, na.rm=T),
       atm.press = mean(atm.press,na.rm=T)
     ), by = .(USAFID,STATE)]
head(station_averages)
```

    ##    USAFID STATE     temp  wind.sp atm.press
    ## 1: 690150    CA 33.18763 3.483560  1010.379
    ## 2: 720110    TX 31.22003 2.138348       NaN
    ## 3: 720113    MI 23.29317 2.470298       NaN
    ## 4: 720120    SC 27.01922 2.503079       NaN
    ## 5: 720137    IL 21.88823 1.979335       NaN
    ## 6: 720151    TX 27.57686 2.998428       NaN

``` r
statemeds<- station_averages[ , .(
           temp50    = median(temp, na.rm=T), 
           wind.sp50 = median(wind.sp, na.rm=T)
            ), by = STATE]
statemeds
```

    ##     STATE   temp50 wind.sp50
    ##  1:    CA 22.66268  2.561738
    ##  2:    TX 29.75188  3.413810
    ##  3:    MI 20.51970  2.273423
    ##  4:    SC 25.80545  1.696119
    ##  5:    IL 22.43194  2.237652
    ##  6:    MO 23.95109  2.453547
    ##  7:    AR 26.24296  1.938625
    ##  8:    OR 17.98061  2.011436
    ##  9:    WA 19.24684  1.268571
    ## 10:    GA 26.70404  1.497527
    ## 11:    MN 19.63017  2.616482
    ## 12:    AL 26.33664  1.662132
    ## 13:    IN 22.25059  2.344333
    ## 14:    NC 24.72953  1.627306
    ## 15:    VA 24.37799  1.654183
    ## 16:    IA 21.33461  2.680875
    ## 17:    PA 21.69177  1.784167
    ## 18:    NE 21.87354  3.192539
    ## 19:    ID 20.56798  2.568944
    ## 20:    WI 18.85524  2.053283
    ## 21:    WV 21.94446  1.632107
    ## 22:    MD 24.89883  1.883499
    ## 23:    AZ 30.32372  3.074359
    ## 24:    OK 27.14427  3.852697
    ## 25:    WY 19.80699  3.873986
    ## 26:    LA 27.87430  1.712535
    ## 27:    KY 23.88844  1.895486
    ## 28:    FL 27.57325  2.705069
    ## 29:    CO 21.52650  3.098777
    ## 30:    OH 22.02062  2.554138
    ## 31:    NJ 23.47238  2.148058
    ## 32:    NM 24.94447  3.776083
    ## 33:    KS 24.21220  3.676997
    ## 34:    ND 18.52849  3.956459
    ## 35:    VT 18.61379  1.408247
    ## 36:    MS 26.69258  1.637030
    ## 37:    CT 22.36880  2.101294
    ## 38:    NV 24.56293  3.035050
    ## 39:    UT 24.35182  3.110795
    ## 40:    SD 20.35662  3.665638
    ## 41:    TN 24.88657  1.576035
    ## 42:    NY 20.40674  2.304075
    ## 43:    RI 22.53551  2.583469
    ## 44:    MA 21.30662  2.710944
    ## 45:    DE 24.56026  2.753082
    ## 46:    NH 19.55054  1.563826
    ## 47:    ME 18.79016  2.237210
    ## 48:    MT 19.15492  4.151737
    ##     STATE   temp50 wind.sp50

``` r
station_averages <- 
  merge(
  x = station_averages,
  y = statemeds,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE
)
```

``` r
station_averages[ , temp_dist_state50   := temp - temp50]
station_averages[ , windsp_dist_state50 := wind.sp - wind.sp50] 
station_averages
```

    ##       STATE USAFID     temp  wind.sp atm.press   temp50 wind.sp50
    ##    1:    AL 720265 26.22064 1.136691       NaN 26.33664  1.662132
    ##    2:    AL 720307 25.14605 1.624349       NaN 26.33664  1.662132
    ##    3:    AL 720361 26.62228 1.343410  1015.275 26.33664  1.662132
    ##    4:    AL 720362 27.26504 1.746168  1014.559 26.33664  1.662132
    ##    5:    AL 720376 24.97884 1.296044       NaN 26.33664  1.662132
    ##   ---                                                            
    ## 1584:    WY 726667 23.10219 3.290873  1012.276 19.80699  3.873986
    ## 1585:    WY 726690 20.51681 4.242981  1013.000 19.80699  3.873986
    ## 1586:    WY 726700 19.97665 3.066306  1015.219 19.80699  3.873986
    ## 1587:    WY 726710 16.86569 3.492218  1014.945 19.80699  3.873986
    ## 1588:    WY 726720 21.70287 3.800334  1012.771 19.80699  3.873986
    ##       temp_dist_state50 windsp_dist_state50
    ##    1:        -0.1159996         -0.52544171
    ##    2:        -1.1905914         -0.03778375
    ##    3:         0.2856450         -0.31872221
    ##    4:         0.9284033          0.08403570
    ##    5:        -1.3577997         -0.36608819
    ##   ---                                      
    ## 1584:         3.2951940         -0.58311300
    ## 1585:         0.7098198          0.36899535
    ## 1586:         0.1696556         -0.80768036
    ## 1587:        -2.9412986         -0.38176812
    ## 1588:         1.8958786         -0.07365157

``` r
station_averages[ , eucdist := temp_dist_state50^2 +
                                windsp_dist_state50^2]
```

``` r
repstation <- station_averages[ , .(
                    eucdist = min(eucdist, na.rm=T))
                  , by=STATE]
```

``` r
test <- merge(
  x = station_averages,
  y = repstation,
  by.x = c("eucdist","STATE"),
  by.y = c("eucdist","STATE"),
  all.x = FALSE,
  all.y = TRUE
)
```
