Lab 10
================
ks
2022-11-02

# Data setup

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

``` sql
PRAGMA table_info(actor)
```

``` r
x1
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

``` sql
PRAGMA table_info(actor)
```

| cid | name        | type    | notnull | dflt_value |  pk |
|:----|:------------|:--------|--------:|:-----------|----:|
| 0   | actor_id    | INTEGER |       0 | NA         |   0 |
| 1   | first_name  | TEXT    |       0 | NA         |   0 |
| 2   | last_name   | TEXT    |       0 | NA         |   0 |
| 3   | last_update | TEXT    |       0 | NA         |   0 |

4 records

``` r
dbGetQuery(con,"
      PRAGMA table_info(actor)
           "
)
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

# Exercise 1

Retrieve the actor ID, first name and last name for all actors using the
actor table. Sort by last name and then by first name.

``` r
dbGetQuery(con,"
SELECT   actor_id, first_name, last_name
FROM    actor
ORDER by last_name, first_name
           ")
```

    ##     actor_id  first_name    last_name
    ## 1         58   CHRISTIAN       AKROYD
    ## 2        182      DEBBIE       AKROYD
    ## 3         92     KIRSTEN       AKROYD
    ## 4        118        CUBA        ALLEN
    ## 5        145         KIM        ALLEN
    ## 6        194       MERYL        ALLEN
    ## 7         76    ANGELINA      ASTAIRE
    ## 8        112     RUSSELL       BACALL
    ## 9        190      AUDREY       BAILEY
    ## 10        67     JESSICA       BAILEY
    ## 11       115    HARRISON         BALE
    ## 12       187       RENEE         BALL
    ## 13        47       JULIA    BARRYMORE
    ## 14       158      VIVIEN     BASINGER
    ## 15       174     MICHAEL       BENING
    ## 16       124    SCARLETT       BENING
    ## 17        14      VIVIEN       BERGEN
    ## 18       121        LIZA      BERGMAN
    ## 19        91 CHRISTOPHER        BERRY
    ## 20        60       HENRY        BERRY
    ## 21        12        KARL        BERRY
    ## 22       189        CUBA        BIRCH
    ## 23        25       KEVIN        BLOOM
    ## 24       185     MICHAEL       BOLGER
    ## 25        37         VAL       BOLGER
    ## 26        98       CHRIS      BRIDGES
    ## 27        39      GOLDIE        BRODY
    ## 28       159       LAURA        BRODY
    ## 29       167    LAURENCE      BULLOCK
    ## 30        40      JOHNNY         CAGE
    ## 31        11        ZERO         CAGE
    ## 32       181     MATTHEW       CARREY
    ## 33        86        GREG      CHAPLIN
    ## 34         3          ED        CHASE
    ## 35       176         JON        CHASE
    ## 36       183     RUSSELL        CLOSE
    ## 37        16        FRED      COSTNER
    ## 38       129       DARYL     CRAWFORD
    ## 39        26         RIP     CRAWFORD
    ## 40        49        ANNE       CRONYN
    ## 41       104    PENELOPE       CRONYN
    ## 42       105      SIDNEY        CROWE
    ## 43        57        JUDE       CRUISE
    ## 44       201         TOM       CRUISE
    ## 45       203         TOM       CRUISE
    ## 46       205         TOM       CRUISE
    ## 47       207         TOM       CRUISE
    ## 48        80       RALPH         CRUZ
    ## 49        81    SCARLETT        DAMON
    ## 50         4    JENNIFER        DAVIS
    ## 51       101       SUSAN        DAVIS
    ## 52       110       SUSAN        DAVIS
    ## 53        48     FRANCES    DAY-LEWIS
    ## 54        35        JUDY         DEAN
    ## 55       143       RIVER         DEAN
    ## 56       148       EMILY          DEE
    ## 57       138     LUCILLE          DEE
    ## 58       107        GINA    DEGENERES
    ## 59        41       JODIE    DEGENERES
    ## 60       166        NICK    DEGENERES
    ## 61        89    CHARLIZE        DENCH
    ## 62       123    JULIANNE        DENCH
    ## 63       160       CHRIS         DEPP
    ## 64       100     SPENCER         DEPP
    ## 65       109   SYLVESTER         DERN
    ## 66       173        ALAN     DREYFUSS
    ## 67        36        BURT      DUKAKIS
    ## 68       188        ROCK      DUKAKIS
    ## 69       106     GROUCHO        DUNST
    ## 70        19         BOB      FAWCETT
    ## 71       199       JULIA      FAWCETT
    ## 72        10   CHRISTIAN        GABLE
    ## 73       165          AL      GARLAND
    ## 74       184    HUMPHREY      GARLAND
    ## 75       127       KEVIN      GARLAND
    ## 76       154       MERYL       GIBSON
    ## 77        46      PARKER     GOLDBERG
    ## 78       139        EWAN      GOODING
    ## 79       191     GREGORY      GOODING
    ## 80        71        ADAM        GRANT
    ## 81       179          ED      GUINESS
    ## 82         1    PENELOPE      GUINESS
    ## 83        90        SEAN      GUINESS
    ## 84        32         TIM      HACKMAN
    ## 85       175     WILLIAM      HACKMAN
    ## 86       202         TOM        HANKS
    ## 87       204         TOM        HANKS
    ## 88       206         TOM        HANKS
    ## 89       208         TOM        HANKS
    ## 90       152         BEN       HARRIS
    ## 91       141        CATE       HARRIS
    ## 92        56         DAN       HARRIS
    ## 93        97         MEG        HAWKE
    ## 94       151    GEOFFREY       HESTON
    ## 95       169     KENNETH      HOFFMAN
    ## 96        79         MAE      HOFFMAN
    ## 97        28       WOODY      HOFFMAN
    ## 98       161      HARVEY         HOPE
    ## 99       134        GENE      HOPKINS
    ## 100      113      MORGAN      HOPKINS
    ## 101       50     NATALIE      HOPKINS
    ## 102      132        ADAM       HOPPER
    ## 103      170        MENA       HOPPER
    ## 104       65      ANGELA       HUDSON
    ## 105       52      CARMEN         HUNT
    ## 106      140      WHOOPI         HURT
    ## 107      131        JANE      JACKMAN
    ## 108      119      WARREN      JACKMAN
    ## 109      146      ALBERT    JOHANSSON
    ## 110        8     MATTHEW    JOHANSSON
    ## 111       64         RAY    JOHANSSON
    ## 112       82       WOODY        JOLIE
    ## 113       43        KIRK     JOVOVICH
    ## 114      130       GRETA       KEITEL
    ## 115      198        MARY       KEITEL
    ## 116       74       MILLA       KEITEL
    ## 117       55         FAY       KILMER
    ## 118      153      MINNIE       KILMER
    ## 119      162       OPRAH       KILMER
    ## 120       45       REESE       KILMER
    ## 121       23      SANDRA       KILMER
    ## 122      103     MATTHEW        LEIGH
    ## 123        5      JOHNNY LOLLOBRIGIDA
    ## 124      157       GRETA       MALDEN
    ## 125      136          ED    MANSFIELD
    ## 126       22       ELVIS         MARX
    ## 127       77        CARY  MCCONAUGHEY
    ## 128       70    MICHELLE  MCCONAUGHEY
    ## 129      114      MORGAN    MCDORMAND
    ## 130      177        GENE     MCKELLEN
    ## 131       38         TOM     MCKELLEN
    ## 132      128        CATE      MCQUEEN
    ## 133       27       JULIA      MCQUEEN
    ## 134       42         TOM      MIRANDA
    ## 135      178        LISA       MONROE
    ## 136      120    PENELOPE       MONROE
    ## 137        7       GRACE       MOSTEL
    ## 138       99         JIM       MOSTEL
    ## 139       61   CHRISTIAN       NEESON
    ## 140       62       JAYNE       NEESON
    ## 141        6       BETTE    NICHOLSON
    ## 142      125      ALBERT        NOLTE
    ## 143      150       JAYNE        NOLTE
    ## 144      122       SALMA        NOLTE
    ## 145      108      WARREN        NOLTE
    ## 146       34      AUDREY      OLIVIER
    ## 147       15        CUBA      OLIVIER
    ## 148       69     KENNETH      PALTROW
    ## 149       21     KIRSTEN      PALTROW
    ## 150       33       MILLA         PECK
    ## 151       30      SANDRA         PECK
    ## 152       87     SPENCER         PECK
    ## 153       73        GARY         PENN
    ## 154      133     RICHARD         PENN
    ## 155       88     KENNETH        PESCI
    ## 156      171     OLYMPIA     PFEIFFER
    ## 157       51        GARY      PHOENIX
    ## 158       54    PENELOPE      PINKETT
    ## 159       84       JAMES         PITT
    ## 160       75        BURT        POSEY
    ## 161       93       ELLEN      PRESLEY
    ## 162      135        RITA     REYNOLDS
    ## 163      142        JADA        RYDER
    ## 164      195       JAYNE  SILVERSTONE
    ## 165      180        JEFF  SILVERSTONE
    ## 166       78     GROUCHO      SINATRA
    ## 167       31       SISSY     SOBIESKI
    ## 168       44        NICK     STALLONE
    ## 169       24     CAMERON       STREEP
    ## 170      116         DAN       STREEP
    ## 171      192        JOHN       SUVARI
    ## 172        9         JOE        SWANK
    ## 173      155         IAN        TANDY
    ## 174       66        MARY        TANDY
    ## 175       59      DUSTIN       TAUTOU
    ## 176      193        BURT       TEMPLE
    ## 177       53        MENA       TEMPLE
    ## 178      149     RUSSELL       TEMPLE
    ## 179      200       THORA       TEMPLE
    ## 180      126     FRANCES        TOMEI
    ## 181       18         DAN         TORN
    ## 182       94     KENNETH         TORN
    ## 183      102      WALTER         TORN
    ## 184       20     LUCILLE        TRACY
    ## 185      117       RENEE        TRACY
    ## 186       17       HELEN       VOIGHT
    ## 187       95       DARYL     WAHLBERG
    ## 188        2        NICK     WAHLBERG
    ## 189      196        BELA       WALKEN
    ## 190       29        ALEC        WAYNE
    ## 191      163 CHRISTOPHER         WEST
    ## 192      197       REESE         WEST
    ## 193      172     GROUCHO     WILLIAMS
    ## 194      137      MORGAN     WILLIAMS
    ## 195       72        SEAN     WILLIAMS
    ## 196       83         BEN       WILLIS
    ## 197       96        GENE       WILLIS
    ## 198      164    HUMPHREY       WILLIS
    ## 199      168        WILL       WILSON
    ## 200      147         FAY      WINSLET
    ## 201       68         RIP      WINSLET
    ## 202      144      ANGELA  WITHERSPOON
    ## 203      156         FAY         WOOD
    ## 204       13         UMA         WOOD
    ## 205       63     CAMERON         WRAY
    ## 206      111     CAMERON    ZELLWEGER
    ## 207      186       JULIA    ZELLWEGER
    ## 208       85      MINNIE    ZELLWEGER

Try in SQL directly:

``` sql
SELECT   actor_id, first_name, last_name
FROM    actor
ORDER by last_name, first_name
```

| actor_id | first_name | last_name |
|---------:|:-----------|:----------|
|       58 | CHRISTIAN  | AKROYD    |
|      182 | DEBBIE     | AKROYD    |
|       92 | KIRSTEN    | AKROYD    |
|      118 | CUBA       | ALLEN     |
|      145 | KIM        | ALLEN     |
|      194 | MERYL      | ALLEN     |
|       76 | ANGELINA   | ASTAIRE   |
|      112 | RUSSELL    | BACALL    |
|      190 | AUDREY     | BAILEY    |
|       67 | JESSICA    | BAILEY    |

Displaying records 1 - 10

``` r
# clean up
dbDisconnect(con)
```
