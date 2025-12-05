RProcedureLanguagesInNYC2024
================
Tomonori Nagano
2025-12-03

# RProcedureLanguagesInNYC2024

- Author: Tomonori Nagano <tnagano@lagcc.cuny.edu>
- Date: Saturday, March 23, 2024
- Script purpose: This R script will analyze the language data by the
  NYC Department of City Planning and the U.S. Census data obtained
  through R’s tidycensus

# Language Data from the NYC Department of City Planning
## Setting up the environment

``` r
# clear the cache
rm(list = ls())

library(ggplot2); library(xtable); library(gdata); library(Hmisc); library(RColorBrewer); library(foreign); 
```

    ## Warning: package 'Hmisc' was built under R version 4.3.3

``` r
library(reshape2); 

# turning off scientific notation of numbers
options(scipen=999)

#setwd("~/Desktop/")
knitr::opts_knit$set(root.dir = dirname(rstudioapi::getActiveDocumentContext()$path))
    
# change the default width
width.default <- getOption("width"); options(width=200)

# the add comma function and the add percent function
addComma<-function(x) {format(x, big.mark = ',', trim = TRUE, scientific = FALSE)}

# creating a notin function
`%notin%` <- Negate(`%in%`)
```

## Loading data

- The data files were obtained from the webpage of the NYC Department of
  City Planning on Saturday, March 23, 2024
  - <https://www.nyc.gov/site/planning/planning-level/nyc-population/american-community-survey.page.page>
  - “Languages” - “Top Languages Spoken at Home by the Limited English
    Proficient (LEP) Population” - “2021 ACS 5-Year” - “PUMA (Community
    District)”

``` r
thisData <- read.csv("data/dcp-lang-spk-at-home-nyc-boro_2021acs5yr-PUMSExtracted.csv")
names(thisData)[1] <- "Language"
thisData$Language = as.factor(thisData$Language)
rownames(thisData) = thisData[,1]
thisData = thisData[,2:7]
thisData.totalPopulation = thisData[1,]
thisData = thisData[-c(1),]
```

## Analyzing data

- The total number of NYC population

``` r
addComma(thisData.totalPopulation)
```

    ##                  New.York.City     Bronx  Brooklyn Manhattan    Queens Staten.Island
    ## Total Population     8,194,266 1,365,366 2,521,451 1,591,854 2,249,945       465,650

- The total number of people who speak languages other than English at
  home population

``` r
addComma(thisData)
```

    ##                                                          New.York.City   Bronx Brooklyn Manhattan  Queens Staten.Island
    ## Spanish                                                      1,923,827 644,958  371,101   334,654 523,480        49,634
    ## Chinese (incl. Mandarin, Cantonese, and Min Nan Chinese)       503,880   5,872  178,385    85,948 212,357        21,318
    ## Russian                                                        203,691   2,963  132,122    14,652  33,964        19,990
    ## Haitian                                                         99,103   4,898   62,004     3,904  27,587           710
    ## Bengali                                                        122,797  18,748   21,019     3,939  77,862         1,229
    ## Yiddish                                                        100,227     212   97,593       742   1,130           550
    ## French                                                          89,493  19,627   21,767    34,484  11,624         1,991
    ## Italian                                                         64,429   5,995   18,948    10,291  20,469         8,726
    ## Korean                                                          64,700   2,263    6,531    11,216  42,319         2,371
    ## Arabic                                                          73,670   7,102   33,327     6,281  17,763         9,197
    ## Polish                                                          52,433     434   15,298     3,751  26,615         6,335
    ## Hebrew                                                          44,328   1,000   23,382     9,607   7,308         3,031
    ## Urdu                                                            45,431   2,464   21,614     2,796  14,191         4,366
    ## Greek                                                           37,813   1,859    5,446     5,270  23,698         1,540
    ## Tagalog                                                         32,043   2,671    3,687     2,415  21,639         1,631
    ## Albanian                                                        35,827   9,091    7,377     1,983   8,439         8,937
    ## Hindi                                                           31,093   1,191    3,854     8,130  17,113           805
    ## Punjabi                                                         25,235     379    2,309       259  22,227            61
    ## Akan (incl. Twi)                                                26,471  19,187    3,752       442   2,870           220
    ## Japanese                                                        21,150     414    4,517     9,678   6,465            76
    ## German                                                          17,983     838    5,252     7,438   4,197           258
    ## Portuguese                                                      19,664   1,011    4,357     6,969   7,113           214
    ## Filipino                                                        17,170   1,430    1,624     1,525  11,978           613
    ## Romanian                                                        12,890     312    1,196     3,849   7,470            63
    ## Turkish                                                         11,738     282    4,945     2,525   2,737         1,249
    ## Ukrainian                                                       12,116     346    7,848       802   1,912         1,208
    ## Vietnamese                                                      10,423   2,879    2,888     1,059   3,460           137
    ## Yoruba                                                          13,556   3,562    4,077     1,531   3,290         1,096
    ## Farsi                                                            8,701     241    1,959     2,214   4,000           287
    ## Gujarati                                                         9,004     406      933     1,660   5,674           331
    ## Malayalam                                                        7,063     170      820       526   3,637         1,910
    ## Fulah                                                            6,438   3,610    1,013     1,126     262           427
    ## Hungarian                                                        4,494     432    1,682       809   1,525            46
    ## Igbo                                                             6,188   3,299      874       123   1,567           325
    ## Nepali                                                          10,112     109      345       338   9,190           130
    ## Jamaican Creole English                                          8,148   3,364    2,522       348   1,840            74
    ## Manding languages                                                7,008   4,426      290     2,058     108           126
    ## Thai                                                             6,319     203      511       689   4,761           155
    ## Croatian                                                         4,788     229      381       486   3,604            88
    ## Serbian                                                          5,548     258    1,286     1,055   2,809           140
    ## Other languages of Asia                                          9,400      29    7,514        80   1,677           100
    ## Wolof                                                            4,681   2,248      354     1,446     224           409
    ## Dutch                                                            4,758     322    1,291     1,727   1,337            81
    ## Other Mande languages                                            6,275   5,110      723       232       0           210
    ## Burmese                                                          6,390      20    1,381       196   4,442           351
    ## Armenian                                                         3,941     264      580       808   2,122           167
    ## Tamil                                                            4,397     632      472     1,108   1,608           577
    ## Pashto                                                           3,333     174      287        18   2,745           109
    ## Other and unspecified languages                                  7,986     128    4,880       271   1,869           838
    ## Bulgarian                                                        2,962     587      363       653   1,213           146
    ## Karen languages                                                    138       0        0        48      90             0
    ## Other Niger-Congo languages                                      4,064   2,077      701       265     662           359
    ## Bosnian                                                          3,341     432      524       220   2,118            47
    ## Serbocroatian                                                    1,774     184      155       346     678           411
    ## Swedish                                                          2,387      95      897     1,243     143             9
    ## Other languages of Africa                                        3,123     571    1,592       546     319            95
    ## Tibetan                                                          8,125       0    1,001        99   7,025             0
    ## Khmer                                                            1,791     784      357        79     567             4
    ## Amharic                                                          3,137   1,308      330     1,156     246            97
    ## India nec                                                        1,233     158      380       125     447           123
    ## Telugu                                                           2,343      73      601       866     739            64
    ## Indonesian                                                       3,403     123      136       499   2,628            17
    ## Macedonian                                                       1,489     102      101       696     285           305
    ## Sinhala                                                          2,442       0       45        70     690         1,637
    ## Pakistan nec                                                     1,730      17      733        24     956             0
    ## Other Indo-Iranian languages                                     3,067      31    1,742       341     870            83
    ## Other Indo-European languages                                    1,670      72      797       535     222            44
    ## Slovak                                                           1,397     124      417       162     553           141
    ## Edoid languages                                                  1,223     664      307         0     252             0
    ## Irish                                                            1,200     134      278       292     472            24
    ## Swahili                                                          1,060     384      229       220     227             0
    ## Other Afro-Asiatic languages                                     4,395   2,904      345       804     257            85
    ## Nilo-Saharan languages                                             180      53       24       103       0             0
    ## Danish                                                           1,095      36      410       488     153             8
    ## Dari                                                               283      52       17         0     214             0
    ## Chin languages                                                     137       0      137         0       0             0
    ## Cebuano                                                          1,631     213      176       393     794            55
    ## Other English-based Creole languages                             3,320     208    1,887       244     869           112
    ## Ga                                                               1,494   1,064      352         8      70             0
    ## Lithuanian                                                       1,680      79      688       220     693             0
    ## Marathi                                                            993       0      230       408     348             7
    ## Malay                                                              650       0      109       169     368             4
    ## Other Central and South American languages                       1,818     751      779        47     223            18
    ## Other Bantu languages                                              989     144      162        77     561            45
    ## Czech                                                            2,315       0      819       893     504            99
    ## Gbe languages                                                    1,441   1,156        3       136      88            58
    ## Norwegian                                                          933      16      435       430       7            45
    ## Finnish                                                            876      13      230       416     202            15
    ## Tigrinya                                                           283     162       44        77       0             0
    ## Ganda                                                              340      34       77       194       0            35
    ## Afrikaans                                                          612      33      284       268      12            15
    ## Kannada                                                            713      19       80       335     219            60
    ## Ilocano                                                            592      42        0         0     284           266
    ## Other Native North American languages                              819     113       13       102     449           142
    ## Shona                                                              142       0      142         0       0             0
    ## Other Eastern Malayo-Polynesian languages                          120      16       77        27       0             0
    ## Konkani                                                            106       0        0        47      59             0
    ## Latvian                                                            670      61      450        47     112             0
    ## Somali                                                             461       0      246       169      28            18
    ## Uto-Aztecan languages                                                0       0        0         0       0             0
    ## Kiowa-Tanoan languages (2016 or earlier)                             0       0        0         0       0             0
    ## Mongolian                                                          248       0      132        20      96             0
    ## Muskogean languages                                                 85       0       85         0       0             0
    ## Other Philippine languages                                         215       0       18        67     130             0
    ## Marshallese                                                         10       0        0        10       0             0
    ## Lao                                                                491     264       25        12     190             0
    ## Swiss German                                                       404      40      104       260       0             0
    ## Chamorro                                                            69       0        0         0      69             0
    ## Ojibwa                                                              59       0        0         0      59             0
    ## Chuukese                                                             0       0        0         0       0             0
    ## Hawaiian                                                            23       0       23         0       0             0
    ## Cajun French                                                         0       0        0         0       0             0
    ## Aleut languages                                                     21      21        0         0       0             0
    ## Pennsylvania German                                                231      53        0         0     178             0
    ## Samoan                                                             147       0        0        28     119             0
    ## Navajo                                                               0       0        0         0       0             0
    ## Chaldean Neo-Aramaic                                                 0       0        0         0       0             0
    ## Kabuverdianu                                                        58       0        9        49       0             0
    ## Kurdish                                                             68      60        8         0       0             0
    ## Assyrian Neo-Aramaic                                                62      27       13        22       0             0
    ## Hmong                                                              105      42        4         0      59             0
    ## Iu Mien                                                              0       0        0         0       0             0
    ## Cherokee                                                            40       0        0         0      40             0
    ## Apache languages                                                     0       0        0         0       0             0

- Proportions of languages against the total number of population

``` r
round(data.matrix(thisData)/rep(data.matrix(thisData.totalPopulation), lengths(thisData))*100,2)
```

    ##                                                          New.York.City Bronx Brooklyn Manhattan Queens Staten.Island
    ## Spanish                                                          23.48 47.24    14.72     21.02  23.27         10.66
    ## Chinese (incl. Mandarin, Cantonese, and Min Nan Chinese)          6.15  0.43     7.07      5.40   9.44          4.58
    ## Russian                                                           2.49  0.22     5.24      0.92   1.51          4.29
    ## Haitian                                                           1.21  0.36     2.46      0.25   1.23          0.15
    ## Bengali                                                           1.50  1.37     0.83      0.25   3.46          0.26
    ## Yiddish                                                           1.22  0.02     3.87      0.05   0.05          0.12
    ## French                                                            1.09  1.44     0.86      2.17   0.52          0.43
    ## Italian                                                           0.79  0.44     0.75      0.65   0.91          1.87
    ## Korean                                                            0.79  0.17     0.26      0.70   1.88          0.51
    ## Arabic                                                            0.90  0.52     1.32      0.39   0.79          1.98
    ## Polish                                                            0.64  0.03     0.61      0.24   1.18          1.36
    ## Hebrew                                                            0.54  0.07     0.93      0.60   0.32          0.65
    ## Urdu                                                              0.55  0.18     0.86      0.18   0.63          0.94
    ## Greek                                                             0.46  0.14     0.22      0.33   1.05          0.33
    ## Tagalog                                                           0.39  0.20     0.15      0.15   0.96          0.35
    ## Albanian                                                          0.44  0.67     0.29      0.12   0.38          1.92
    ## Hindi                                                             0.38  0.09     0.15      0.51   0.76          0.17
    ## Punjabi                                                           0.31  0.03     0.09      0.02   0.99          0.01
    ## Akan (incl. Twi)                                                  0.32  1.41     0.15      0.03   0.13          0.05
    ## Japanese                                                          0.26  0.03     0.18      0.61   0.29          0.02
    ## German                                                            0.22  0.06     0.21      0.47   0.19          0.06
    ## Portuguese                                                        0.24  0.07     0.17      0.44   0.32          0.05
    ## Filipino                                                          0.21  0.10     0.06      0.10   0.53          0.13
    ## Romanian                                                          0.16  0.02     0.05      0.24   0.33          0.01
    ## Turkish                                                           0.14  0.02     0.20      0.16   0.12          0.27
    ## Ukrainian                                                         0.15  0.03     0.31      0.05   0.08          0.26
    ## Vietnamese                                                        0.13  0.21     0.11      0.07   0.15          0.03
    ## Yoruba                                                            0.17  0.26     0.16      0.10   0.15          0.24
    ## Farsi                                                             0.11  0.02     0.08      0.14   0.18          0.06
    ## Gujarati                                                          0.11  0.03     0.04      0.10   0.25          0.07
    ## Malayalam                                                         0.09  0.01     0.03      0.03   0.16          0.41
    ## Fulah                                                             0.08  0.26     0.04      0.07   0.01          0.09
    ## Hungarian                                                         0.05  0.03     0.07      0.05   0.07          0.01
    ## Igbo                                                              0.08  0.24     0.03      0.01   0.07          0.07
    ## Nepali                                                            0.12  0.01     0.01      0.02   0.41          0.03
    ## Jamaican Creole English                                           0.10  0.25     0.10      0.02   0.08          0.02
    ## Manding languages                                                 0.09  0.32     0.01      0.13   0.00          0.03
    ## Thai                                                              0.08  0.01     0.02      0.04   0.21          0.03
    ## Croatian                                                          0.06  0.02     0.02      0.03   0.16          0.02
    ## Serbian                                                           0.07  0.02     0.05      0.07   0.12          0.03
    ## Other languages of Asia                                           0.11  0.00     0.30      0.01   0.07          0.02
    ## Wolof                                                             0.06  0.16     0.01      0.09   0.01          0.09
    ## Dutch                                                             0.06  0.02     0.05      0.11   0.06          0.02
    ## Other Mande languages                                             0.08  0.37     0.03      0.01   0.00          0.05
    ## Burmese                                                           0.08  0.00     0.05      0.01   0.20          0.08
    ## Armenian                                                          0.05  0.02     0.02      0.05   0.09          0.04
    ## Tamil                                                             0.05  0.05     0.02      0.07   0.07          0.12
    ## Pashto                                                            0.04  0.01     0.01      0.00   0.12          0.02
    ## Other and unspecified languages                                   0.10  0.01     0.19      0.02   0.08          0.18
    ## Bulgarian                                                         0.04  0.04     0.01      0.04   0.05          0.03
    ## Karen languages                                                   0.00  0.00     0.00      0.00   0.00          0.00
    ## Other Niger-Congo languages                                       0.05  0.15     0.03      0.02   0.03          0.08
    ## Bosnian                                                           0.04  0.03     0.02      0.01   0.09          0.01
    ## Serbocroatian                                                     0.02  0.01     0.01      0.02   0.03          0.09
    ## Swedish                                                           0.03  0.01     0.04      0.08   0.01          0.00
    ## Other languages of Africa                                         0.04  0.04     0.06      0.03   0.01          0.02
    ## Tibetan                                                           0.10  0.00     0.04      0.01   0.31          0.00
    ## Khmer                                                             0.02  0.06     0.01      0.00   0.03          0.00
    ## Amharic                                                           0.04  0.10     0.01      0.07   0.01          0.02
    ## India nec                                                         0.02  0.01     0.02      0.01   0.02          0.03
    ## Telugu                                                            0.03  0.01     0.02      0.05   0.03          0.01
    ## Indonesian                                                        0.04  0.01     0.01      0.03   0.12          0.00
    ## Macedonian                                                        0.02  0.01     0.00      0.04   0.01          0.07
    ## Sinhala                                                           0.03  0.00     0.00      0.00   0.03          0.35
    ## Pakistan nec                                                      0.02  0.00     0.03      0.00   0.04          0.00
    ## Other Indo-Iranian languages                                      0.04  0.00     0.07      0.02   0.04          0.02
    ## Other Indo-European languages                                     0.02  0.01     0.03      0.03   0.01          0.01
    ## Slovak                                                            0.02  0.01     0.02      0.01   0.02          0.03
    ## Edoid languages                                                   0.01  0.05     0.01      0.00   0.01          0.00
    ## Irish                                                             0.01  0.01     0.01      0.02   0.02          0.01
    ## Swahili                                                           0.01  0.03     0.01      0.01   0.01          0.00
    ## Other Afro-Asiatic languages                                      0.05  0.21     0.01      0.05   0.01          0.02
    ## Nilo-Saharan languages                                            0.00  0.00     0.00      0.01   0.00          0.00
    ## Danish                                                            0.01  0.00     0.02      0.03   0.01          0.00
    ## Dari                                                              0.00  0.00     0.00      0.00   0.01          0.00
    ## Chin languages                                                    0.00  0.00     0.01      0.00   0.00          0.00
    ## Cebuano                                                           0.02  0.02     0.01      0.02   0.04          0.01
    ## Other English-based Creole languages                              0.04  0.02     0.07      0.02   0.04          0.02
    ## Ga                                                                0.02  0.08     0.01      0.00   0.00          0.00
    ## Lithuanian                                                        0.02  0.01     0.03      0.01   0.03          0.00
    ## Marathi                                                           0.01  0.00     0.01      0.03   0.02          0.00
    ## Malay                                                             0.01  0.00     0.00      0.01   0.02          0.00
    ## Other Central and South American languages                        0.02  0.06     0.03      0.00   0.01          0.00
    ## Other Bantu languages                                             0.01  0.01     0.01      0.00   0.02          0.01
    ## Czech                                                             0.03  0.00     0.03      0.06   0.02          0.02
    ## Gbe languages                                                     0.02  0.08     0.00      0.01   0.00          0.01
    ## Norwegian                                                         0.01  0.00     0.02      0.03   0.00          0.01
    ## Finnish                                                           0.01  0.00     0.01      0.03   0.01          0.00
    ## Tigrinya                                                          0.00  0.01     0.00      0.00   0.00          0.00
    ## Ganda                                                             0.00  0.00     0.00      0.01   0.00          0.01
    ## Afrikaans                                                         0.01  0.00     0.01      0.02   0.00          0.00
    ## Kannada                                                           0.01  0.00     0.00      0.02   0.01          0.01
    ## Ilocano                                                           0.01  0.00     0.00      0.00   0.01          0.06
    ## Other Native North American languages                             0.01  0.01     0.00      0.01   0.02          0.03
    ## Shona                                                             0.00  0.00     0.01      0.00   0.00          0.00
    ## Other Eastern Malayo-Polynesian languages                         0.00  0.00     0.00      0.00   0.00          0.00
    ## Konkani                                                           0.00  0.00     0.00      0.00   0.00          0.00
    ## Latvian                                                           0.01  0.00     0.02      0.00   0.00          0.00
    ## Somali                                                            0.01  0.00     0.01      0.01   0.00          0.00
    ## Uto-Aztecan languages                                             0.00  0.00     0.00      0.00   0.00          0.00
    ## Kiowa-Tanoan languages (2016 or earlier)                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Mongolian                                                         0.00  0.00     0.01      0.00   0.00          0.00
    ## Muskogean languages                                               0.00  0.00     0.00      0.00   0.00          0.00
    ## Other Philippine languages                                        0.00  0.00     0.00      0.00   0.01          0.00
    ## Marshallese                                                       0.00  0.00     0.00      0.00   0.00          0.00
    ## Lao                                                               0.01  0.02     0.00      0.00   0.01          0.00
    ## Swiss German                                                      0.00  0.00     0.00      0.02   0.00          0.00
    ## Chamorro                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Ojibwa                                                            0.00  0.00     0.00      0.00   0.00          0.00
    ## Chuukese                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Hawaiian                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Cajun French                                                      0.00  0.00     0.00      0.00   0.00          0.00
    ## Aleut languages                                                   0.00  0.00     0.00      0.00   0.00          0.00
    ## Pennsylvania German                                               0.00  0.00     0.00      0.00   0.01          0.00
    ## Samoan                                                            0.00  0.00     0.00      0.00   0.01          0.00
    ## Navajo                                                            0.00  0.00     0.00      0.00   0.00          0.00
    ## Chaldean Neo-Aramaic                                              0.00  0.00     0.00      0.00   0.00          0.00
    ## Kabuverdianu                                                      0.00  0.00     0.00      0.00   0.00          0.00
    ## Kurdish                                                           0.00  0.00     0.00      0.00   0.00          0.00
    ## Assyrian Neo-Aramaic                                              0.00  0.00     0.00      0.00   0.00          0.00
    ## Hmong                                                             0.00  0.00     0.00      0.00   0.00          0.00
    ## Iu Mien                                                           0.00  0.00     0.00      0.00   0.00          0.00
    ## Cherokee                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Apache languages                                                  0.00  0.00     0.00      0.00   0.00          0.00

- Proportions of languages against the total number of people who speak
  languages other than English at home

``` r
round(prop.table(data.matrix(thisData),2)*100, 2) 
```

    ##                                                          New.York.City Bronx Brooklyn Manhattan Queens Staten.Island
    ## Spanish                                                          48.92 80.29    33.23     55.00  42.04         31.19
    ## Chinese (incl. Mandarin, Cantonese, and Min Nan Chinese)         12.81  0.73    15.98     14.12  17.05         13.40
    ## Russian                                                           5.18  0.37    11.83      2.41   2.73         12.56
    ## Haitian                                                           2.52  0.61     5.55      0.64   2.22          0.45
    ## Bengali                                                           3.12  2.33     1.88      0.65   6.25          0.77
    ## Yiddish                                                           2.55  0.03     8.74      0.12   0.09          0.35
    ## French                                                            2.28  2.44     1.95      5.67   0.93          1.25
    ## Italian                                                           1.64  0.75     1.70      1.69   1.64          5.48
    ## Korean                                                            1.65  0.28     0.58      1.84   3.40          1.49
    ## Arabic                                                            1.87  0.88     2.98      1.03   1.43          5.78
    ## Polish                                                            1.33  0.05     1.37      0.62   2.14          3.98
    ## Hebrew                                                            1.13  0.12     2.09      1.58   0.59          1.90
    ## Urdu                                                              1.16  0.31     1.94      0.46   1.14          2.74
    ## Greek                                                             0.96  0.23     0.49      0.87   1.90          0.97
    ## Tagalog                                                           0.81  0.33     0.33      0.40   1.74          1.02
    ## Albanian                                                          0.91  1.13     0.66      0.33   0.68          5.62
    ## Hindi                                                             0.79  0.15     0.35      1.34   1.37          0.51
    ## Punjabi                                                           0.64  0.05     0.21      0.04   1.79          0.04
    ## Akan (incl. Twi)                                                  0.67  2.39     0.34      0.07   0.23          0.14
    ## Japanese                                                          0.54  0.05     0.40      1.59   0.52          0.05
    ## German                                                            0.46  0.10     0.47      1.22   0.34          0.16
    ## Portuguese                                                        0.50  0.13     0.39      1.15   0.57          0.13
    ## Filipino                                                          0.44  0.18     0.15      0.25   0.96          0.39
    ## Romanian                                                          0.33  0.04     0.11      0.63   0.60          0.04
    ## Turkish                                                           0.30  0.04     0.44      0.41   0.22          0.78
    ## Ukrainian                                                         0.31  0.04     0.70      0.13   0.15          0.76
    ## Vietnamese                                                        0.27  0.36     0.26      0.17   0.28          0.09
    ## Yoruba                                                            0.34  0.44     0.37      0.25   0.26          0.69
    ## Farsi                                                             0.22  0.03     0.18      0.36   0.32          0.18
    ## Gujarati                                                          0.23  0.05     0.08      0.27   0.46          0.21
    ## Malayalam                                                         0.18  0.02     0.07      0.09   0.29          1.20
    ## Fulah                                                             0.16  0.45     0.09      0.19   0.02          0.27
    ## Hungarian                                                         0.11  0.05     0.15      0.13   0.12          0.03
    ## Igbo                                                              0.16  0.41     0.08      0.02   0.13          0.20
    ## Nepali                                                            0.26  0.01     0.03      0.06   0.74          0.08
    ## Jamaican Creole English                                           0.21  0.42     0.23      0.06   0.15          0.05
    ## Manding languages                                                 0.18  0.55     0.03      0.34   0.01          0.08
    ## Thai                                                              0.16  0.03     0.05      0.11   0.38          0.10
    ## Croatian                                                          0.12  0.03     0.03      0.08   0.29          0.06
    ## Serbian                                                           0.14  0.03     0.12      0.17   0.23          0.09
    ## Other languages of Asia                                           0.24  0.00     0.67      0.01   0.13          0.06
    ## Wolof                                                             0.12  0.28     0.03      0.24   0.02          0.26
    ## Dutch                                                             0.12  0.04     0.12      0.28   0.11          0.05
    ## Other Mande languages                                             0.16  0.64     0.06      0.04   0.00          0.13
    ## Burmese                                                           0.16  0.00     0.12      0.03   0.36          0.22
    ## Armenian                                                          0.10  0.03     0.05      0.13   0.17          0.10
    ## Tamil                                                             0.11  0.08     0.04      0.18   0.13          0.36
    ## Pashto                                                            0.08  0.02     0.03      0.00   0.22          0.07
    ## Other and unspecified languages                                   0.20  0.02     0.44      0.04   0.15          0.53
    ## Bulgarian                                                         0.08  0.07     0.03      0.11   0.10          0.09
    ## Karen languages                                                   0.00  0.00     0.00      0.01   0.01          0.00
    ## Other Niger-Congo languages                                       0.10  0.26     0.06      0.04   0.05          0.23
    ## Bosnian                                                           0.08  0.05     0.05      0.04   0.17          0.03
    ## Serbocroatian                                                     0.05  0.02     0.01      0.06   0.05          0.26
    ## Swedish                                                           0.06  0.01     0.08      0.20   0.01          0.01
    ## Other languages of Africa                                         0.08  0.07     0.14      0.09   0.03          0.06
    ## Tibetan                                                           0.21  0.00     0.09      0.02   0.56          0.00
    ## Khmer                                                             0.05  0.10     0.03      0.01   0.05          0.00
    ## Amharic                                                           0.08  0.16     0.03      0.19   0.02          0.06
    ## India nec                                                         0.03  0.02     0.03      0.02   0.04          0.08
    ## Telugu                                                            0.06  0.01     0.05      0.14   0.06          0.04
    ## Indonesian                                                        0.09  0.02     0.01      0.08   0.21          0.01
    ## Macedonian                                                        0.04  0.01     0.01      0.11   0.02          0.19
    ## Sinhala                                                           0.06  0.00     0.00      0.01   0.06          1.03
    ## Pakistan nec                                                      0.04  0.00     0.07      0.00   0.08          0.00
    ## Other Indo-Iranian languages                                      0.08  0.00     0.16      0.06   0.07          0.05
    ## Other Indo-European languages                                     0.04  0.01     0.07      0.09   0.02          0.03
    ## Slovak                                                            0.04  0.02     0.04      0.03   0.04          0.09
    ## Edoid languages                                                   0.03  0.08     0.03      0.00   0.02          0.00
    ## Irish                                                             0.03  0.02     0.02      0.05   0.04          0.02
    ## Swahili                                                           0.03  0.05     0.02      0.04   0.02          0.00
    ## Other Afro-Asiatic languages                                      0.11  0.36     0.03      0.13   0.02          0.05
    ## Nilo-Saharan languages                                            0.00  0.01     0.00      0.02   0.00          0.00
    ## Danish                                                            0.03  0.00     0.04      0.08   0.01          0.01
    ## Dari                                                              0.01  0.01     0.00      0.00   0.02          0.00
    ## Chin languages                                                    0.00  0.00     0.01      0.00   0.00          0.00
    ## Cebuano                                                           0.04  0.03     0.02      0.06   0.06          0.03
    ## Other English-based Creole languages                              0.08  0.03     0.17      0.04   0.07          0.07
    ## Ga                                                                0.04  0.13     0.03      0.00   0.01          0.00
    ## Lithuanian                                                        0.04  0.01     0.06      0.04   0.06          0.00
    ## Marathi                                                           0.03  0.00     0.02      0.07   0.03          0.00
    ## Malay                                                             0.02  0.00     0.01      0.03   0.03          0.00
    ## Other Central and South American languages                        0.05  0.09     0.07      0.01   0.02          0.01
    ## Other Bantu languages                                             0.03  0.02     0.01      0.01   0.05          0.03
    ## Czech                                                             0.06  0.00     0.07      0.15   0.04          0.06
    ## Gbe languages                                                     0.04  0.14     0.00      0.02   0.01          0.04
    ## Norwegian                                                         0.02  0.00     0.04      0.07   0.00          0.03
    ## Finnish                                                           0.02  0.00     0.02      0.07   0.02          0.01
    ## Tigrinya                                                          0.01  0.02     0.00      0.01   0.00          0.00
    ## Ganda                                                             0.01  0.00     0.01      0.03   0.00          0.02
    ## Afrikaans                                                         0.02  0.00     0.03      0.04   0.00          0.01
    ## Kannada                                                           0.02  0.00     0.01      0.06   0.02          0.04
    ## Ilocano                                                           0.02  0.01     0.00      0.00   0.02          0.17
    ## Other Native North American languages                             0.02  0.01     0.00      0.02   0.04          0.09
    ## Shona                                                             0.00  0.00     0.01      0.00   0.00          0.00
    ## Other Eastern Malayo-Polynesian languages                         0.00  0.00     0.01      0.00   0.00          0.00
    ## Konkani                                                           0.00  0.00     0.00      0.01   0.00          0.00
    ## Latvian                                                           0.02  0.01     0.04      0.01   0.01          0.00
    ## Somali                                                            0.01  0.00     0.02      0.03   0.00          0.01
    ## Uto-Aztecan languages                                             0.00  0.00     0.00      0.00   0.00          0.00
    ## Kiowa-Tanoan languages (2016 or earlier)                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Mongolian                                                         0.01  0.00     0.01      0.00   0.01          0.00
    ## Muskogean languages                                               0.00  0.00     0.01      0.00   0.00          0.00
    ## Other Philippine languages                                        0.01  0.00     0.00      0.01   0.01          0.00
    ## Marshallese                                                       0.00  0.00     0.00      0.00   0.00          0.00
    ## Lao                                                               0.01  0.03     0.00      0.00   0.02          0.00
    ## Swiss German                                                      0.01  0.00     0.01      0.04   0.00          0.00
    ## Chamorro                                                          0.00  0.00     0.00      0.00   0.01          0.00
    ## Ojibwa                                                            0.00  0.00     0.00      0.00   0.00          0.00
    ## Chuukese                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Hawaiian                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Cajun French                                                      0.00  0.00     0.00      0.00   0.00          0.00
    ## Aleut languages                                                   0.00  0.00     0.00      0.00   0.00          0.00
    ## Pennsylvania German                                               0.01  0.01     0.00      0.00   0.01          0.00
    ## Samoan                                                            0.00  0.00     0.00      0.00   0.01          0.00
    ## Navajo                                                            0.00  0.00     0.00      0.00   0.00          0.00
    ## Chaldean Neo-Aramaic                                              0.00  0.00     0.00      0.00   0.00          0.00
    ## Kabuverdianu                                                      0.00  0.00     0.00      0.01   0.00          0.00
    ## Kurdish                                                           0.00  0.01     0.00      0.00   0.00          0.00
    ## Assyrian Neo-Aramaic                                              0.00  0.00     0.00      0.00   0.00          0.00
    ## Hmong                                                             0.00  0.01     0.00      0.00   0.00          0.00
    ## Iu Mien                                                           0.00  0.00     0.00      0.00   0.00          0.00
    ## Cherokee                                                          0.00  0.00     0.00      0.00   0.00          0.00
    ## Apache languages                                                  0.00  0.00     0.00      0.00   0.00          0.00

