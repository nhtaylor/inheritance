# 1. PROVIDING ACCURATE COLUMN NAMES
colnames(categories)  <- c(
  ## 1979
  "id", "age_79", "ma_ed", "pa_ed", "sibnum_79", 
  "sample_id", "race", "sex",
  ## 1980
  "employ_1980",
  ## 1981
  "employ_1981",
  ## 1982
  "employ_1982",
  ## 1983
  "employ_1983",
  ## 1984
  "employ_1984",
  ## 1985
  "employ_1985",
  ## 1986
  "employ_1986",
  ## 1987
  "pa_month87", "pa_year87", "pa_age87", "ma_month87", "ma_year87", 
  "ma_age87", "employ_1987",
  ## 1988
  "pa_month88", "pa_year88", "pa_age88", "ma_month88", "ma_year88",
  "ma_age88", "degree_1988", "hours_1988", "gift_1988", "value_1988", 
  "networth_1988", "employ_1988", "totinc_1988", "region_1988", "marstat_1988",
  "reside_1988", "kidnum_1988",
  ## 1989
  "degree_1989", "hours_1989", "gift_1989", "value_1989", "networth_1989",  
  "employ_1989", "totinc_1989", "region_1989", "marstat_1989", "reside_1989",
  "kidnum_1989",
  ## 1990
  "degree_1990", "hours_1990", "gift_1990", "value_1990", "networth_1990", 
  "employ_1990", "totinc_1990", "region_1990", "marstat_1990", "reside_1990",
  "kidnum_1990",
  ## 1991
  "degree_1991", "gift_1991", "value_1991", 
  ## 1992
  "degree_1992", "hours_1992", "gift_1992", "value_1992", "networth_1992", 
  "employ_1992", "totinc_1992", "region_1992", "marstat_1992", "reside_1992",
  "kidnum_1992",
  ## 1993
  "degree_1993", "hours_1993", "employ_1993", "gift_1993", "value_1993",
  "networth_1993", "totinc_1993", "region_1993", "marstat_1993", "reside_1993",
  "kidnum_1993",
  ## 1994
  "degree_1994", "hours_1994", "employ_1994", "gift_1994", "value_1994", 
  "networth_1994", "totinc_1994", "region_1994", "marstat_1994", "reside_1994",
  "kidnum_1994",
  ## 1996
  "totinc_1996", "region_1996", "marstat_1996", "reside_1996", "kidnum_1996", 
  "degree_1996", "hours_1996", "bad_hours_96",  "employ_1996", "gift_1996", 
  "value_1996", "networth_1996",
  ## 1998
  "degree_1998", "hours_1998", "gift_1998", "value_1998", "networth_1998",
  "employ_1998", "totinc_1998", "region_1998", "marstat_1998", "reside_1998",
  "kidnum_1998",
  ## 2000
  "degree_2000", "hours_2000", "employ_2000", "networth_2000", "gift_2000",
  "value_2000", "totinc_2000", "region_2000", "marstat_2000", "reside_2000",
  "kidnum_2000",
  ## 2002
  "degree_2002", "gift_2002", "value_2002", 
  ## 2004
  "degree_2004", "hours_2004", "employ_2004", "gift_2004", "value_2004",
  "networth_2004", "totinc_2004", "region_2004", "marstat_2004", "reside_2004",
  "kidnum_2004",
  ## 2006
  "degree_2006", "gift_2006", "value_2006", 
  ## 2008
  "degree_2008", "hours_2008", "employ_2008", "gift_2008", "value_2008", 
  "networth_2008", "totinc_2008", "region_2008", "marstat_2008", "reside_2008",
  "kidnum_2008",
  ## 2010
  "degree_2010", "gift_2010", "value_2010",
  ## 2012
  "degree_2012", "hours_2012", "employ_2012", "gift_2012", "value_2012",
  "networth_2012", "totinc_2012", "region_2012", "marstat_2012", "reside_2012",
  "kidnum_2012"
)
echo <- categories[, order(colnames(categories))]


# 2. Cleaning one-use variables

## sex to FEMALE
table(echo$sex)
echo$sex <- echo$sex - 1
table(echo$sex)
colnames(echo)[which(names(echo) == "sex")] <- "female"
table(echo$female)
echo <- echo[, order(colnames(echo))]

## PARENTAL EDUCATION
# mother education
table(echo$ma_ed)
echo$ma_ed <- ifelse(echo$ma_ed < 12, 0,
              ifelse(echo$ma_ed == 12, 1,
              ifelse(echo$ma_ed > 12 & echo$ma_ed < 16, 2,
              ifelse(echo$ma_ed == 16 | echo$ma_ed == 17, 3, 
              ifelse(echo$ma_ed > 17 & echo$ma_ed <= 21, 4,
                NA)))))
table(echo$ma_ed)
# father education
table(echo$pa_ed)
echo$pa_ed <- ifelse(echo$pa_ed < 12, 0,
              ifelse(echo$pa_ed == 12, 1,
              ifelse(echo$pa_ed > 12 & echo$pa_ed < 16, 2,
              ifelse(echo$pa_ed == 16 | echo$pa_ed == 17, 3, 
              ifelse(echo$pa_ed > 17 & echo$pa_ed <= 20, 4,
                NA)))))
table(echo$pa_ed)
## parental education
echo$parent_ed <- ifelse(echo$ma_ed >= echo$pa_ed, echo$ma_ed,
                  ifelse(echo$pa_ed > echo$ma_ed, echo$pa_ed,
                  ifelse(is.na(echo$ma_ed) & is.na(echo$pa_ed), NA,
                    7)))
table(echo$parent_ed)

## AGE for all time periods
echo$age_1988 <- echo$age_79 + 9
echo$age_1989 <- echo$age_79 + 10
echo$age_1990 <- echo$age_79 + 11
echo$age_1992 <- echo$age_79 + 13
echo$age_1993 <- echo$age_79 + 14
echo$age_1994 <- echo$age_79 + 15
echo$age_1996 <- echo$age_79 + 17
echo$age_1998 <- echo$age_79 + 19
echo$age_2000 <- echo$age_79 + 21
echo$age_2004 <- echo$age_79 + 25
echo$age_2008 <- echo$age_79 + 29
echo$age_2012 <- echo$age_79 + 33
 
## PARENTAL AGE 
# father's age in '88
sum(is.na(echo$pa_age87))
echo$pa_age87 <- ifelse(!is.na(echo$pa_age87), echo$pa_age87,
                 ifelse(is.na(echo$pa_age87), 87 - echo$pa_year87, 
                        NA))
sum(is.na(echo$pa_age87))
echo$pa_age87 <- ifelse(!is.na(echo$pa_age87), echo$pa_age87,
                 ifelse(is.na(echo$pa_age87), 87 - echo$pa_year88,
                        NA))
sum(is.na(echo$pa_age87))
echo$pa_age87 <- ifelse(!is.na(echo$pa_age87), echo$pa_age87,
                 ifelse(is.na(echo$pa_age87), echo$pa_age88 - 1, 
                        NA))
sum(is.na(echo$pa_age87))
table(echo$pa_age87)
echo$pa_age88 <- echo$pa_age87 + 1
# mother's age in '88
sum(is.na(echo$ma_age87))
echo$ma_age87 <- ifelse(!is.na(echo$ma_age87), echo$ma_age87,
                 ifelse(is.na(echo$ma_age87), 87 - echo$ma_year87, 
                               NA))
sum(is.na(echo$ma_age87))
echo$ma_age87 <- ifelse(!is.na(echo$ma_age87), echo$ma_age87,
                 ifelse(is.na(echo$ma_age87), 87 - echo$ma_year88,
                               NA))
sum(is.na(echo$ma_age87))
echo$ma_age87 <- ifelse(!is.na(echo$ma_age87), echo$ma_age87,
                 ifelse(is.na(echo$ma_age87), echo$ma_age88 - 1, 
                               NA))
sum(is.na(echo$ma_age87))
table(echo$ma_age87)
echo$ma_age88 <- echo$ma_age87 + 1
## parental age (avg > mom > parent > na) in '88
echo$rental_age88 <- ifelse(!is.na(echo$ma_age88) & !is.na(echo$pa_age88), 
                               ((echo$ma_age88 + echo$pa_age88) / 2),
                        ifelse(is.na(echo$ma_age88) & !is.na(echo$pa_age88), 
                               echo$pa_age88,
                        ifelse(!is.na(echo$ma_age88) & is.na(echo$pa_age88),
                               echo$ma_age88,
                          NA)))

## EDUCATION
# swapping na's for zeros
echo$degree_1988[is.na(echo$degree_1988)] <- as.numeric(0)
echo$degree_1989[is.na(echo$degree_1989)] <- as.numeric(0)
echo$degree_1990[is.na(echo$degree_1990)] <- as.numeric(0)
echo$degree_1991[is.na(echo$degree_1991)] <- as.numeric(0)
echo$degree_1992[is.na(echo$degree_1992)] <- as.numeric(0)
echo$degree_1993[is.na(echo$degree_1993)] <- as.numeric(0)
echo$degree_1994[is.na(echo$degree_1994)] <- as.numeric(0)
echo$degree_1996[is.na(echo$degree_1996)] <- as.numeric(0)
echo$degree_1998[is.na(echo$degree_1998)] <- as.numeric(0)
echo$degree_2000[is.na(echo$degree_2000)] <- as.numeric(0)
echo$degree_2002[is.na(echo$degree_2002)] <- as.numeric(0)
echo$degree_2004[is.na(echo$degree_2004)] <- as.numeric(0)
echo$degree_2006[is.na(echo$degree_2006)] <- as.numeric(0)
echo$degree_2008[is.na(echo$degree_2008)] <- as.numeric(0)
echo$degree_2010[is.na(echo$degree_2010)] <- as.numeric(0)
echo$degree_2012[is.na(echo$degree_2012)] <- as.numeric(0)

## EDUCATION
# all years in same categories of education
# 0 - less than hs diploma
# 1 - hs diploma
# 2 - some college
# 3 - bachelor's degree
# 4 - advanced degree
## FIRST PART SHIFT EACH VARIABLE TO NEW SCALE
## THEN CODE UPDATES TO TOTAL EDUCATION ATTAINMENT & NOT DIFFERENCES

# 1988
table(echo$degree_1988)
echo$degree_1988 <- ifelse(echo$degree_1988 == 1, 1,
                    ifelse(echo$degree_1988 == 2, 2,
                    ifelse(echo$degree_1988 == 3 | echo$degree_1988 == 4, 3,
                    ifelse(echo$degree_1988 >= 5, 4,
                      0))))
table(echo$degree_1988)

# 1989
table(echo$degree_1989)
echo$degree_1989 <- ifelse(echo$degree_1989 == 1, 1,
                    ifelse(echo$degree_1989 == 2, 2,
                    ifelse(echo$degree_1989 == 3 | echo$degree_1989 == 4, 3,
                    ifelse(echo$degree_1989 >= 5, 4,
                      0))))
table(echo$degree_1989)
echo$degree_1989 <- ifelse(echo$degree_1989 > echo$degree_1988, echo$degree_1989,
                           echo$degree_1988)
table(echo$degree_1989)


# 1990
table(echo$degree_1990)
echo$degree_1990 <- ifelse(echo$degree_1990 == 1, 1,
                    ifelse(echo$degree_1990 == 2, 2,
                    ifelse(echo$degree_1990 == 3 | echo$degree_1990 == 4, 3,
                    ifelse(echo$degree_1990 >= 5, 4,
                      0))))
table(echo$degree_1990)
echo$degree_1990 <- ifelse(echo$degree_1990 > echo$degree_1989, echo$degree_1990,
                           echo$degree_1989)
table(echo$degree_1990)

# 1991
table(echo$degree_1991)
echo$degree_1991 <- ifelse(echo$degree_1991 == 1, 1,
                           ifelse(echo$degree_1991 == 2, 2,
                                  ifelse(echo$degree_1991 == 3 | echo$degree_1991 == 4, 3,
                                         ifelse(echo$degree_1991 >= 5, 4,
                                                0))))
table(echo$degree_1991)
echo$degree_1991 <- ifelse(echo$degree_1991 > echo$degree_1990, echo$degree_1991,
                           echo$degree_1990)
table(echo$degree_1991)

# 1992
table(echo$degree_1992)
echo$degree_1992 <- ifelse(echo$degree_1992 == 1, 1,
                    ifelse(echo$degree_1992 == 2, 2,
                    ifelse(echo$degree_1992 == 3 | echo$degree_1992 == 4, 3,
                    ifelse(echo$degree_1992 >= 5, 4,
                       0))))
table(echo$degree_1992)
echo$degree_1992 <- ifelse(echo$degree_1992 > echo$degree_1991, echo$degree_1992,
                           echo$degree_1991)
table(echo$degree_1992)

# 1993
table(echo$degree_1993)
echo$degree_1993 <- ifelse(echo$degree_1993 == 1, 1,
                    ifelse(echo$degree_1993 == 2, 2,
                    ifelse(echo$degree_1993 == 3 | echo$degree_1993 == 4, 3,
                    ifelse(echo$degree_1993 >= 5, 4,
                      0))))
table(echo$degree_1993)
echo$degree_1993 <- ifelse(echo$degree_1993 > echo$degree_1992, echo$degree_1993,
                           echo$degree_1992)
table(echo$degree_1993)

# 1994
table(echo$degree_1994)
echo$degree_1994 <- ifelse(echo$degree_1994 == 1, 1,
                    ifelse(echo$degree_1994 == 2, 2,
                    ifelse(echo$degree_1994 == 3 | echo$degree_1994 == 4, 3,
                    ifelse(echo$degree_1994 >= 5, 4,
                      0))))
table(echo$degree_1994)
echo$degree_1994 <- ifelse(echo$degree_1994 > echo$degree_1993, echo$degree_1994,
                           echo$degree_1993)
table(echo$degree_1994)

# 1996
table(echo$degree_1996)
echo$degree_1996 <- ifelse(echo$degree_1996 == 1, 1,
                    ifelse(echo$degree_1996 == 2, 2,
                    ifelse(echo$degree_1996 == 3 | echo$degree_1996 == 4, 3,
                    ifelse(echo$degree_1996 >= 5, 4,
                      0))))
table(echo$degree_1996)
echo$degree_1996 <- ifelse(echo$degree_1996 > echo$degree_1994, echo$degree_1996,
                           echo$degree_1994)
table(echo$degree_1996)

# 1998
table(echo$degree_1998)
echo$degree_1998 <- ifelse(echo$degree_1998 == 1, 1,
                    ifelse(echo$degree_1998 == 2, 2,
                    ifelse(echo$degree_1998 == 3 | echo$degree_1998 == 4, 3,
                    ifelse(echo$degree_1998 >= 5, 4,
                      0))))
table(echo$degree_1998)
echo$degree_1998 <- ifelse(echo$degree_1998 > echo$degree_1996, echo$degree_1998,
                           echo$degree_1996)
table(echo$degree_1998)

# 2000
table(echo$degree_2000)
echo$degree_2000 <- ifelse(echo$degree_2000 == 1, 1,
                    ifelse(echo$degree_2000 == 2, 2,
                    ifelse(echo$degree_2000 == 3 | echo$degree_2000 == 4, 3,
                    ifelse(echo$degree_2000 >= 5, 4,
                      0))))
table(echo$degree_2000)
echo$degree_2000 <- ifelse(echo$degree_2000 > echo$degree_1998, echo$degree_2000,
                           echo$degree_1998)
table(echo$degree_2000)

# 2002
table(echo$degree_2002)
echo$degree_2002 <- ifelse(echo$degree_2002 == 1, 1,
                    ifelse(echo$degree_2002 == 2, 2,
                    ifelse(echo$degree_2002 == 3 | echo$degree_2002 == 4, 3,
                    ifelse(echo$degree_2002 >= 5, 4,
                      0))))
table(echo$degree_2002)
echo$degree_2002 <- ifelse(echo$degree_2002 > echo$degree_2000, echo$degree_2002,
                           echo$degree_2000)
table(echo$degree_2002)

# 2004
table(echo$degree_2004)
echo$degree_2004 <- ifelse(echo$degree_2004 == 1, 1,
                    ifelse(echo$degree_2004 == 2, 2,
                    ifelse(echo$degree_2004 == 3 | echo$degree_2004 == 4, 3,
                    ifelse(echo$degree_2004 >= 5, 4,
                      0))))
table(echo$degree_2004)
echo$degree_2004 <- ifelse(echo$degree_2004 > echo$degree_2002, echo$degree_2004,
                           echo$degree_2002)
table(echo$degree_2004)

# 2006
table(echo$degree_2006)
echo$degree_2006 <- ifelse(echo$degree_2006 == 1, 1,
                           ifelse(echo$degree_2006 == 2, 2,
                                  ifelse(echo$degree_2006 == 3 | echo$degree_2006 == 4, 3,
                                         ifelse(echo$degree_2006 >= 5, 4,
                                                0))))
table(echo$degree_2006)
echo$degree_2006 <- ifelse(echo$degree_2006 > echo$degree_2004, echo$degree_2006,
                           echo$degree_2004)
table(echo$degree_2006)

# 2008
table(echo$degree_2008)
echo$degree_2008 <- ifelse(echo$degree_2008 == 1, 1,
                    ifelse(echo$degree_2008 == 2, 2,
                    ifelse(echo$degree_2008 == 3 | echo$degree_2008 == 4, 3,
                    ifelse(echo$degree_2008 >= 5, 4,
                      0))))
table(echo$degree_2008)
echo$degree_2008 <- ifelse(echo$degree_2008 > echo$degree_2006, echo$degree_2008,
                           echo$degree_2006)
table(echo$degree_2008)

# 2010
table(echo$degree_2010)
echo$degree_2010 <- ifelse(echo$degree_2010 == 1, 1,
                    ifelse(echo$degree_2010 == 2, 2,
                    ifelse(echo$degree_2010 == 3 | echo$degree_2010 == 4, 3,
                    ifelse(echo$degree_2010 >= 5, 4,
                      0))))
table(echo$degree_2010)
echo$degree_2010 <- ifelse(echo$degree_2010 > echo$degree_2008, echo$degree_2010,
                           echo$degree_2008)
table(echo$degree_2010)

# 2012
table(echo$degree_2012)
echo$degree_2012 <- ifelse(echo$degree_2012 == 1, 1,
                    ifelse(echo$degree_2012 == 2, 2,
                    ifelse(echo$degree_2012 == 3 | echo$degree_2012 == 4, 3,
                    ifelse(echo$degree_2012 >= 5, 4,
                      0))))
table(echo$degree_2012)
echo$degree_2012 <- ifelse(echo$degree_2012 > echo$degree_2010, echo$degree_2012,
                           echo$degree_2010)
table(echo$degree_2012)

## INHERITANCE VALUES
# swapping na's for zeros
echo$value_1988[is.na(echo$value_1988)] <- as.numeric(0)
echo$value_1989[is.na(echo$value_1989)] <- as.numeric(0)
echo$value_1990[is.na(echo$value_1990)] <- as.numeric(0)
echo$value_1992[is.na(echo$value_1992)] <- as.numeric(0)
echo$value_1993[is.na(echo$value_1993)] <- as.numeric(0)
echo$value_1994[is.na(echo$value_1994)] <- as.numeric(0)
echo$value_1996[is.na(echo$value_1996)] <- as.numeric(0)
echo$value_1998[is.na(echo$value_1998)] <- as.numeric(0)
echo$value_2000[is.na(echo$value_2000)] <- as.numeric(0)
echo$value_2002[is.na(echo$value_2002)] <- as.numeric(0)
echo$value_2004[is.na(echo$value_2004)] <- as.numeric(0)
echo$value_2006[is.na(echo$value_2006)] <- as.numeric(0)
echo$value_2008[is.na(echo$value_2008)] <- as.numeric(0)
echo$value_2010[is.na(echo$value_2010)] <- as.numeric(0)
echo$value_2012[is.na(echo$value_2012)] <- as.numeric(0)

## missing inheritance values in '91, 02, '06, and '10
# 1991 > 1992
table(echo$gift_1992)
echo$gift_1992 <- ifelse(echo$gift_1992 == 1 & is.na(echo$gift_1991), 1,
                  ifelse(echo$gift_1991 == 1 & is.na(echo$gift_1992), 1,
                  ifelse(echo$gift_1991 == 0 & echo$gift_1992 == 1, 1,
                  ifelse(echo$gift_1991 == 1 & echo$gift_1992 == 0, 1,
                  ifelse(echo$gift_1991 == 1 & echo$gift_1992 == 1, 1,
                  ifelse(echo$gift_1992 == 0 & echo$gift_1991 == 0, 0, 
                    0))))))
table(echo$gift_1992)
echo$value_1992 <- echo$value_1991 + echo$value_1992

# 2002 > 2004
table(echo$gift_2004)
echo$gift_2004 <- ifelse(echo$gift_2004 == 1 & is.na(echo$gift_2002), 1,
                  ifelse(echo$gift_2002 == 1 & is.na(echo$gift_2004), 1,
                  ifelse(echo$gift_2002 == 0 & echo$gift_2004 == 1, 1,
                  ifelse(echo$gift_2002 == 1 & echo$gift_2004 == 0, 1,
                  ifelse(echo$gift_2002 == 1 & echo$gift_2004 == 1, 1,
                  ifelse(echo$gift_2004 == 0 & echo$gift_2002 == 0, 0, 
                    0))))))
table(echo$gift_2004)
echo$value_2004 <- echo$value_2002 + echo$value_2004

## 2006 > 2008
table(echo$gift_2008)
echo$gift_2004 <- ifelse(echo$gift_2004 == 1 & is.na(echo$gift_2002), 1,
                  ifelse(echo$gift_2002 == 1 & is.na(echo$gift_2004), 1,
                  ifelse(echo$gift_2002 == 0 & echo$gift_2004 == 1, 1,
                  ifelse(echo$gift_2002 == 1 & echo$gift_2004 == 0, 1,
                  ifelse(echo$gift_2002 == 1 & echo$gift_2004 == 1, 1,
                  ifelse(echo$gift_2004 == 0 & echo$gift_2002 == 0, 0, 
                    0))))))
table(echo$gift_2008)
echo$value_2004 <- echo$value_2002 + echo$value_2004

## 2010 > 2012
table(echo$gift_2012)
echo$gift_2012 <- ifelse(echo$gift_2012 == 1 & is.na(echo$gift_2010), 1,
                  ifelse(echo$gift_2010 == 1 & is.na(echo$gift_2012), 1,
                  ifelse(echo$gift_2010 == 0 & echo$gift_2012 == 1, 1,
                  ifelse(echo$gift_2010 == 1 & echo$gift_2012 == 0, 1,
                  ifelse(echo$gift_2010 == 1 & echo$gift_2012 == 1, 1,
                  ifelse(echo$gift_2012 == 0 & echo$gift_2010 == 0, 0, 
                    0))))))
table(echo$gift_2012)
echo$value_2012 <- echo$value_2010 + echo$value_2012

### NEEDS WORK
## EMPLOY to self-employment
# echo$employ_1988 <- ifelse(echo$employ_1988 == 3, 1, 0)
# echo$employ_1989 <-
# echo$employ_1990 <-
# echo$employ_1992 <-
# echo$employ_1993 <-
# echo$employ_1994 <-
# echo$employ_1996 <-
# echo$employ_1998 <-
# echo$employ_2000 <-
# echo$employ_2004 <-
# echo$employ_2008 <-
# echo$employ_2012 <- )

### NEED WORKS
## FULL TIME EMPLOYMENT
# echo$hours_2012 <- )

## RURAL-URBAN RESIDENCE


## NUMBER OF KIDS
# good to go!
  
## REGION
# also good to go!
  
## 3. Checking NA'S 
# nac <- sapply(delta, function(y) sum(length(which(is.na(y)))))
# nac <- data.frame(nac)
# nac

## removing na's based on 2012 net worth (whole sample present) -- no longer beneficial
## d <- echo[!is.na(echo$kidnum_2012), ]
## dim(d)

## checking na's again
# na_fix <-sapply(d, function(y) sum(length(which(is.na(y)))))
# na_fix <- data.frame(na_fix)
# na_fix


## INFLATION, in 2012 dollaz
# 1988
echo$networth_1988 <- echo$networth_1988 * 1.941
echo$totinc_1988 <- echo$totinc_1988 * 1.941
echo$value_1988 <- echo$value_1988 * 1.941
# 1989
echo$networth_1989 <- echo$networth_1989 * 1.852
echo$totinc_1989 <- echo$totinc_1989 * 1.852
echo$value_1989 <- echo$value_1989 * 1.852
# 1990
echo$networth_1990 <- echo$networth_1990 * 1.757
echo$totinc_1990 <- echo$totinc_1990 * 1.757
echo$value_1990 <- echo$value_1990 * 1.757
# 1992
echo$networth_1992 <- echo$networth_1992 * 1.636
echo$totinc_1992 <- echo$totinc_1992 * 1.636
echo$value_1992 <- echo$value_1992 * 1.636
# 1993
echo$networth_1993 <- echo$networth_1993 * 1.589
echo$totinc_1993 <- echo$totinc_1993 * 1.589
echo$value_1993 <- echo$value_1993 * 1.589
# 1994
echo$networth_1994 <- echo$networth_1994 * 1.549
echo$totinc_1994 <- echo$totinc_1994 * 1.549
echo$value_1994 <- echo$value_1994 * 1.549
# 1996
echo$networth_1996 <- echo$networth_1996 * 1.463
echo$totinc_1996 <- echo$totinc_1996 * 1.463
echo$value_1996 <- echo$value_1996 * 1.463
# 1998
echo$networth_1998 <- echo$networth_1998 * 1.409
echo$totinc_1998 <- echo$totinc_1998 * 1.409
echo$value_1998 <- echo$value_1998 * 1.409
# 2000
echo$networth_2000 <- echo$networth_2000 * 1.333
echo$totinc_2000 <- echo$totinc_2000 * 1.333
echo$value_2000 <- echo$value_2000 * 1.333
# 2004
echo$networth_2004 <- echo$networth_2004 * 1.215
echo$totinc_2004 <- echo$totinc_2004 * 1.215
echo$value_2004 <- echo$value_2004 * 1.215
# 2008
echo$networth_2008 <- echo$networth_2008 * 1.066
echo$totinc_2008 <- echo$totinc_2008 * 1.066
echo$value_2008 <- echo$value_2008 * 1.066
# 2012 variables don't need adjusting to their own dollar value


## 2.b Subsetting my overall sample

## Remove military folks
# echo <- echo[!(echo$sample_id == "MIL MALE BLACK" |
#                echo$sample_id == "MIL FEMALE BLACK" |
#                echo$sample_id == "MIL MALE WHITE" |
#                echo$sample_id == "MIL FEMALE WHITE" |
#                echo$sample_id == "MIL MALE HISPANIC" |
#                echo$sample_id == "MIL FEMALE HISPANIC"), ]


## 3. turning wide to long format
library(tidyverse)
# dts <- d_fix %>%
#   gather(variable, value, -id, -ma_age87, -pa_age87, -ma_ed, -pa_ed, -race, 
#          -sample_id, -sex, -sibnum_79, -age_79) %>%
#   mutate(year = parse_number(variable) ) %>%
#   mutate(variable = gsub("_\\d\\d\\d\\d", "", x = variable) ) %>%
#   spread(variable, value)
# View(dts)
# 
# options(scipen = 999)
# m1 <-lm(networth_2012 ~ totinc_2012 + value_2012, data = echo)
# summary(m1)
# table(dts$totinc012)

