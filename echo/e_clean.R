# 1. PROVIDING ACCURATE COLUMN NAMES
colnames(categories)  <- c(
  ## 1979 & others
  "id", "age_79", "ma_ed", "pa_ed",
  "sibnum_79", "sample_id", "race", "sex",
  ## 1987
  "pa_month87", "pa_year87", "pa_age87", "ma_month87", "ma_year87", "ma_age87",
  ## 1988
  "pa_month88", "pa_year88", "pa_age88", "ma_month88", "ma_year88",
  "ma_age88", "degree_1988", "gift_1988", "value_1988", "networth_1988",
  "hours_1988", "employ_1988", "totinc_1988", "region_1988", "marstat_1988",
  "reside_1988", "kidnum_1988",
  ## 1989
  "degree_1989", "gift_1989", "value_1989", "networth_1989", "hours_1989", 
  "employ_1989", "totinc_1989", "region_1989", "marstat_1989", "reside_1989",
  "kidnum_1989",
  ## 1990
  "degree_1990", "gift_1990", "value_1990", "networth_1990", "hours_1990", 
  "employ_1990", "totinc_1990", "region_1990", "marstat_1990", "reside_1990",
  "kidnum_1990",
  ## 1992
  "degree_1992", "gift_1992", "value_1992", "networth_1992", "hours_1992", 
  "employ_1992", "totinc_1992", "region_1992", "marstat_1992", "reside_1992",
  "kidnum_1992",
  ## 1993
  "degree_1993", "hours_1993", "employ_1993", "gift_1993", "value_1993",
  "networth_1993",  "totinc_1993", "region_1993", "marstat_1993", "reside_1993",
  "kidnum_1993",
  ## 1994
  "degree_1994", "hours_1994", "employ_1994", "gift_1994", "value_1994", 
  "networth_1994", "totinc_1994", "region_1994", "marstat_1994", "reside_1984",
  "kidnum_1994",
  ## 1996
  "totinc_1996", "region_1996", "marstat_1996", "reside_1996", "kidnum_1996", 
  "degree_1996", "hours_1996", "employ_1996", "gift_1996", "value_1996", 
  "networth_1996",
  ## 1998
  "degree_1998", "hours_1998", "gift_1998", "value_1998", "networth_1998",
  "employ_1998", "totinc_1998", "region_1998", "marstat_1998", "reside_1998",
  "kidnum_1998",
  ## 2000
  "degree_2000", "hours_2000", "employ_2000", "networth_2000", "gift_2000",
  "value_2000", "totinc_2000", "region_2000", "marstat_2000", "reside_2000",
  "kidnum_2000",
  ## 2004
  "degree_2004", "hours_2004", "employ_2004", "gift_2004", "value_2004",
  "networth_2004", "totinc_2004", "region_2004", "marstat_2004", "reside_2004",
  "kidnum_2004",
  ## 2008
  "degree_2008", "hours_2008", "employ_2008", "gift_2008", "value_2008", 
  "networth_2008", "totinc_2008", "region_2008", "marstat_2008", "reside_2008",
  "kidnum_2008",
  ## 2012
  "degree_2012", "hours_2012", "employ_2012", "gift_2012", "value_2012",
  "networth_2012", "totinc_2012", "region_2012", "marstat_2012", "reside_2012",
  "kidnum_2012"
)
echo <- categories[, order(colnames(categories))]

# 2. Cleaning one-use variables

## sex to female
table(echo$sex)
echo$sex <- echo$sex - 1
table(echo$sex)
colnames(echo)[which(names(echo) == "sex")] <- "female"
table(echo$female)
echo <- echo[, order(colnames(echo))]

## parental education
table(echo$ma_ed)
echo$ma_ed <- ifelse(echo$ma_ed < 12, 0,
              ifelse(echo$ma_ed == 12, 1,
              ifelse(echo$ma_ed > 12 & echo$ma_ed < 16, 2,
              ifelse(echo$ma_ed == 16 | echo$ma_ed == 17, 3, 
              ifelse(echo$ma_ed > 17 & echo$ma_ed <= 21, 4,
                NA)))))
table(echo$ma_ed)
## father education
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

## 3. Checking NA'S 
nac <- sapply(delta, function(y) sum(length(which(is.na(y)))))
nac <- data.frame(nac)
nac

## removing na's based on 2012 net worth (whole sample present) -- no longer beneficial
# d <- delta[!is.na(d$kidnum_2012), ]
# dim(d)

## checking na's again
# na_fix <-sapply(d, function(y) sum(length(which(is.na(y)))))
# na_fix <- data.frame(na_fix)
# na_fix

## 3. turning wide to long format
library(tidyverse)
dts <- d_fix %>%
  gather(variable, value, -id, -ma_age87, -pa_age87, -ma_ed, -pa_ed, -race, 
         -sample_id, -sex, -sibnum_79, -age_79, -version) %>%
  mutate(year = parse_number(variable) ) %>%
  mutate(variable = gsub("_\\d\\d\\d\\d", "", x = variable) ) %>%
  spread(variable, value)
View(dts)

options(scipen = 999)
m1 <-lm(networth_2012 ~ totinc_2012 + value_2012, data = echo)
summary(m1)
table(dts$totinc012)

