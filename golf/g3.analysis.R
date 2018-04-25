library(tidyverse)
# ## Descriptives for incomes
# income <- c("totinc_1988", "totinc_1989", "totinc_1990", "totinc_1992", 
#             "totinc_1993", "totinc_1994", "totinc_1996", "totinc_1998",
#             "totinc_2000", "totinc_2004", "totinc_2008", "totinc_2012")
# inc.desc <- echo[, income]
# library(psych)
# describe(inc.desc)
# ## descriptives for net worth
# wealth <- c("networth_1988", "networth_1989", "networth_1990", "networth_1992", 
#             "networth_1993", "networth_1994", "networth_1996", "networth_1998",
#             "networth_2000", "networth_2004", "networth_2008", "networth_2012")
# wealth.desc <- echo[, wealth]
# describe(wealth.desc)
# ## descriptives for inheritance
# gift <- c("value_1988", "value_1989", "value_1990", "value_1992", 
#             "value_1993", "value_1994", "value_1996", "value_1998",
#             "value_2000", "value_2004", "value_2008", "value_2012")
# gift.desc <- echo[, gift]
# describe(gift.desc)
# sum(echo$networth_1988[echo$networth_1988 == 0], na.rm = T)
# x <- 0
# ## histogramming wealth
# range(echo$networth_1988, na.rm = T)
# hist(echo$networth_1988, breaks = 150, xlim = c(-150000, 1000000))
# hist(echo$networth_1989)

## is value_X included in income_X?
# tail(table(echo$totinc_1989))
# tail(table(echo$value_1989))
# tail(table(echo$networth_1989))

## survey weights
library(survey)
s1 <- svydesign(ids = ~1, 
                strata = NULL,
                data = echo,
                weights = ~w1_2012)
svymean(~networth_1992, s1, na.rm = T)

# descriptive stats
svyquantile(~networth_1992, s1, c(0, .25,.5,.75, 1), ci = F, na.rm = T)
tail(table(echo$networth_1992))
sum(!is.na(echo$networth_1992))
sum(echo$negwealth_1992 == 1, na.rm  = T)
sum(echo$negwealth_1992 == 1, na.rm  = T) / sum(!is.na(echo$networth_1992))
sum(echo$nowealth_1992 == 1, na.rm = T)
sum(echo$nowealth_1992 == 1, na.rm = T) / sum(!is.na(echo$networth_1992))

## 2012
s12 <- svydesign(ids = ~1, 
                strata = NULL,
                data = echo,
                weights = ~w1_2012)
svymean(~networth_1992, s1, na.rm = T)
# model 1, whites
slm1 <- svyglm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
                 age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
                 reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s12, subset = race == "White")
summary(slm1)

# model 2, blacks
slm2 <- svyglm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
                 age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
                 reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s12, subset = race == "Black")
summary(slm2)

# model 3, hispanices
slm3 <- svyglm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
                 age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
                 reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s12, subset = race == "Hispanic")
summary(slm3)

library(stargazer)
stargazer(slm1, slm2, slm3, 
          title = "Predicting 2012 Net Worth",
          align = T,
          type = "latex")

summary(slm1)
summary(slm2)
summary(slm3)

# heteroskedasticity diagnostic, but does it take into consideration survey weights
library(lmtest)
bptest(slm1) ## present
bptest(slm2) ## present
bptest(slm3) ## not present

## robust standard errors
library(rms)


## 2008
s08 <- svydesign(ids = ~1, 
                strata = NULL,
                data = echo,
                weights = ~w1_2008)
svymean(~networth_1992, s1, na.rm = T)
# model 4, whites
slm4 <- svyglm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
                 age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
                 reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s08, subset = race == "White")
summary(slm4)

# model 5, blacks
slm5 <- svyglm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
                 age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
                 reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s08, subset = race == "Black")
summary(slm5)

# model 6, hispanices
slm6 <- svyglm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
                 age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
                 reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s08, subset = race == "Hispanic")
summary(slm6)

summary(slm4)
summary(slm5)
summary(slm6)

# heteroskedasticity diagnostic, but does it take into consideration survey weights
bptest(slm4) ## present
bptest(slm5) ## present
bptest(slm6) ## present


## 2004
s04 <- svydesign(ids = ~1, 
                strata = NULL,
                data = echo,
                weights = ~w1_2004)
svymean(~networth_1992, s1, na.rm = T)
# model 7, whites
slm7 <- svyglm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
                 age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
                 reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s04, subset = race == "White")
summary(slm7)

# model 8, blacks
slm8 <- svyglm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
                 age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
                 reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s04, subset = race == "Black")
summary(slm8)

# model 9, hispanices
slm9 <- svyglm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
                 age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
                 reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + 
                 parent_ed,
               design = s04, subset = race == "Hispanic")
summary(slm9)

summary(slm7)
summary(slm8)
summary(slm9)

# heteroskedasticity diagnostic, but does it take into consideration survey weights
bptest(slm7) ## present
bptest(slm8) ## present
bptest(slm9) ## present



# 1988
# 1989
# 1990
# 1992
# 1993
# 1994
# 1996
# 1998
# 2000
# 2004
# 2008
# 2012
# length(which(is.infinite(echo$ln_wealth_2012)))

## 2012 log-log model
lm1 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "White")
summary(lm1)

lm2 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Black")
summary(lm2)

lm3 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + hours_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Hispanic")
summary(lm3)

summary(lm1)
summary(lm2)
summary(lm3)

## heteroskedasticity diagnostic
bptest(lm1) ## very much so for whites
bptest(lm2) ## yes for blacks
bptest(lm3) ## not for hispanics


## 2008 log-log model
lm1 <-  lm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
             age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
             reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "White")
summary(lm1)

lm2 <-  lm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
             age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
             reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Black")
summary(lm2)

lm3 <-  lm(ln_wealth_2008 ~ ln_totinc_2008 + ln_value_2008 + ln_value_2004 +
             age_2008 + female + degree_2008 + hours_2008 + marstat_2008 + region_2008 + 
             reside_2008 + kidnum_2008 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Hispanic")
summary(lm3)

summary(lm1)
summary(lm2)
summary(lm3)



## 2004 log-log model
lm1 <-  lm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
             age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
             reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "White")
summary(lm1)

lm2 <-  lm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
             age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
             reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Black")
summary(lm2)

lm3 <-  lm(ln_wealth_2004 ~ ln_totinc_2004 + ln_value_2004 + ln_value_2000 +
             age_2004 + female + degree_2004 + hours_2004 + marstat_2004 + region_2004 + 
             reside_2004 + kidnum_2004 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Hispanic")
summary(lm3)

summary(lm1)
summary(lm2)
summary(lm3)
 
## log-level model >> MAKE THIS LOG-LOG FOR FUNCTIONAL FORM
## subset for race, interaction for everything
## 1st diff would be nice complement,
## look into taking off sign of net worth and adding it back on after logging
## how foten does a positive windfall lead to negative net worth
## test each log transformation model by themselves, add one,
## heteroskedasticity* & outliers/information for inheritances, repeating inheritance
## baseline inheritance information, sharing their frequency and characteristics

options(scipen = 999)
ggplot(echo, aes(x = value_2012)) +
  geom_histogram(binwidth = 15)



## how to find out whether someone received
# whites
e1 <- echo %>%
  filter(race == "White") %>% 
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e1[is.na(e1)] <- 0
e1$total <-  e1$gift_2000 + e1$gift_2004 + e1$gift_2008 + e1$gift_2012 

1471 / 6559 # received at least one inheritance 22%
524 / 1471 # likelihood of receiving a second inheritance after already receiving another one.
# 36

# blacks
e2 <- echo %>%
  filter(race == "Black") %>% 
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e2[is.na(e2)] <- 0
e2$total <-  e2$gift_2000 + e2$gift_2004 + e2$gift_2008 + e2$gift_2012 
table(e2$total)
361 / 2562 #14%
78 / 361 # 21%

# hispanic
e2 <- echo %>%
  filter(race == "Hispanic") %>% 
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e2[is.na(e2)] <- 0
e2$total <-  e2$gift_2000 + e2$gift_2004 + e2$gift_2008 + e2$gift_2012 
table(e2$total)
233 / 1924 # 12%
63 / 233 # 27%

sum(echo$gift_2012 == 1, na.rm = T) + sum(echo$gift_2008 == 1, na.rm = T) +  
  sum(echo$gift_2004 == 1, na.rm = T) +  sum(echo$gift_2000 == 1, na.rm = T)

