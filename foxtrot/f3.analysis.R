library(tidyverse)
## Descriptives for incomes
income <- c("totinc_1988", "totinc_1989", "totinc_1990", "totinc_1992", 
            "totinc_1993", "totinc_1994", "totinc_1996", "totinc_1998",
            "totinc_2000", "totinc_2004", "totinc_2008", "totinc_2012")
inc.desc <- echo[, income]
library(psych)
describe(inc.desc)

## descriptives for net worth
wealth <- c("networth_1988", "networth_1989", "networth_1990", "networth_1992", 
            "networth_1993", "networth_1994", "networth_1996", "networth_1998",
            "networth_2000", "networth_2004", "networth_2008", "networth_2012")
wealth.desc <- echo[, wealth]
describe(wealth.desc)

## descriptives for inheritance
gift <- c("value_1988", "value_1989", "value_1990", "value_1992", 
            "value_1993", "value_1994", "value_1996", "value_1998",
            "value_2000", "value_2004", "value_2008", "value_2012")
gift.desc <- echo[, gift]
describe(gift.desc)

sum(echo$networth_1988[echo$networth_1988 == 0], na.rm = T)
x <- 0

## histogramming wealth
range(echo$networth_1988, na.rm = T)
hist(echo$networth_1988, breaks = 150, xlim = c(-150000, 1000000))
hist(echo$networth_1989)


## is value_X included in income_X?
tail(table(echo$totinc_1989))
tail(table(echo$value_1989))
tail(table(echo$networth_1989))

length(which(is.Inf(echo$ln_wealth_2012)))

## log-log model for $ variables
lm1 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "White")
summary(lm1)

lm2 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Black")
summary(lm2)

lm3 <-  lm(ln_wealth_2012 ~ ln_totinc_2012 + ln_value_2012 + ln_value_2008 +
             age_2012 + female + degree_2012 + marstat_2012 + region_2012 + 
             reside_2012 + kidnum_2012 + sibnum_79 + rental_age88 + parent_ed,
           data = echo, subset = race == "Hispanic")
summary(lm3)




# ## traditional lm
# lm1 <- lm(networth_2012 ~ totinc_2012 + value_2012 + age_2012 + female +
#          relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#          sibnum_79 + rental_age88 + parent_ed + degree_2012, data = echo)
# summary(lm1)
# 
# # with prior inheritance
# lm2 <- lm(networth_2012 ~ totinc_2012 + value_2012 + value_2008 + age_2012 + female +
#            relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#            sibnum_79 + rental_age88 + parent_ed + degree_2012, data = echo)
# summary(lm2)
# 
# # with prior inheritance and interaction
# lm3 <- lm(networth_2012 ~ totinc_2012 + value_2012 + value_2008 + age_2012 + female +
#            relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#            sibnum_79 + rental_age88 + parent_ed + degree_2012 + 
#            relevel(race, ref = "White")*value_2012 + relevel(race, ref = "White")*value_2008, 
#          data = echo)
# summary(lm3)
# 
# ## log-level model >> MAKE THIS LOG-LOG FOR FUNCTIONAL FORM
# ## subset for race, interaction for everything
# ## 1st diff would be nice complement,
# ## look into taking off sign of net worth and adding it back on after logging
# ## how foten does a positive windfall lead to negative net worth
# ## test each log transformation model by themselves, add one,
# ## heteroskedasticity* & outliers/information for inheritances, repeating inheritance
# ## baseline inheritance information, sharing their frequency and characteristics
# ## totinc & value
# m1 <- lm(logwealth_2012 ~ ln_totinc_2012 + ln_value_2012 + age_2012 + female +
#            relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#            sibnum_79 + rental_age88 + parent_ed + degree_2012, data = echo)
# summary(m1)
# 
# ##  with prior inheritance
# m2 <- lm(log(logwealth_2012) ~ totinc_2012 + value_2012 + value_2008 + age_2012 + 
#            female + relevel(race, ref = "White") + marstat_2012 + region_2012 +
#            kidnum_2012 + sibnum_79 + rental_age88 + parent_ed + degree_2012, 
#          data = echo)
# summary(m2)

# ## tobit or not tobit....
# library(VGAM)
# mt1 <- vglm(networth_2012 ~ totinc_2012 + value_2012 + age_2012 + female +
#              relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#              sibnum_79 + rental_age88 + parent_ed + degree_2012, 
#            family = tobit(Upper = 3690789), data = echo)
# summary(mt1)
# 
# ## prior inheritance
# mt2 <- vglm(networth_2012 ~ totinc_2012 + value_2012 + value_2008 + value_2004 + value_2000 +
#               age_2012 + female +
#               relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
#               sibnum_79 + rental_age88 + parent_ed + degree_2012, 
#             family = tobit(Upper = 3690789), data = echo)
# summary(mt2)
# 
# ## now with 2008 and prior inheritance
# mt3 <- vglm(networth_2008 ~ totinc_2008 + value_2008 + value_2004 + age_2008 + female +
#               relevel(race, ref = "White") + marstat_2008 + region_2008 + kidnum_2008 +  
#               sibnum_79 + rental_age88 + parent_ed + degree_2008, 
#             family = tobit(Upper = 3690789), data = echo)
# summary(mt3)
