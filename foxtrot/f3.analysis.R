## means & medians
library(tidyverse)
mean(echo$networth_1988, na.rm = T)
mean(echo$networth_1989, na.rm = T) ## looks strange
mean(echo$networth_1990, na.rm = T)
mean(echo$networth_1992, na.rm = T)
mean(echo$networth_1993, na.rm = T)
mean(echo$networth_1994, na.rm = T)
mean(echo$networth_1996, na.rm = T)
mean(echo$networth_1998, na.rm = T)
mean(echo$networth_2000, na.rm = T)
mean(echo$networth_2004, na.rm = T)
mean(echo$networth_2008, na.rm = T)
mean(echo$networth_2012, na.rm = T)

mean(echo$totinc_1988, na.rm = T)
mean(echo$totinc_1989, na.rm = T) ## also strange
mean(echo$totinc_1990, na.rm = T)
mean(echo$totinc_1992, na.rm = T) ## also weird
mean(echo$totinc_1993, na.rm = T)
mean(echo$totinc_1994, na.rm = T)
mean(echo$totinc_1996, na.rm = T)
mean(echo$totinc_1998, na.rm = T)
mean(echo$totinc_2000, na.rm = T)
mean(echo$totinc_2004, na.rm = T)
mean(echo$totinc_2008, na.rm = T)
mean(echo$totinc_2012, na.rm = T)

mean(echo$value_1988, na.rm = T)
mean(echo$value_1989, na.rm = T)
mean(echo$value_1990, na.rm = T)
mean(echo$value_1992, na.rm = T)
mean(echo$value_1993, na.rm = T)
mean(echo$value_1994, na.rm = T)
mean(echo$value_1996, na.rm = T)
mean(echo$value_1998, na.rm = T)
mean(echo$value_2000, na.rm = T)
mean(echo$value_2004, na.rm = T)
mean(echo$value_2008, na.rm = T)
mean(echo$value_2012, na.rm = T)

mean(echo$value_2008[echo$value_2008 != 0])
median(echo$value_2008[echo$value_2008 != 0])
table(echo$gift_2008)

## is value_X included in income_X?
tail(table(echo$totinc_1989))
tail(table(echo$value_1989))
tail(table(echo$networth_1989))

## traditional lm
m1 <- lm(networth_2012 ~ totinc_2012 + value_2012 + age_2012 + female +
         relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
         sibnum_79 + rental_age88 + parent_ed + degree_2012, data = echo)
summary(m1)

## tobit or not tobit....
library(VGAM)
m2 <- vglm(networth_2012 ~ totinc_2012 + value_2012 + age_2012 + female +
             relevel(race, ref = "White") + marstat_2012 + region_2012 + kidnum_2012 +  
             sibnum_79 + rental_age88 + parent_ed + degree_2012, 
           family = tobit(Upper = 3690789), data = echo)
summary(m2)



