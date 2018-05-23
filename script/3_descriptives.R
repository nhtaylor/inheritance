## descriptive statistics
options(scipen = 999)

## WHITES
e <- echo %>% 
  filter(race == "Black")
# number of inheritances
table(e$gift_1988)
sum(e$gift_1988 == 1, na.rm = T) / sum(!is.na(e$gift_1988))
table(e$gift_1989)
sum(e$gift_1989 == 1, na.rm = T) / sum(!is.na(e$gift_1989))
table(e$gift_1990)
sum(e$gift_1990 == 1, na.rm = T) / sum(!is.na(e$gift_1990))
table(e$gift_1992)
sum(e$gift_1992 == 1, na.rm = T) / sum(!is.na(e$gift_1992))
table(e$gift_1993)
sum(e$gift_1993 == 1, na.rm = T) / sum(!is.na(e$gift_1993))
table(e$gift_1994)
sum(e$gift_1994 == 1, na.rm = T) / sum(!is.na(e$gift_1994))
table(e$gift_1996)
sum(e$gift_1996 == 1, na.rm = T) / sum(!is.na(e$gift_1996))
table(e$gift_1998)
sum(e$gift_1998 == 1, na.rm = T) / sum(!is.na(e$gift_1998))
table(e$gift_2000)
sum(e$gift_2000 == 1, na.rm = T) / sum(!is.na(e$gift_2000))
table(e$gift_2004)
sum(e$gift_2004 == 1, na.rm = T) / sum(!is.na(e$gift_2004))
table(e$gift_2008)
sum(e$gift_2008 == 1, na.rm = T) / sum(!is.na(e$gift_2008))
table(e$gift_2012)
sum(e$gift_2012 == 1, na.rm = T) / sum(!is.na(e$gift_2012))

# total number for white across time period
y <- sum(e$gift_2012 == 1, na.rm = T) + 
  sum(e$gift_2008 == 1, na.rm = T) +  
  sum(e$gift_2004 == 1, na.rm = T) +  
  sum(e$gift_2000 == 1, na.rm = T) +
  sum(e$gift_1998 == 1, na.rm = T) +  
  sum(e$gift_1996 == 1, na.rm = T) +  
  sum(e$gift_1994 == 1, na.rm = T) +
  sum(e$gift_1993 == 1, na.rm = T) +  
  sum(e$gift_1992 == 1, na.rm = T) +  
  sum(e$gift_1990 == 1, na.rm = T) +
  sum(e$gift_1989 == 1, na.rm = T) +  
  sum(e$gift_1988 == 1, na.rm = T)
  
## prepping for mean & median database
d <- echo %>% 
  filter(race == "White")
d$value_2012[d$value_2012 == 0] <- NA
d$value_2008[d$value_2008 == 0] <- NA
d$value_2004[d$value_2004 == 0] <- NA
d$value_2000[d$value_2000 == 0] <- NA
d$value_1998[d$value_1998 == 0] <- NA
d$value_1996[d$value_1996 == 0] <- NA
d$value_1994[d$value_1994 == 0] <- NA
d$value_1993[d$value_1993 == 0] <- NA
d$value_1992[d$value_1992 == 0] <- NA
d$value_1990[d$value_1990 == 0] <- NA
d$value_1989[d$value_1989 == 0] <- NA
d$value_1988[d$value_1988 == 0] <- NA

# mean inheritance value
mean(d$value_1988, na.rm = T)
mean(d$value_1989, na.rm = T)
mean(d$value_1990, na.rm = T)
mean(d$value_1992, na.rm = T)
mean(d$value_1993, na.rm = T)
mean(d$value_1994, na.rm = T)
mean(d$value_1996, na.rm = T)
mean(d$value_1998, na.rm = T)
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_1988,
               d$value_1989,
               d$value_1990,
               d$value_1992,
               d$value_1993,
               d$value_1994,
               d$value_1996,
               d$value_1998,
               d$value_2000,
               d$value_2004, 
               d$value_2008, 
               d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
median(d$value_1988, na.rm = T)
median(d$value_1989, na.rm = T)
median(d$value_1990, na.rm = T)
median(d$value_1992, na.rm = T)
median(d$value_1993, na.rm = T)
median(d$value_1994, na.rm = T)
median(d$value_1996, na.rm = T)
median(d$value_1998, na.rm = T)
median(d$value_2000, na.rm = T)
median(d$value_2004, na.rm = T)
median(d$value_2008, na.rm = T)
median(d$value_2012, na.rm = T)
# total
z_bar <- rbind(delta$value_2000,delta$value_2004,delta$value_2008,delta$value_2012)
median(z_bar, na.rm = T)


## BLACKS
e <- echo %>%
  filter(race == "Black")
# number of inheritances
table(e$gift_2000)
table(e$gift_2004)
table(e$gift_2008)
table(e$gift_2012)
# total number for white across time period
sum(e$gift_2012 == 1, na.rm = T) + sum(e$gift_2008 == 1, na.rm = T) +  
  sum(e$gift_2004 == 1, na.rm = T) +  sum(e$gift_2000 == 1, na.rm = T)

## prepping for mean & median database
d <- echo %>%
  filter(race == "Black")
d$value_2012[d$value_2012 == 0] <- NA
d$value_2008[d$value_2008 == 0] <- NA
d$value_2004[d$value_2004 == 0] <- NA
d$value_2000[d$value_2000 == 0] <- NA
d$value_1998[d$value_1998 == 0] <- NA
d$value_1996[d$value_1996 == 0] <- NA
d$value_1994[d$value_1994 == 0] <- NA
d$value_1993[d$value_1993 == 0] <- NA
d$value_1992[d$value_1992 == 0] <- NA
d$value_1990[d$value_1990 == 0] <- NA
d$value_1989[d$value_1989 == 0] <- NA
d$value_1988[d$value_1988 == 0] <- NA

# mean inheritance value
mean(d$value_1988, na.rm = T)
mean(d$value_1989, na.rm = T)
mean(d$value_1990, na.rm = T)
mean(d$value_1992, na.rm = T)
mean(d$value_1993, na.rm = T)
mean(d$value_1994, na.rm = T)
mean(d$value_1996, na.rm = T)
mean(d$value_1998, na.rm = T)
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_2000, d$value_2004, d$value_2008, d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
median(d$value_1988, na.rm = T)
median(d$value_1989, na.rm = T)
median(d$value_1990, na.rm = T)
median(d$value_1992, na.rm = T)
median(d$value_1993, na.rm = T)
median(d$value_1994, na.rm = T)
median(d$value_1996, na.rm = T)
median(d$value_1998, na.rm = T)
median(d$value_2000, na.rm = T)
median(d$value_2004, na.rm = T)
median(d$value_2008, na.rm = T)
median(d$value_2012, na.rm = T)
# total
z_bar <- rbind(delta$value_2000,delta$value_2004,delta$value_2008,delta$value_2012)
median(z_bar, na.rm = T)


## HISPANICS/LATINOS
e <- echo %>%
  filter(race == "Hispanic")
# number of inheritances
table(e$gift_2000)
table(e$gift_2004)
table(e$gift_2008)
table(e$gift_2012)
# total number for white across time period
sum(e$gift_2012 == 1, na.rm = T) + sum(e$gift_2008 == 1, na.rm = T) +  
  sum(e$gift_2004 == 1, na.rm = T) +  sum(e$gift_2000 == 1, na.rm = T)

## prepping for mean & median database
d <- echo %>%
  filter(race == "Hispanic")
d$value_2012[d$value_2012 == 0] <- NA
d$value_2008[d$value_2008 == 0] <- NA
d$value_2004[d$value_2004 == 0] <- NA
d$value_2000[d$value_2000 == 0] <- NA
d$value_1998[d$value_1998 == 0] <- NA
d$value_1996[d$value_1996 == 0] <- NA
d$value_1994[d$value_1994 == 0] <- NA
d$value_1993[d$value_1993 == 0] <- NA
d$value_1992[d$value_1992 == 0] <- NA
d$value_1990[d$value_1990 == 0] <- NA
d$value_1989[d$value_1989 == 0] <- NA
d$value_1988[d$value_1988 == 0] <- NA

# mean inheritance value
mean(d$value_1988, na.rm = T)
mean(d$value_1989, na.rm = T)
mean(d$value_1990, na.rm = T)
mean(d$value_1992, na.rm = T)
mean(d$value_1993, na.rm = T)
mean(d$value_1994, na.rm = T)
mean(d$value_1996, na.rm = T)
mean(d$value_1998, na.rm = T)
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_2000, d$value_2004, d$value_2008, d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
median(d$value_1988, na.rm = T)
median(d$value_1989, na.rm = T)
median(d$value_1990, na.rm = T)
median(d$value_1992, na.rm = T)
median(d$value_1993, na.rm = T)
median(d$value_1994, na.rm = T)
median(d$value_1996, na.rm = T)
median(d$value_1998, na.rm = T)
median(d$value_2000, na.rm = T)
median(d$value_2004, na.rm = T)
median(d$value_2008, na.rm = T)
median(d$value_2012, na.rm = T)
# total
z_bar <- rbind(delta$value_2000,delta$value_2004,delta$value_2008,delta$value_2012)
median(z_bar, na.rm = T)






## LONGITUDINAL DATA
# whites
e1 <- echo %>% 
  filter(race == "White")
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e1[is.na(e1)] <- 0
e1$total <-  e1$gift_2000 + 
  e1$gift_2004 + 
  e1$gift_2008 + 
  e1$gift_2012 +
  e1$gift_1988 + 
  e1$gift_1989 + 
  e1$gift_1990 +
  e1$gift_1992 + 
  e1$gift_1993 + 
  e1$gift_1994 +  
  e1$gift_1996 + 
  e1$gift_1998

sum(e1$total) ## 2264
table(e1$total)
2844 / 6559 # number of ppl receiving at least one inheritance / all white ppl
1484 / (1471 + 1360) # number of inheritances received as 2nd or more / total number of inheritances
# 36



# blacks
e2 <- echo %>%
  filter(race == "Black")
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e2[is.na(e2)] <- 0
e2$total <- e2$gift_2000 + 
  e2$gift_2004 + 
  e2$gift_2008 + 
  e2$gift_2012 +
  e2$gift_1988 + 
  e2$gift_1989 + 
  e2$gift_1990 +
  e2$gift_1992 + 
  e2$gift_1993 + 
  e2$gift_1994 +  
  e2$gift_1996 + 
  e2$gift_1998 
table(e2$total)
sum(e2$total) ## 2264
768 / 2923 #26%
285 / 768 # 37%

# hispanic
e2 <- echo %>%
  filter(race == "Hispanic") 
  select(gift_2012, gift_2008, gift_2004, gift_2000)
e2[is.na(e2)] <- 0
e2$total <- e2$gift_2000 + 
  e2$gift_2004 + 
  e2$gift_2008 + 
  e2$gift_2012 +
  e2$gift_1988 + 
  e2$gift_1989 + 
  e2$gift_1990 +
  e2$gift_1992 + 
  e2$gift_1993 + 
  e2$gift_1994 +  
  e2$gift_1996 + 
  e2$gift_1998
table(e2$total)
sum(e2$total) ## 1020
546 / 1924 # 12%
221 / (546) # 27%







## Boring descirptive data
length(which(!is.na(echo$kidnum_2000)))
length(which(!is.na(echo$kidnum_2004)))
length(which(!is.na(echo$kidnum_2008)))
length(which(!is.na(echo$kidnum_2012)))


mean(echo$kidnum_2000, na.rm = T)
mean(echo$kidnum_2004, na.rm = T)
mean(echo$kidnum_2008, na.rm = T)
mean(echo$kidnum_2012, na.rm = T)

mean(echo$degree_2000, na.rm = T)
mean(echo$degree_2004, na.rm = T)
mean(echo$degree_2008, na.rm = T)
mean(echo$degree_2012, na.rm = T)

mean(echo$age_2000, na.rm = T)
mean(echo$age_2004, na.rm = T)
mean(echo$age_2008, na.rm = T)
mean(echo$age_2012, na.rm = T)

f <- echo %>%
  filter(!is.na(kidnum_2012))
sum(f$female[f$female == 1])
sum(f$female[f$female == 1]) / length(f$female)
table(f$race)





## Descriptives Table
## removing na's based on 2012 net worth (whole sample present) -- no longer beneficial
d <- echo[!is.na(echo$kidnum_2012), ]
# nrow(d)
# table(d$race)
d <- d %>%
  filter(race == "White") ## change for different races
mean(d$networth_2012, na.rm = T)
sd(d$networth_2012, na.rm = T)
median(d$networth_2012, na.rm = T)
table(d$negwealth_2012)
table(d$nowealth_2012)

mean(d$totinc_2012, na.rm = T)
sd(d$totinc_2012, na.rm = T)
median(d$totinc_2012, na.rm = T)

mean(d$age_2012, na.rm = T)
mean(d$rental_age88 - 10, na.rm = T)
mean(d$sibnum_79, na.rm = T)

table(d$female)
table(d$marstat_2012)
sum(!is.na(d$marstat_2012))
mean(d$kidnum_2012, na.rm = T)
table(d$hours_2012)

table(d$degree_2012)
sum(!is.na(d$degree_2012))
table(d$parent_ed)
sum(!is.na(d$parent_ed))

table(d$reside_2012)
table(d$region_2012)
sum(!is.na(d$region_2012))
