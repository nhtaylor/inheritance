## descriptive statistics
options(scipen = 999)

ggplot(echo, aes(x = value_2012)) +
  geom_histogram(binwidth = 15)


# echo$totvalue[echo$totvalue == 0] <- NA
# mean(echo$totvalue, na.rm = TRUE)


## WHITES
e <- echo %>%
  filter(race == "White")
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
  filter(race == "White")
d$value_2012[d$value_2012 == 0] <- NA
d$value_2008[d$value_2008 == 0] <- NA
d$value_2004[d$value_2004 == 0] <- NA
d$value_2000[d$value_2000 == 0] <- NA

# mean inheritance value
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_2000, d$value_2004, d$value_2008, d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
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

# mean inheritance value
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_2000, d$value_2004, d$value_2008, d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
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

# mean inheritance value
mean(d$value_2000, na.rm = T)
mean(d$value_2004, na.rm = T)
mean(d$value_2008, na.rm = T)
mean(d$value_2012, na.rm = T)
# total
x_bar <- rbind(d$value_2000, d$value_2004, d$value_2008, d$value_2012)
mean(x_bar, na.rm = T)

# median inheritance value
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
  filter(!is.na(kidnum_2000))
sum(f$female[f$female == 1])
sum(f$female[f$female == 1]) / length(f$female)
table(f$race)
