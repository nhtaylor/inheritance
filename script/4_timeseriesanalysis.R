## ***  ## *** ## *** ## ***  ## *** ## *** ##

# race-separated datasets
# ets is total
etw <- subset(ets, race == "White")
etb <- subset(ets, race == "Black")
eth <- subset(ets, race == "Hispanic")


# simple OLS in plm package
library(plm)
m1 <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
            gift + age + female + degree + hours +
            marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
            parent_ed + as.factor(year), # model formula
               index = c("id", "year"), # id & time variables
               model = "pooling", 
               data = ets) ## this is equivalent to above OLS ##
m1 <- clusterSE(m1, cluster.var = "id")
summary(m1)


m_w <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
             gift + age + female + degree + hours +
            marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
            parent_ed + as.factor(year), # model formula
          index = c("id", "year"), # id & time variables
          model = "pooling", 
          data = etw) ## this is equivalent to above OLS ##
m_w <- clusterSE(m_w, cluster.var = "id")
summary(m_w)

m_b <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
             gift + age + female + degree + hours +
            marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
            parent_ed + as.factor(year), # model formula
          index = c("id", "year"), # id & time variables
          model = "pooling", 
          data = etb) ## this is equivalent to above OLS ##
m_b <- clusterSE(m_b, cluster.var = "id")
summary(m_b)

m_h <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
             gift + age + female + degree + hours +
            marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
            parent_ed + as.factor(year), # model formula
          index = c("id", "year"), # id & time variables
          model = "pooling", 
          data = eth) ## this is equivalent to above OLS ##
m_h <- clusterSE(m_h, cluster.var = "id")
summary(m_h)



## first differences
fd1 <- plm(d.wealth ~ d.income + d.value + d.nowealth + d.negwealth +
             d.gift + d.age + female + d.educ + 
            d.employ + marstat + region + d.reside + d.kid + sibnum_79 + 
            rental_age88 + parent_ed + as.factor(year), # female dropped b/c of singularity?
          index = c("id", "year"), # id & time variables
          model = "fd", ## first differences
          data = ets)
summary(fd1)


fd_w <- plm(d.wealth ~ d.income + d.value + d.nowealth + d.negwealth + 
              d.gift + d.age + female + d.educ + 
             d.employ + marstat + region + d.reside + d.kid + sibnum_79 + 
             rental_age88 + parent_ed + as.factor(year), # female dropped b/c of singularity?
           index = c("id", "year"), # id & time variables
           model = "fd", ## first differences
           data = etw)
summary(fd_w)

fd_b <- plm(d.wealth ~ d.income + d.value + d.nowealth + d.negwealth +
              d.gift + d.age + female + d.educ + 
             d.employ + marstat + region + d.reside + d.kid + sibnum_79 + 
             rental_age88 + parent_ed + as.factor(year), # female dropped b/c of singularity?
           index = c("id", "year"), # id & time variables
           model = "fd", ## first differences
           data = etb)
summary(fd_b)

fd_h <- plm(d.wealth ~ d.income + d.value + d.nowealth + d.negwealth +
              d.gift + d.age + female + d.educ + 
             d.employ + marstat + region + d.reside + d.kid + sibnum_79 + 
             rental_age88 + parent_ed + as.factor(year), # female dropped b/c of singularity?
           index = c("id", "year"), # id & time variables
           model = "fd", ## first differences
           data = eth)
summary(fd_h)



## fixed effects model!!
fe1 <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
             gift + age + female + degree + hours +
            marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
            parent_ed + as.factor(year), # model formula
          index = c("id", "year"), # id & time variables
          model = "within", 
          data = ets) ## fixed effects ## 
summary(fe1)
# sigmaRho(fe1)

fe_w <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
              gift + age + as.factor(female) + degree + hours +
             marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
             parent_ed + as.factor(year), # model formula
           index = c("id", "year"), # id & time variables
           model = "within", 
           data = etw) ## fixed effects ## 
summary(fe_w)

fe_b <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
              gift + age + as.factor(female) + degree + hours +
             marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
             parent_ed + as.factor(year), # model formula
           index = c("id", "year"), # id & time variables
           model = "within", 
           data = etb) ## fixed effects ## 
summary(fe_b)

fe_h <- plm(ln_wealth ~ ln_totinc + ln_value + nowealth + negwealth +
              gift + age + as.factor(female) + degree + hours +
             marstat + region + reside + kidnum + sibnum_79 + rental_age88 + 
             parent_ed + as.factor(year), # model formula
           index = c("id", "year"), # id & time variables
           model = "within", 
           data = eth) ## fixed effects ## 
summary(fe_h)


## stargazer
library(stargazer)

## POOLED OLS GRAPH
stargazer(m1, m_b, m_h, m_w, title = "Figure 3: Pooled OLS Regression", align = T,
          type = "html", out = "01_pooled.htm", 
          column.labels = c("Total", "Blacks", "Hispanic/Latinos",  "Whites"),
          dep.var.labels = c("Ln Household Net Worth"),
          covariate.labels = c("Ln Household Income", "Ln Inheritance Value", "No Wealth", 
                               "Negative Wealth", "Gift", "Age", "Female", "Education", 
                               "Full-time Employment", "Married", "Separated", "Divorced",
                               "Widowed", "North Central (region)", "South (region)", 
                               "West (region)", "Rural/Urban Residence", "Number of Children",
                               "Sibling Count",
                               "Parental Age", "Parental Education", "1989", "1990", "1992",
                               "1993", "1994", "1996", "1998", "2000", "2004", "2008", "2012"
                               )
          )
## FIRST DIFF TABLE
stargazer(fd1, fd_b, fd_h, fd_w, title = "Figure 4: First Difference Regression", align = T, 
          type = "html", out = "02_firstdiff.htm", 
          column.labels = c("Total", "Black", "Hispanic/Latino", "White"),
          dep.var.labels = c("Ln Household Net Worth Difference"),
          covariate.labels = c("Ln Household Income Difference", "Ln Inheritance Value Difference", 
                               "No Wealth Difference", 
                               "Negative Wealth Difference", "Gift Difference", "Age Differnece", 
                               "Education Difference", 
                               "Full-time Employment Difference", "Married", 
                               "Separated", "Divorced", "Widowed", 
                               "North Central (region)", "South (region)", 
                               "West (region)", "Rural/Urban Residence", 
                               "Number of Children Differenced",
                               "1989", "1990", "1992",
                               "1993", "1994", "1996", "1998", "2000", "2004", "2008")
)
 
## FIXED EFFECTS GRAPH         
stargazer(fe1, fe_b, fe_h, fe_w, title = "Figure 5: Fixed Effects Regression", align = T, 
          type = "html", out = "03_fixedeffects.htm", 
          column.labels = c("Total", "Black", "Hispanic/Latino", "White"),
          dep.var.labels = c("Ln Household Net Worth"),
          covariate.labels = c("Ln Household Income", "Ln Inheritance Value", "No Wealth", 
                               "Negative Wealth", "Gift", "Age", "Education", 
                               "Full-time Employment", "Married", "Separated", "Divorced",
                               "Widowed", "North Central (region)", "South (region)", 
                               "West (region)", "Rural/Urban Residence", "Number of Children",
                               "1989", "1990", "1992",
                               "1993", "1994", "1996", "1998", "2000", "2004", "2008")
)


### *** ## *** ## *** ###