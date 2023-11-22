#This script reports on the analysis in paper x and consists of 3 parts.
#Part 1 contains the analysis for the difference-in-difference design. Part 2 & 3 
#contain the analysis conducted in the appendix.


install.packages("Hmisc")
install.packages("tidyverse")
install.packages("stargazer")
install.packages("plm")
install.packages("lmtest")
install.packages("sandwich")
install.packages("xtable")
install.packages("plm")
install.packages("lmtest")



library(Hmisc)     # CRAN v4.4-2
library(tidyverse) # CRAN v1.3.1
library(stargazer) # CRAN v5.2.2
library(plm)       # CRAN v2.2-3
library(lmtest)    # CRAN v0.9-38
library(sandwich)  # CRAN v2.5-1
library(xtable)    # CRAN v1.8-4

#load in data
ballot_days_final <- readRDS("/Users/fperil/Documents/0_IPZ/2023_2/Leemann-QuantMethods/QuantitativeMethods/QuantitativeMethods/Data/replication/data/ballot_days_final.rds")  

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Part 1: Difference-in-difference ----
##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A

#------------------------------------------------------------------------------------------#
# Table 1: Difference-in-Differences Estimation of Turnout on Prepaid Postage ----
#------------------------------------------------------------------------------------------#

# Column 1:
Diff_c1 <- plm(turnout ~ postage,
               data = ballot_days_final, 
               index = c('muninr', 'votedateid'),
               model = 'within',
               effect = 'twoways')


# calculate clustered st. errors
# vcovHC(Diff_c1, cluster="group", type="HC1")
Diff_c1_coef <- coeftest(Diff_c1,
                         vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# Column 2:
Diff_c2 <- plm(turnout ~ postage + 
                 meaninc + medmeaninc + pop + popdens + foreignshare +
                 youngshare + oldshare + cantonal_election,
               data = ballot_days_final, 
               index = c('muninr', 'votedateid'),
               model = 'within',
               effect = 'twoways')

Diff_c2_coef <- coeftest(Diff_c2, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Save Table 1 
strategy1_ss_models <- list(Diff_c1, Diff_c2)
strategy1_ss_se <-list(Diff_c1_coef[,2], Diff_c2_coef[,2])


stargazer(
  strategy1_ss_models,
  se = strategy1_ss_se,
  type="text",
  omit = c("votedateid"),
  dep.var.labels=c("Turnout"),
  dep.var.caption  = "",
  font.size="normalsize",
  add.lines = list(c("Municipal FE", "\\ding{51}", "\\ding{51}"),
                   c("Vote Day FE", "\\ding{51}", "\\ding{51}")),
  omit.stat = c("adj.rsq", "f"),
  title = "Difference-in-Differences Estimation of Turnout on Prepaid Postage",
  covariate.labels = c("Prepaid Postage", "Mean Income", "Mean / Median Income", 
                       "Population", "Population Density", "\\% foreigners",
                       "\\% young", "\\% aged",  "Dummy Cantonal Election")
) 


# stargazer(
#   strategy1_ss_models,
#   se = strategy1_ss_se,
#   type="latex",
#   omit = c("votedateid"),
#   dep.var.labels=c("Turnout"),
#   dep.var.caption  = "",
#   font.size="normalsize",
#   add.lines = list(c("Municipal FE", "\\ding{51}", "\\ding{51}"),
#                    c("Vote Day FE", "\\ding{51}", "\\ding{51}")),
#   omit.stat = c("adj.rsq", "f"),
#   title = "Difference-in-Differences Estimation of Turnout on Prepaid Postage",
#   covariate.labels = c("Prepaid Postage", "Mean Income", "Mean / Median Income",
#                        "Population", "Population Density", "\\% foreigners",
#                        "\\% young", "\\% aged",  "Dummy Cantonal Election"),
#   out="/Users/fperil/Documents/0_IPZ/2023_2/Leemann-QuantMethods/QuantitativeMethods/QuantitativeMethods/Data/replication/data//DID_rep.tex"
# )
# 

#------------------------------------------------------------------------------------------#
# Table 2: Testing for Effect Heterogeneity ----
#------------------------------------------------------------------------------------------#

                   # create TREATED dummy

test <- ballot_days_final %>% 
  group_by(muninr) %>%
  summarise(mean.postage = mean(postage, na.rm = T)) 

test$TREATED <- ifelse(test$mean.postage > 0, 1, 0)

ballot_days_final <- left_join( ballot_days_final, test, by = c("muninr"= "muninr"))
table(ballot_days_final$TREATED, ballot_days_final$postage, useNA = "ifany")

ballot_days_final$large.pop <- as.numeric(ballot_days_final$pop>5)

# Column 1
Diff_c3 <- plm(turnout ~ postage * large.pop + large.pop:factor(votedateid) + meaninc + medmeaninc + popdens + foreignshare + youngshare + oldshare + cantonal_election,
    data = ballot_days_final,
    index = c('muninr', 'votedateid'),
    model = 'within',
    effect = 'twoways')

# calculate clustered standard errors
Diff_c3_coef <- coeftest(Diff_c3, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))

# Column 2
Diff_c4 <- plm(turnout ~ postage * pop + pop:factor(votedateid) +  meaninc + medmeaninc + popdens + foreignshare + youngshare + oldshare + cantonal_election,
    data = ballot_days_final,
    index = c('muninr', 'votedateid'),
    model = 'within',
    effect = 'twoways')

Diff_c4_coef <- coeftest(Diff_c4, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1"))

#Save Table 2
strategy1_split_models <- list(Diff_c3, Diff_c4)
strategy1_split_se <-list(Diff_c3_coef[,2], Diff_c4_coef[,2])


stargazer(
  strategy1_split_models,
  se = strategy1_split_se,
  type = "text",
  dep.var.labels=c("Turnout"),
  dep.var.caption  = "",
  font.size="normalsize",
  title = "Difference-in-Differences Estimation of Turnout on Prepaid Postage",
  covariate.labels = c("Prepaid Postage", "Large Population", "Population",
                       "Prepaid Postage * Large Population",
                       "Prepaid Postage * Population"),
  add.lines = list(c("Controls", "\\ding{51}", "\\ding{51}"),
                   c("Municipal FE", "\\ding{51}", "\\ding{51}"),
                   c("Vote Day FE", "\\ding{51}", "\\ding{51}")),
  omit.stat = c("adj.rsq", "f"),
  omit = c("pop\\:factor", "meaninc", "medmeaninc", "popdens", "foreignshare", "youngshare","oldshare","cantonal")
) #

# stargazer(
#   strategy1_split_models,
#   se = strategy1_split_se,
#   #type = "text",
#   type="latex",
#   #omit = c(),
#   dep.var.labels=c("Turnout"),
#   dep.var.caption  = "",
#   font.size="normalsize",
#   title = "Difference-in-Differences Estimation of Turnout on Prepaid Postage",
#   covariate.labels = c("Prepaid Postage", "Large Population", "Population", 
#                        "Prepaid Postage * Large Population",
#                        "Prepaid Postage * Population"),
#   add.lines = list(c("Controls", "\\ding{51}", "\\ding{51}"),
#                    c("Municipal FE", "\\ding{51}", "\\ding{51}"),
#                    c("Vote Day FE", "\\ding{51}", "\\ding{51}")),
#   omit.stat = c("adj.rsq", "f"),
#   omit = c("pop\\:factor", "meaninc", "medmeaninc", "popdens", "foreignshare", "youngshare","oldshare","cantonal"),
#   out="tables/DID_split_new.tex"
# ) # 



##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Part 2: Appendix: IV Regression Results ----
##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A

#For this analysis we estimate four models, each one for nominal and real prices in three settings:
# - all municipalities (part 2a)
# - small municipalities only (part 2b)
# - large municipalities only (part 2c)


#Data preparation
ballot_days_final$stampuse<- ifelse(ballot_days_final$postage==1, 0, 1)

#real price of stamp
ballot_days_final$rpostageprice <- ballot_days_final$stampuse * ballot_days_final$rstamp

#nominal price of stamp
ballot_days_final$npostageprice <- ballot_days_final$stampuse * ballot_days_final$nstamp

# stamp price change: 2004 --> 85 cents
ballot_days_final$stampricechange <- ifelse(ballot_days_final$postage==0 & ballot_days_final$year==2004, 1, 0)

table(ballot_days_final$stampricechange, useNA = "ifany")

ballot_days_final <- ballot_days_final %>%
  group_by(votedateid) %>%
  mutate(rpostageprice_avg = ifelse(postage == 0, mean(rpostageprice), NA))

# Split data into small vs. large municipalities
ballot_days_final <- ballot_days_final %>% mutate(quartile = ntile(pop, 4))

ballot_days_small <- ballot_days_final %>% filter(quartile ==1) %>% as.data.frame()
ballot_days_large <- ballot_days_final %>% filter(quartile !=1) %>% as.data.frame()

#------------------------------------------------------------------------------------------#
# Part 2a) All Municipalities ----
#          NOMINAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

# Column 1 
tab4_Ac1_all <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                    data = ballot_days_final, 
                    index = c('muninr', 'votedateid'), 
                    model = 'within',
                    effect = 'twoways') 

# clustered st. errors
tab4_Ac1_coef_all <- coeftest(tab4_Ac1_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 


# Column 2
tab4_Ac2_second_all <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , 
                           data = ballot_days_final,
                           index = c('muninr', 'votedateid'), 
                           model = "within", 
                           effect ="twoways")
# clustered st. errors
tab4_Ac2_coef_second_all <- coeftest(tab4_Ac2_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))


# Column 3
tab4_Ac3_second_all <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                             stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                           data = ballot_days_final,
                           index = c('muninr', 'votedateid'),
                           model = "within",
                           effect = "twoways")

# clustered st. errors
tab4_Ac3_coef_second_all <-coeftest(tab4_Ac3_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 


# Column 4
tab4_Ac4_second_all <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage+stampricechange +meaninc + medmeaninc+ pop + popdens + foreignshare + youngshare +oldshare , 
                           data = ballot_days_final,
                           index = c('muninr', 'votedateid'), 
                           model = "within", 
                           effect ="twoways")

# clustered st. errors
tab4_Ac4_coef_second_all <- coeftest(tab4_Ac4_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab4_Ac2_first_all <- plm(npostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_final,
                          index = c('muninr', 'votedateid'), 
                          model = "within", effect ="twoways")

# clustered st. errors
tab4_Ac2_coef_first_all <- coeftest(tab4_Ac2_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab4_Ac3_first_all <- plm(npostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_final,
                          index = c('muninr', 'votedateid'), 
                          model = "within", effect ="twoways")

tab4_Ac3_coef_first_all <- coeftest(tab4_Ac3_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab4_Ac4_first_all <- plm(npostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                            popdens + foreignshare + youngshare +oldshare , data = ballot_days_final,
                          index = c('muninr', 'votedateid'), 
                          model = "within", effect ="twoways")

tab4_Ac4_coef_first_all <- coeftest(tab4_Ac4_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#- Part 1 (second stage) of Table A4 -
# ---> has to be done manually
Tab4_models_second_all <- list(tab4_Ac1_all, 
                               tab4_Ac2_second_all, 
                               tab4_Ac3_second_all, 
                               tab4_Ac4_second_all)
Tab4_coefs_second_all <- list(tab4_Ac1_coef_all[,2], 
                              tab4_Ac2_coef_second_all[,2], 
                              tab4_Ac3_coef_second_all[,2], 
                              tab4_Ac4_coef_second_all[,2])

stargazer(
  Tab4_models_second_all,
  se = Tab4_coefs_second_all,
  title = "Turnout Nominal Costs: Second Stage Regressions for All Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare"),
  type="text"
) 

#- Part 2 (first stage) of Table A4 -
# ---> has to be done manually
Tab4_models_first_all <- list(tab4_Ac1_all, tab4_Ac2_first_all, tab4_Ac3_first_all, tab4_Ac4_first_all)
Tab4_coefs_first_all <- list(tab4_Ac1_coef_all[,2], tab4_Ac2_coef_first_all[,2], tab4_Ac3_coef_first_all[,2], tab4_Ac4_coef_first_all[,2])

stargazer(
  Tab4_models_first_all,
  type = "text",
  se = Tab4_coefs_first_all,
  title = "Turnout nominal costs: first stage regressions for All municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("npostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 


#------------------------------------------------------------------------------------------#
# Part 2a) All Municipalities ----
#          REAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

#Column 1
tab5_Ac1_all <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                    data = ballot_days_final, 
                    index = c('muninr', 'votedateid'), 
                    model = 'within', effect = 'twoways')

# clustered st. errors
tab5_Ac1_coef_all <- coeftest(tab5_Ac1_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 2
tab5_Ac2_second_all <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , 
                           data = ballot_days_final,
                           index = c('muninr', 'votedateid'), 
                           model = "within", effect ="twoways")

tab5_Ac2_coef_second_all <- coeftest(tab5_Ac2_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) # clustered st. errors

#Column 3
tab5_Ac3_second_all <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                             stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                           data = ballot_days_final,
                           index = c('muninr', 'votedateid'),
                           model = "within",
                           effect = "twoways")

tab5_Ac3_coef_second_all <-coeftest(tab5_Ac3_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab5_Ac4_second_all <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage+stampricechange +meaninc +medmeaninc+ pop +popdens + foreignshare + youngshare +oldshare , data = ballot_days_final,
                           index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

#clustered st. errors
tab5_Ac4_coef_second_all <- coeftest(tab5_Ac4_second_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab5_Ac2_first_all <- plm(rpostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_final,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab5_Ac2_coef_first_all <- coeftest(tab5_Ac2_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) # muninr clustered st. errors

#Column 3
tab5_Ac3_first_all <- plm(rpostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_final,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab5_Ac3_coef_first_all <- coeftest(tab5_Ac3_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab5_Ac4_first_all <- plm(rpostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                            popdens + foreignshare + youngshare +oldshare , data = ballot_days_final,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab5_Ac4_coef_first_all <- coeftest(tab5_Ac4_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 


#- Part 1 (second stage) of Table A5 -
# ---> has to be done manually
tab5_models_second_all <- list(tab5_Ac1_all, tab5_Ac2_second_all, tab5_Ac3_second_all, tab5_Ac4_second_all)
tab5_coefs_second_all <- list(tab5_Ac1_coef_all[,2], tab5_Ac2_coef_second_all[,2], tab5_Ac3_coef_second_all[,2], tab5_Ac4_coef_second_all[,2])

stargazer(
  tab5_models_second_all,
  se = tab5_coefs_second_all,
  title = "Turnout Rreal Costs: Second Stage Regressions for All Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare"),
  type="text"
) 

#- Part 2 (second stage) of Table A5 -
# ---> has to be done manually
tab5_models_first_all <- list(tab5_Ac1_all, tab5_Ac2_first_all, tab5_Ac3_first_all, tab5_Ac4_first_all)
tab5_coefs_first_all <- list(tab5_Ac1_coef_all[,2], tab5_Ac2_coef_first_all[,2], tab5_Ac3_coef_first_all[,2], tab5_Ac4_coef_first_all[,2])

stargazer(
  tab5_models_first_all,
  type = "text",
  se = tab5_coefs_first_all,
  title = "Turnout Real Costs: First Stage Regressions for All Municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("rpostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 



#------------------------------------------------------------------------------------------#
# Part 2b) Small Municipalities ----
#          Table A6: NOMINAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

#Column 1
tab4_Ac1_small <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                    data = ballot_days_small, index = c('muninr', 'votedateid'), model = 'within',
                    effect = 'twoways')

# clustered st. errors
tab4_Ac1_coef_small <- coeftest(tab4_Ac1_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 2
tab4_Ac2_second_small <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , data = ballot_days_small,
                           index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# clustered st. errors
tab4_Ac2_coef_second_small <- coeftest(tab4_Ac2_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab4_Ac3_second_small <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                             stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                           data = ballot_days_small,
                           index = c('muninr', 'votedateid'),
                           model = "within",
                           effect = "twoways")

tab4_Ac3_coef_second_small <-coeftest(tab4_Ac3_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab4_Ac4_second_small <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage+stampricechange +meaninc +medmeaninc+ pop + popdens + foreignshare + youngshare +oldshare , data = ballot_days_small,
                           index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# clustered st. errors
tab4_Ac4_coef_second_small <- coeftest(tab4_Ac4_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab4_Ac2_first_small <- plm(npostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# muninr clustered st. errors
tab4_Ac2_coef_first_small <- coeftest(tab4_Ac2_first_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

#Column 3
tab4_Ac3_first_small <- plm(npostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab4_Ac3_coef_first_small <- coeftest(tab4_Ac3_first_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab4_Ac4_first_small <- plm(npostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                            popdens + foreignshare + youngshare +oldshare , data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab4_Ac4_coef_first_small <- coeftest(tab4_Ac4_first_all, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#- Part 1 (second stage) of Table A6 -
# ---> has to be done manually
Tab4_models_second_small <- list(tab4_Ac1_small, tab4_Ac2_second_small, tab4_Ac3_second_small, tab4_Ac4_second_small)
Tab4_coefs_second_small <- list(tab4_Ac1_coef_small[,2], tab4_Ac2_coef_second_small[,2], tab4_Ac3_coef_second_small[,2], tab4_Ac4_coef_second_small[,2])

stargazer(
  Tab4_models_second_small,
  se = Tab4_coefs_second_small,
  title = "Turnout Nominal Costs: Second Stage Regressions for Small Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  type="text",
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 

#- Part 2 (first stage) of Table A6 -
# ---> has to be done manually
Tab4_models_first_small <- list(tab4_Ac1_small, tab4_Ac2_first_small, tab4_Ac3_first_small, tab4_Ac4_first_small)
Tab4_coefs_first_small <- list(tab4_Ac1_coef_small[,2], tab4_Ac2_coef_first_small[,2], tab4_Ac3_coef_first_small[,2], tab4_Ac4_coef_first_small[,2])

stargazer(
  Tab4_models_first_small,
  se = Tab4_coefs_first_small,
  type = "text",
  title = "Turnout nominal costs: first stage regressions for Small municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("npostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 


#------------------------------------------------------------------------------------------#
# Part 2b) Small Municipalities ----
#          Table A7: REAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

#Column 1
tab7_Ac1_small <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                    data = ballot_days_small, index = c('muninr', 'votedateid'), model = 'within',
                    effect = 'twoways')
# clustered st. errors
tab7_Ac1_coef_small <- coeftest(tab7_Ac1_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 2
tab7_Ac2_second_small <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , data = ballot_days_small,
                           index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# clustered st. errors
tab7_Ac2_coef_second_small <- coeftest(tab7_Ac2_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab7_Ac3_second_small <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                             stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                           data = ballot_days_small,
                           index = c('muninr', 'votedateid'),
                           model = "within",
                           effect = "twoways")

tab7_Ac3_coef_second_small <-coeftest(tab7_Ac3_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab7_Ac4_second_small <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                             postage+stampricechange +meaninc +medmeaninc+ pop +popdens + foreignshare + youngshare +oldshare , data = ballot_days_small,
                           index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# clustered st. errors
tab7_Ac4_coef_second_small <- coeftest(tab7_Ac4_second_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab7_Ac2_first_small <- plm(rpostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# muninr clustered st. errors
tab7_Ac2_coef_first_small <- coeftest(tab7_Ac2_first_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab7_Ac3_first_small <- plm(rpostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                          data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab7_Ac3_coef_first_small <- coeftest(tab7_Ac3_first_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab7_Ac4_first_small <- plm(rpostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                            popdens + foreignshare + youngshare +oldshare , data = ballot_days_small,
                          index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab7_Ac4_coef_first_small <- coeftest(tab7_Ac4_first_small, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#- Part 2 (second stage) of Table A7 -
# ---> has to be done manually
tab7_models_second_small <- list(tab7_Ac1_small, tab7_Ac2_second_small, tab7_Ac3_second_small, tab7_Ac4_second_small)
tab7_coefs_second_small <- list(tab7_Ac1_coef_small[,2], tab7_Ac2_coef_second_small[,2], tab7_Ac3_coef_second_small[,2], tab7_Ac4_coef_second_small[,2])

stargazer(
  tab7_models_second_small,
  se = tab7_coefs_second_small,
  title = "Turnout real Costs: Second Stage Regressions for small Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare"),
  type="text"
) 

#- Part 2 (first stage) of Table A7 -
# ---> has to be done manually
tab7_models_first_small <- list(tab7_Ac1_small, tab7_Ac2_first_small, tab7_Ac3_first_small, tab7_Ac4_first_small)
tab7_coefs_first_small <- list(tab7_Ac1_coef_small[,2], tab7_Ac2_coef_first_small[,2], tab7_Ac3_coef_first_small[,2], tab7_Ac4_coef_first_small[,2])

stargazer(
  tab7_models_first_small,
  se = tab7_coefs_first_small,
  type = "text",
  title = "Turnout real costs: first stage regressions for Small municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("rpostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")) 


#------------------------------------------------------------------------------------------#
# Part 2c) Large Municipalities ----
#          Table A8: NOMINAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

#Column 1
tab8_Ac1_large <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                      data = ballot_days_large, index = c('muninr', 'votedateid'), model = 'within',
                      effect = 'twoways')

# clustered st. errors
tab8_Ac1_coef_large <- coeftest(tab8_Ac1_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 2
tab8_Ac2_second_large <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                               postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , data = ballot_days_large,
                             index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# clustered st. errors
tab8_Ac2_coef_second_large <- coeftest(tab8_Ac2_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab8_Ac3_second_large <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                               stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                             data = ballot_days_large,
                             index = c('muninr', 'votedateid'),
                             model = "within",
                             effect = "twoways")

tab8_Ac3_coef_second_large <-coeftest(tab8_Ac3_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab8_Ac4_second_large <- plm(turnout ~ npostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                               postage+stampricechange +meaninc +medmeaninc+ pop + popdens + foreignshare + youngshare +oldshare , data = ballot_days_large,
                             index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# clustered st. errors
tab8_Ac4_coef_second_large <- coeftest(tab8_Ac4_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab8_Ac2_first_large <- plm(npostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                            data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

# muninr clustered st. errors
tab8_Ac2_coef_first_large <- coeftest(tab8_Ac2_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

#Column 3
tab8_Ac3_first_large <- plm(npostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                            data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab8_Ac3_coef_first_large <- coeftest(tab8_Ac3_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab8_Ac4_first_large <- plm(npostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                              popdens + foreignshare + youngshare +oldshare , data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab8_Ac4_coef_first_large <- coeftest(tab8_Ac4_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#- Part 1 (second stage) of Table A6 -
# ---> has to be done manually
tab8_models_second_large <- list(tab8_Ac1_large, tab8_Ac2_second_large, tab8_Ac3_second_large, tab8_Ac4_second_large)
tab8_coefs_second_large <- list(tab8_Ac1_coef_large[,2], tab8_Ac2_coef_second_large[,2], tab8_Ac3_coef_second_large[,2], tab8_Ac4_coef_second_large[,2])

stargazer(
  tab8_models_second_large,
  se = tab8_coefs_second_large,
  title = "Turnout Nominal Costs: Second Stage Regressions for Large Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  type="text",
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 

#- Part 2 (first stage) of Table A6 -
# ---> has to be done manually
tab8_models_first_large <- list(tab8_Ac1_large, tab8_Ac2_first_large, tab8_Ac3_first_large, tab8_Ac4_first_large)
tab8_coefs_first_large <- list(tab8_Ac1_coef_large[,2], tab8_Ac2_coef_first_large[,2], tab8_Ac3_coef_first_large[,2], tab8_Ac4_coef_first_large[,2])

stargazer(
  tab8_models_first_large,
  se = tab8_coefs_first_large,
  type = "text",
  title = "Turnout nominal costs: first stage regressions for Large municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("npostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 


#------------------------------------------------------------------------------------------#
# Part 2c) Large Municipalities ----
#          Table A9: REAL + PRICE CHANGE
#------------------------------------------------------------------------------------------#

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Second stages for all models:

#Column 1
tab9_Ac1_large <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare +oldshare, 
                      data = ballot_days_large, index = c('muninr', 'votedateid'), model = 'within',
                      effect = 'twoways')
# clustered st. errors
tab9_Ac1_coef_large <- coeftest(tab9_Ac1_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 2
tab9_Ac2_second_large <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                               postage + meaninc + medmeaninc + pop +popdens + foreignshare + youngshare + oldshare , data = ballot_days_large,
                             index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# clustered st. errors
tab9_Ac2_coef_second_large <- coeftest(tab9_Ac2_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab9_Ac3_second_large <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare |
                               stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare ,
                             data = ballot_days_large,
                             index = c('muninr', 'votedateid'),
                             model = "within",
                             effect = "twoways")

tab9_Ac3_coef_second_large <-coeftest(tab9_Ac3_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab9_Ac4_second_large <- plm(turnout ~ rpostageprice + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare|
                               postage+stampricechange +meaninc +medmeaninc+ pop +popdens + foreignshare + youngshare +oldshare , data = ballot_days_large,
                             index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# clustered st. errors
tab9_Ac4_coef_second_large <- coeftest(tab9_Ac4_second_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# First stages for all models:

#Column 2
tab9_Ac2_first_large <- plm(rpostageprice ~ postage + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                            data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")
# muninr clustered st. errors
tab9_Ac2_coef_first_large <- coeftest(tab9_Ac2_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 3
tab9_Ac3_first_large <- plm(rpostageprice ~ stampricechange + meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
                            data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab9_Ac3_coef_first_large <- coeftest(tab9_Ac3_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#Column 4
tab9_Ac4_first_large <- plm(rpostageprice ~ postage + stampricechange + meaninc + medmeaninc + pop +
                              popdens + foreignshare + youngshare +oldshare , data = ballot_days_large,
                            index = c('muninr', 'votedateid'), model = "within", effect ="twoways")

tab9_Ac4_coef_first_large <- coeftest(tab9_Ac4_first_large, vcov=function(x) vcovHC(x, cluster="group", type="HC1")) 

#- Part 2 (second stage) of Table A7 -
# ---> has to be done manually
tab9_models_second_large <- list(tab9_Ac1_large, tab9_Ac2_second_large, tab9_Ac3_second_large, tab9_Ac4_second_large)
tab9_coefs_second_large <- list(tab9_Ac1_coef_large[,2], tab9_Ac2_coef_second_large[,2], tab9_Ac3_coef_second_large[,2], tab9_Ac4_coef_second_large[,2])

stargazer(
  tab9_models_second_large,
  se = tab9_coefs_second_large,
  title = "Turnout real Costs: Second Stage Regressions for Large Municipalities",
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp Price",
    "Prepaid Postage, Stamp Price"),
  omit = c("meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare"),
  type="text"
) 

#- Part 2 (first stage) of Table A7 -
# ---> has to be done manually
tab9_models_first_large <- list(tab9_Ac1_large, tab9_Ac2_first_large, tab9_Ac3_first_large, tab9_Ac4_first_large)
tab9_coefs_first_large <- list(tab9_Ac1_coef_large[,2], tab9_Ac2_coef_first_large[,2], tab9_Ac3_coef_first_large[,2], tab9_Ac4_coef_first_large[,2])

stargazer(
  tab9_models_first_large,
  se = tab9_coefs_first_large,
  type = "text",
  title = "Turnout real costs: first stage regressions for All municipalities" ,
  column.labels = c(
    "-",
    "Prepaid Postage",
    "Stamp price",
    "Prepaid postage, stamp price"),
  omit = c("rpostageprice", "meaninc", "medmeaninc", "pop", "popdens", "foreignshare", "youngshare","oldshare")
) 

##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Part 3: Appendix: Potential determinants of Prepaid Postage ----
##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A


schelkschneit_all_ballots <-readRDS("data/Schelker_Schneiter_1.Rds")
all_ballots_final <-readRDS("data/ballot_days_final.rds")


#Data Schelker Schneiter
to_collapse_SS <- schelkschneit_all_ballots %>% 
  dplyr::select(turnout, postage, meaninc, medmeaninc, pop, foreignshare, youngshare, oldshare, popdens)

Tab2.dat <- schelkschneit_all_ballots %>%
  group_by(muninr, year) %>%
  summarise_at(names(to_collapse_SS), mean, na.rm = TRUE)

Tab2.dat <- as.data.frame(Tab2.dat)
Tab2.dat$postage[Tab2.dat$postage>0] <- 1

#Our Data
to_collapse_LW <- all_ballots_final %>% 
  dplyr::select(turnout, postage, meaninc, medmeaninc, pop, foreignshare, youngshare, oldshare,  popdens)

LW_tab2.dat <- all_ballots_final %>%
  group_by(muninr, year) %>%
  summarise_at(names(to_collapse_LW), mean, na.rm = TRUE)

LW_tab2.dat <- as.data.frame(LW_tab2.dat)
LW_tab2.dat$postage[LW_tab2.dat$postage > 0] <- 1

#Schelker Schneiter
tab2_c3_SS <- plm(postage ~ meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
    data = Tab2.dat,
    index = c('muninr', 'year'),
    model = 'within',
    effect = 'twoways')

# municipality level clustered st. errors
tab2_c3_c_SS <- coeftest(tab2_c3_SS, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1")) 


#Our Data
LW_tab2_c3 <- plm(postage ~ meaninc + medmeaninc + pop + popdens + foreignshare + youngshare + oldshare,
    data = LW_tab2.dat,
    index = c('muninr', 'year'),
    model = 'within',
    effect = 'twoways')

LW_tab2_c3_c <- coeftest(LW_tab2_c3, vcov = function(x) vcovHC(x, cluster = "group", type = "HC1")) 

tab2_models<-list(tab2_c3_SS, LW_tab2_c3)
tab2_coefs<-list(tab2_c3_c_SS[,2], LW_tab2_c3_c[,2])

stargazer(
  tab2_models,
  type = "text",
  se = tab2_coefs,
  digits = 3,
  digits.extra = 3,
  column.labels = c(
    "Schelker and Schneiter",
    "Our Data (all cantons)"), 
  intercept.bottom=F, 
  dep.var.labels.include = FALSE,
  covariate.labels = c("Mean income", "Median/mean income", "Population",
                       "Population density", "\\% foreigners", "\\% young", "\\% old"),
  dep.var.caption  = "",
  add.lines = list(c("Municipal FE", "\\ding{51}", "\\ding{51}"),
                   c("Year FE", "\\ding{51}", "\\ding{51}")),
  omit.stat = c("adj.rsq", "f"),
  title            = "Potential determinants of Prepaid postage"#,
  #out="tables/table2_colum3.tex"
)


##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Part 3: Appendix: Descriptive Statistics ----
##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
ballot_days_final$stampuse<- ifelse(ballot_days_final$postage==1, 0, 1)

#real price of stamp
ballot_days_final$rpostageprice <- ballot_days_final$stampuse * ballot_days_final$rstamp

#nominal price of stamp
ballot_days_final$npostageprice <- ballot_days_final$stampuse * ballot_days_final$nstamp

# stamp price change: 2004 --> 85 cents
ballot_days_final$stampricechange <- ifelse(ballot_days_final$postage==0 & ballot_days_final$year==2004, 1, 0)



desc_stat <- ballot_days_final %>% ungroup() %>%
  dplyr::select(pop, popdens, meaninc, medianinc, medmeaninc,
                foreignshare, youngshare, oldshare, 
                #rpostageprice, npostageprice, 
                postage,
                stampricechange) %>%
  as.data.frame()



summary_function <- function(data){
  #overall_min <- round(sapply(data, min, na.rm=T),2)
  #overall_max <- round(sapply(data, max, na.rm=T),2)
  overall_mean <- round(sapply(data, mean, na.rm=T),2)
  overall_sd <- round(sapply(data, sd, na.rm=T),2)
  
  cbind(overall_mean, overall_sd) %>% as.data.frame
  
}

overall <- summary_function(desc_stat) %>% tibble::rownames_to_column("Variable")
untreated <- summary_function(desc_stat %>% filter(postage==0))%>% tibble::rownames_to_column("Variable")
treated <- summary_function(desc_stat %>% filter(postage==1))%>% tibble::rownames_to_column("Variable")

all_descs <- merge(overall, untreated, "Variable") %>%
  merge(treated, "Variable")

#rename Variable
all_descs$Variable[all_descs$Variable=="foreignshare"] <- "\\% foreign"
all_descs$Variable[all_descs$Variable=="meaninc"] <- "Mean Income"
all_descs$Variable[all_descs$Variable=="medianinc"] <- "Median Income"
all_descs$Variable[all_descs$Variable=="medmeaninc"] <- "Median / Mean Income"
all_descs$Variable[all_descs$Variable=="oldshare"] <- "\\% aged"
all_descs$Variable[all_descs$Variable=="pop"] <- "Population Size"
all_descs$Variable[all_descs$Variable=="popdens"] <- "Population Density"
all_descs$Variable[all_descs$Variable=="postage"] <- "Postage"
all_descs$Variable[all_descs$Variable=="stampricechange"] <- "Stamp Price Change"
all_descs$Variable[all_descs$Variable=="youngshare"] <- "\\% young"

#rename Columns
colnames(all_descs) <- c("Variable", "Overall Mean", "Overall Sd", "Untreated Mean", "Untreated sd", "Treated Mean", "Treated sd")
all_descs <- all_descs[c(6,7, 2,3,4, 1, 10, 5, 8, 9),]


table1 <- xtable(all_descs, digits=2, caption="Summary Statistics")
align(table1) <- paste0(c("ll", rep("c", dim(all_descs)[2]-1)), collapse="")
#sink("tables/descriptives.tex")
print(table1,
      include.rownames=FALSE,
      caption.placement="top",
      table.placement ="htp!", 
      hline.after=c(-1,0,nrow(all_descs)), 
      sanitize.text.function = function(x) x)
#sink()





test <- ballot_days_final %>% filter(postage==1)
unique(test$municipality)
length(unique(test$municipality))

add_info <- ballot_days_final %>% ungroup %>% dplyr::select(canton, muninr, municipality) %>% distinct()

treats <- table(ballot_days_final$muninr, ballot_days_final$postage) %>% as.data.frame.matrix() %>%
  tibble::rownames_to_column("muninr")%>%
  setNames(c("muninr", "No Prepaid", "Prepaid")) %>%
  mutate(muninr=as.numeric(muninr),
         obs = `No Prepaid` + Prepaid)%>%
  filter(Prepaid>0) %>%
  left_join(add_info, "muninr") %>%
  dplyr::select(-muninr, municipality,obs, `No Prepaid`, Prepaid)



##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A
# Part 3: Appendix: nr of post boxes ----
##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A##A

load("data/brfk_gde.RData")
bk <- n_bk %>%
  rename(muninr=BFSnr, 
         canton=Canton,
         nrbk = x) %>% filter(canton %in% c("BE", "FR", "LU", "TI", "TG"))

# gmde <- openxlsx::read.xlsx("data/regionalportraits.xlsx", sheet="Sheet1") %>%
#   dplyr::select(muninr, einwohner, density)
# 
# 
# test <- merge(gmde, bk, "muninr", all.x=T)
# fit1a <- lm(nrbk ~ scale(einwohner) , test)
# fit1b <- lm(nrbk ~ scale(density), test)
# fit1c <- lm(nrbk ~ scale(einwohner) + scale(density), test)
# appendix_bk_models<-list(fit1a, fit1b, fit1c)
# stargazer(appendix_bk_models, type="text")

app_dta <- ballot_days_final %>% 
  ungroup %>%
  dplyr::select(muninr, vote_date, pop_total, pop,pop_total, popdens, meaninc, medianinc, mean_steink, med_mean_steink) %>%
  group_by(muninr)%>%
  summarise_all(mean, na.rm=T) 

mun_info <- ballot_days_final %>% 
  ungroup %>%
  dplyr::select(muninr, municipality, canton)%>%
  rename(cantonname = canton)%>%
  distinct()

app_dta <- merge(app_dta, mun_info, by="muninr")%>%
  left_join(bk, "muninr")
dim(app_dta)
which(is.na(app_dta$nrbk))

length(which(is.na(app_dta$nrbk)))
length(which(is.na(app_dta$popdens)))



# Version a
fit1a <- lm(nrbk ~ scale(pop) , app_dta )
fit1b <- lm(nrbk ~ scale(popdens), app_dta)
fit1c <- lm(nrbk ~ scale(pop) + scale(popdens), app_dta)

appendix_bk_models<-list(fit1a, fit1b, fit1c)
stargazer(appendix_bk_models, type="text")


stargazer(appendix_bk_models, type="text",
          digits = 3,
          digits.extra = 3,
          column.labels = c(
            "Model 1",
            "Model 2", 
            "Model 3"),
          #intercept.bottom=F, 
          dep.var.caption  = "",
          dep.var.labels.include = FALSE,
          notes="Population Size in 1000",
          covariate.labels = c("Population Size", "Population Density"),
          title            = "Importance of Population Density vs. Population Size, both z-transformed",
          omit.stat = c("f"),
          out="tables/table_a12a.tex")

#Robust?
fit1a <- lm(nrbk ~ scale(pop) , app_dta %>% filter(muninr!=351))
fit1b <- lm(nrbk ~ scale(popdens), app_dta%>% filter(muninr!=351))
fit1c <- lm(nrbk ~ scale(pop) + scale(popdens), app_dta%>% filter(muninr!=351))

appendix_bk_models<-list(fit1a, fit1b, fit1c)
stargazer(appendix_bk_models, type="text")

#Version b
fit1a <- lm(scale(nrbk) ~ scale(pop) , app_dta)
fit1b <- lm(scale(nrbk) ~ scale(popdens), app_dta)
fit1c <- lm(scale(nrbk) ~ scale(pop) + scale(popdens), app_dta)

appendix_bk_models<-list(fit1a, fit1b, fit1c)
stargazer(appendix_bk_models, type="text")


stargazer(appendix_bk_models, type="text",
          digits = 3,
          digits.extra = 3,
          column.labels = c(
            "Model 1",
            "Model 2", 
            "Model 3"),
          #intercept.bottom=F, 
          dep.var.caption  = "",
          dep.var.labels.include = FALSE,
          notes="Population Size in 1000",
          covariate.labels = c("Population Size", "Population Density"),
          title            = "Importance of Population Density vs. Population Size, both z-transformed", #)
          omit.stat = c("f"),
          out="tables/table_a12b.tex")

#Robust?
fit1a <- lm(scale(nrbk) ~ scale(pop) , app_dta %>% filter(muninr!=351))
fit1b <- lm(scale(nrbk) ~ scale(popdens), app_dta%>% filter(muninr!=351))
fit1c <- lm(scale(nrbk) ~ scale(pop) + scale(popdens), app_dta%>% filter(muninr!=351))

appendix_bk_models<-list(fit1a, fit1b, fit1c)
stargazer(appendix_bk_models, type="text")

plot_df <- app_dta

plot_df[which(is.na(plot_df$canton)),]

library(ggrepel)   # CRAN v0.9.1
p1 <- ggplot(plot_df, aes(x=(pop_total), y=nrbk))+
  geom_point(shape=18, size=3, alpha=.5)+
  #scale_shape_manual(values=c(16, 17, 1, 18, 5))+
  geom_smooth(method="lm", size=.1, se=T, color="black")+
  #geom_text_repel(data=subset(plot_df, nrbk>300), aes(x=pop_total, y=nrbk, label=municipality), size=2)+
  # geom_text_repel(data=subset(plot_df, popdens>2000), aes(x=popdens, y=nrbk, label=municipality), size=2)+
  #facet_wrap(~canton)+
  scale_y_continuous(limits=c(0, 350))+
  labs(y="Number of mailboxes\n", x="Population Size", caption=paste("n = ", nrow(plot_df)))+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text = element_text(size=8),
        axis.title = element_text(size=14))

ggsave(p1, filename="plots/nrbk_popsize.pdf", device="pdf", width=9.708, height=6)


















