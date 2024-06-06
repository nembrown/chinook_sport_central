library(tidyverse)
library(odbc)
library(lubridate)
library(ggnewscale)
library(glmmTMB)
library(DHARMa)
library(fitdistrplus)
library(lme4)
library(bbmle)
library(SuppDists)
library(MuMIn)

#Need to run wrangle_data first and then you get Sport_mark_rate_finescale_combined

source("wrangle_data.R")


Season_south<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023)) %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_south_no_nas<-Season_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))

#Season_model_full<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=gaussian, data = Season_south_no_nas)
Season_model_full<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old*season*creel_effort,  family=gaussian, data = Season_south_no_nas)
summary(Season_model_full)
res <- simulateResiduals(Season_model_full, plot = T, quantreg=T)

#poisson
#Season_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=poisson, data = Season_south_no_nas)
Season_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old*season*creel_effort,  family=poisson, data = Season_south_no_nas)
res_pois <- simulateResiduals(Season_model_full_poisson, plot = T, quantreg=T)
summary(Season_model_full_poisson)

#gamma
#Season_model_full_gamma<- glm(formula = (catch_estimate+3) ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
Season_model_full_gamma<- glm(formula = (catch_estimate+3) ~creel_plus_summer*status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Season_model_full_gamma, plot = T, quantreg=T)
summary(Season_model_full_gamma)

AICtab(Season_model_full,Season_model_full_poisson, Season_model_full_gamma)

#drop_ a single term from the full interaction model:
Season_model_gamma_drop_status<- glm(formula = catch_estimate + 1 ~creel_plus_summer*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_status <- simulateResiduals(Season_model_gamma_drop_status, plot = T, quantreg=T)
summary(Season_model_gamma_drop_status)
#
# Season_model_gamma_drop_kept<- glm(formula = catch_estimate + 1 ~creel_plus_summer*mark_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
# res_gam_drop_kept <- simulateResiduals(Season_model_gamma_drop_kept, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept)
#
# Season_model_gamma_drop_mark<- glm(formula = catch_estimate + 1 ~creel_plus_summer*kept_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
# res_gam_drop_mark <- simulateResiduals(Season_model_gamma_drop_mark, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_mark)

#Season_model_gamma_drop_fishery<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
Season_model_gamma_drop_fishery<- glm(formula = catch_estimate+3 ~creel_plus_summer*status*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_fishery <- simulateResiduals(Season_model_gamma_drop_fishery, plot = T, quantreg=T)
summary(Season_model_gamma_drop_fishery)

#Season_model_gamma_drop_season<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
Season_model_gamma_drop_season<- glm(formula = catch_estimate+3 ~creel_plus_summer*status*finescale_fishery_old*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_season <- simulateResiduals(Season_model_gamma_drop_season, plot = T, quantreg=T)
summary(Season_model_gamma_drop_season)

#Season_model_gamma_drop_effort<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south_no_nas)
Season_model_gamma_drop_effort<- glm(formula = catch_estimate+3 ~creel_plus_summer*status*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_effort <- simulateResiduals(Season_model_gamma_drop_effort, plot = T, quantreg=T)
summary(Season_model_gamma_drop_effort)

#AICtab(Season_model_full_gamma,  Season_model_gamma_drop_fishery, Season_model_gamma_drop_kept, Season_model_gamma_drop_mark,  Season_model_gamma_drop_season, Season_model_gamma_drop_effort)

AICtab(Season_model_full_gamma,  Season_model_gamma_drop_fishery, Season_model_gamma_drop_status,  Season_model_gamma_drop_season, Season_model_gamma_drop_effort)


#drop_ kept is the best... try sequentially dropping terms:

Season_model_gamma_drop_kept_mark<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_mark <- simulateResiduals(Season_model_gamma_drop_kept_mark, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_mark)

Season_model_gamma_drop_kept_season<- glm(formula = catch_estimate + 1 ~creel_plus_summer*mark_status*finescale_fishery_old*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_season <- simulateResiduals(Season_model_gamma_drop_kept_season, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_season)

Season_model_gamma_drop_kept_fishery<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_fishery <- simulateResiduals(Season_model_gamma_drop_kept_fishery, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_fishery)

Season_model_gamma_drop_kept_effort<- glm(formula = catch_estimate+1 ~creel_plus_summer*mark_status*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort <- simulateResiduals(Season_model_gamma_drop_kept_effort, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort)

AICtab(Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_mark, Season_model_gamma_drop_kept_season, Season_model_gamma_drop_kept_fishery, Season_model_gamma_drop_kept_effort)
#####dropping kept only is the best - maybe efffort

##Any further terms? no.
#####kept and effort only.

Season_model_gamma_drop_kept_effort_mark<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_mark_effort <- simulateResiduals(Season_model_gamma_drop_kept_effort_mark, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_mark)

Season_model_gamma_drop_kept_effort_fishery<- glm(formula = catch_estimate+1 ~creel_plus_summer*season*mark_status,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_effort_fishery <- simulateResiduals(Season_model_gamma_drop_kept_effort_fishery, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_fishery)

Season_model_gamma_drop_kept_effort_season<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*mark_status,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_effort_season <- simulateResiduals(Season_model_gamma_drop_kept_effort_season, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_season)

AICtab(Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_effort, Season_model_gamma_drop_kept_effort_mark, Season_model_gamma_drop_kept_effort_fishery, Season_model_gamma_drop_kept_effort_season)


###full model status:
Season_model_gamma_full<- glm(formula = catch_estimate+3 ~creel_plus_summer*finescale_fishery_old*season*status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)
dd<-dredge(Season_model_gamma_full, fixed= ~ creel_plus_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
summary(get.models(dd, 1)[[1]])

Season_model_gamma_full_spec<- glm(formula = catch_estimate+3 ~creel_plus_summer+finescale_fishery_old+season+status+
                                     creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:status+
                                     finescale_fishery_old:season + finescale_fishery_old:status + season:status ,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort_spec <- simulateResiduals(Season_model_gamma_full_spec, plot = T, quantreg=T)
summary(Season_model_gamma_full_spec)
AICtab(Season_model_gamma_full_spec, Season_model_gamma_full, Season_model_gamma_drop_status)



#Now changing around model specification:
#First find the model with drop kept and effort that is the best.Then add back in effort.
Season_model_gamma_drop_kept_effort_2<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*mark_status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)
dd<-dredge(Season_model_gamma_drop_kept_effort_2, fixed= ~ creel_plus_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
summary(get.models(dd, 1)[[1]])

#Model with both dropped kept and effort that fits the best:
Season_model_gamma_drop_kept_effort_spec<- glm(formula = catch_estimate+1 ~creel_plus_summer+finescale_fishery_old+season+mark_status+
                                                 creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
                                                 finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
                                                 creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season ,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort_spec <- simulateResiduals(Season_model_gamma_drop_kept_effort_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort_spec)

#Make sure the specifications is better than the full interactive model
AICtab(Season_model_gamma_drop_kept_effort, Season_model_gamma_drop_kept_effort_spec)

# try adding effort back in
Season_model_gamma_drop_kept_2<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*creel_effort*mark_status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)

#fix the factors that we know are the best here
dd2<-dredge(Season_model_gamma_drop_kept_2, fixed = ~
              creel_plus_summer+finescale_fishery_old+season+mark_status+
              creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
              finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
              creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season )
subset(dd2, delta < 2)
plot(dd2, labAsExpr = TRUE)
summary(get.models(dd2, 1)[[1]])

#Try the model with effort added back in:
Season_model_gamma_drop_kept_spec<- glm(formula = catch_estimate+1 ~creel_plus_summer+finescale_fishery_old+season+creel_effort+mark_status+
                                          creel_plus_summer:creel_effort + creel_effort:finescale_fishery_old + creel_effort:mark_status + creel_effort:season +
                                          creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
                                          finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
                                          creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season +
                                          creel_plus_summer:creel_effort:mark_status + creel_effort:finescale_fishery_old:season + creel_effort:mark_status:season  ,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(Season_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_spec)

AICtab(Season_model_gamma_drop_kept_spec, Season_model_gamma_drop_kept_effort_spec, Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_effort)
#adding effort does improve the model.



#
# #try adding kept status back in:
# Season_model_gamma_spec<- glm(formula = catch_estimate+3 ~creel_plus_summer+finescale_fishery_old+season+creel_effort+mark_status+ kept_status+
#                                 creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:creel_effort +creel_plus_summer:mark_status+
#                                 finescale_fishery_old:season + finescale_fishery_old:creel_effort+
#                                 season:creel_effort + season:mark_status +
#                                 creel_effort:mark_status +
#                                 creel_plus_summer:finescale_fishery_old:creel_effort +creel_plus_summer:creel_effort:mark_status+
#                                 finescale_fishery_old:season:creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
# res_gam_spec <- simulateResiduals(Season_model_gamma_spec, plot = T, quantreg=T)
# summary(Season_model_gamma_spec)
#
# AICtab(Season_model_gamma_drop_kept_mark, Season_model_gamma_drop_kept_mark_spec, Season_model_gamma_drop_kept_spec, Season_model_gamma_spec, Season_model_gamma_drop_kept)
# #kept status should be dropped fully
#


### Selected model:
Season_model_gamma_drop_kept_spec
res_gam_drop_kept_spec <- simulateResiduals(Season_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort_spec)

testDispersion(Season_model_gamma_drop_kept_spec)
simulationOutput <- simulateResiduals(fittedModel = Season_model_gamma_drop_kept_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_south_no_nas$creel_plus_summer)

plotResiduals(simulationOutput, form = na.omit(Season_south$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_south$season))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
# plotConventionalResiduals(fittedModel)
plot(simulationOutput, quantreg = T)



#### Adding data back in post modelling
#Adding predicted data
Season_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))
Season_south_old_new<-predict.glm(Season_model_gamma_full_spec, newdata =  Season_south_old, type = "response")
Season_south_old_new_2<-Season_south_old %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)



#### NBC



### NBC AABM modelling

Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2013:2023)) %>% filter(finescale_fishery_old == "NBC AABM S")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

North_aabm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status*season*historic_effort,  family=gaussian, data = Season_north_aabm_no_nas)
summary(North_aabm_model_full)
res <- simulateResiduals(North_aabm_model_full, plot = T, quantreg=T)

#poisson
North_aabm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*status*season*historic_effort,  family=poisson, data = Season_north_aabm_no_nas)
res_pois <- simulateResiduals(North_aabm_model_full_poisson, plot = T, quantreg=T)
summary(North_aabm_model_full_poisson)

#gamma
North_aabm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status*season*historic_effort,  family=Gamma(link = "log"), data=Season_north_aabm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(North_aabm_model_full_gamma, plot = T, quantreg=T)
summary(North_aabm_model_full_gamma)

AICtab(North_aabm_model_full,North_aabm_model_full_poisson, North_aabm_model_full_gamma)

#gamma is the best

#Now changing around model specification:
North_aabm_model_full_gamma<- glm(formula = catch_estimate+1 ~historic_summer*season*historic_effort*status,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas, na.action = na.fail)
dd<-dredge(North_aabm_model_full_gamma, fixed= ~ historic_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
#
summary(get.models(dd, 1)[[1]])

#The model with AIC <2 factors added back in:
North_aabm_model_full_gamma_spec<- glm(formula = catch_estimate+1 ~historic_summer+season+status+
                                         historic_effort*historic_summer + historic_effort*status +
                                         historic_summer*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(North_aabm_model_full_gamma_spec, plot = T, quantreg=T)
summary(North_aabm_model_full_gamma_spec)

AICtab(North_aabm_model_full_gamma, North_aabm_model_full_gamma_spec)



### Selected model:

testDispersion(North_aabm_model_full_gamma_spec)
simulationOutput <- simulateResiduals(fittedModel = North_aabm_model_full_gamma_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_north_aabm_no_nas$historic_summer)

plotResiduals(simulationOutput, form = na.omit(Season_north_aabm$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_north_aabm$season))


#### Adding data back in post modelling
#Adding predicted data
Season_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_old_new<-predict.glm(North_aabm_model_full_gamma_spec, newdata =  Season_north_aabm_old, type = "response")
Season_north_aabm_old_new_2<-Season_north_aabm_old %>%   mutate(catch_estimate_predicted = Season_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_aabm_combined<- rbind(Season_north_aabm_old_new_2, Season_north_aabm2)




