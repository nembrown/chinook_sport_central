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
library(pacRecCatch)
library(ROracle)

#Need to run wrangle_data first and then you get Sport_mark_rate_finescale_combined

#source("wrangle_data.R")
Sport_mark_rate_finescale_combined<-wrangle_rec_catch("pacRecCatch_db.yaml")
write_rds(Sport_mark_rate_finescale_combined, "Sport_mark_rate_finescale_combined.RDS")


Sport_mark_rate_finescale<-wrangle_rec_catch("pacRecCatch_db.yaml", by="area")
Sport_mark_rate_finescale_combined<-wrangle_rec_catch("pacRecCatch_db.yaml", by="fishery")



# Seasonal South - Spring and Fall ----------------------------------------

Season_south_sf<-Sport_mark_rate_finescale_combined %>%
                 filter(YEAR %in% c(2013:2023))%>%
                 filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
                 filter(season %in% c("spring", "fall"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_south_no_nas<-Season_south_sf %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))


#take out creel effort
#Make sure likelihoods are the same.
Season_model_full<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old*season,  family=gaussian, data = Season_south_no_nas)
summary(Season_model_full)
res <- simulateResiduals(Season_model_full, plot = T, quantreg=T)

#poisson
Season_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old*season,  family=poisson, data = Season_south_no_nas)
res_pois <- simulateResiduals(Season_model_full_poisson, plot = T, quantreg=T)
summary(Season_model_full_poisson)

#gamma
Season_model_full_gamma<- glm(formula = (catch_estimate+3) ~creel_plus_summer*status*finescale_fishery_old*season,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Season_model_full_gamma, plot = T, quantreg=T)
summary(Season_model_full_gamma)

AICtab(Season_model_full,Season_model_full_poisson, Season_model_full_gamma)
## Gamma is the best

###full model status:
Season_model_gamma_full<- glm(formula = catch_estimate+3 ~creel_plus_summer*finescale_fishery_old*season*status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)
dd<-dredge(Season_model_gamma_full, fixed= ~ creel_plus_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
summary(get.models(dd, 1)[[1]])

Season_model_gamma_full_spec<- glm(formula =catch_estimate + 3 ~ finescale_fishery_old + season +
                                     status + creel_plus_summer:finescale_fishery_old + creel_plus_summer:status +
                                     finescale_fishery_old:season + 1 + creel_plus_summer,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort_spec <- simulateResiduals(Season_model_gamma_full_spec, plot = T, quantreg=T)
summary(Season_model_gamma_full_spec)


testDispersion(Season_model_gamma_full_spec)
simulationOutput <- simulateResiduals(fittedModel = Season_model_gamma_full_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_south_no_nas$creel_plus_summer)


#### Adding data back in post modelling
#Adding predicted data
Season_south_old_sf<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("spring", "fall"))
Season_south_old_new<-predict.glm(Season_model_gamma_full_spec, newdata =  Season_south_old_sf, type = "response")
Season_south_old_new_2<-Season_south_old_sf %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south_sf %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)

#### Testing mixed effects
Season_model_full_gamma_glmm<- glmmTMB(formula = (catch_estimate+3) ~creel_plus_summer*status*season + (1|finescale_fishery_old) + (1|YEAR),  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Season_model_full_gamma_glmm, plot = T, quantreg=T)
summary(Season_model_full_gamma_glmm)

dd<-dredge(Season_model_full_gamma_glmm, fixed= ~ creel_plus_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
summary(get.models(dd, 1)[[1]])

Season_model_full_gamma_glmm_spec<- glmmTMB(formula = (catch_estimate + 3) ~ creel_plus_summer + season + status +
                                              (1 | finescale_fishery_old) + (1 | YEAR) + creel_plus_summer:status,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Season_model_full_gamma_glmm_spec, plot = T, quantreg=T)

AICtab(Season_model_full_gamma_glmm_spec, Season_model_gamma_full_spec)


# Season south - summer only  ---------------------------------------------

Summer_south<-Sport_mark_rate_finescale_combined %>%
  filter(YEAR %in% c(2013:2023))%>%
  filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
  filter(season %in% c("summer"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_south_no_nas<-Summer_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))

Summer_model_full<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old,  family=gaussian, data = Summer_south_no_nas)
summary(Summer_model_full)
res <- simulateResiduals(Summer_model_full, plot = T, quantreg=T)

#poisson
Summer_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*status*finescale_fishery_old,  family=poisson, data = Summer_south_no_nas)
res_pois <- simulateResiduals(Summer_model_full_poisson, plot = T, quantreg=T)
summary(Summer_model_full_poisson)

#gamma
Summer_model_full_gamma<- glm(formula = (catch_estimate) ~creel_plus_summer*status*finescale_fishery_old,  family=Gamma(link = "log"), data=Summer_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Summer_model_full_gamma, plot = T, quantreg=T)
summary(Summer_model_full_gamma)

AICtab(Summer_model_full,Summer_model_full_poisson, Summer_model_full_gamma)

###full model status:
Summer_model_gamma_full<- glm(formula = catch_estimate ~creel_plus_summer*finescale_fishery_old*status,  family=Gamma(link = "log"), data = Summer_south_no_nas, na.action = na.fail)
dd2<-dredge(Summer_model_gamma_full, fixed= ~ creel_plus_summer)
subset(dd2, delta < 2)
plot(dd2, labAsExpr = TRUE)
summary(get.models(dd2, 1)[[1]])

Summer_model_gamma_full_spec<- glm(formula = catch_estimate ~ finescale_fishery_old + status +
                                     creel_plus_summer:finescale_fishery_old +
                                     creel_plus_summer:status +
                                     finescale_fishery_old:status + 1 + creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south_no_nas)
res_gam_effort_summer_spec <- simulateResiduals(Summer_model_gamma_full_spec, plot = T, quantreg=T)
summary(Summer_model_gamma_full_spec)


testDispersion(Summer_model_gamma_full_spec)
simulationOutput <- simulateResiduals(fittedModel = Summer_model_gamma_full_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Summer_south_no_nas$creel_plus_summer)


#### Adding data back in post modelling
#Adding predicted data
Summer_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("summer"))
Summer_south_old_new<-predict.glm(Summer_model_gamma_full_spec, newdata =  Summer_south_old, type = "response")
Summer_south_old_new_2<-Summer_south_old %>%   mutate(catch_estimate_predicted = Summer_south_old_new)

Summer_south2<-Summer_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_south_combined<- rbind(Summer_south_old_new_2, Summer_south2)



# NBC AABM spring, fall ---------------------------------------------------------------

Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>%
                    filter(finescale_fishery_old == "NBC AABM S") %>%
                    filter(season %in% c("spring", "fall"))

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

North_aabm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status*season,  family=gaussian, data = Summer_north_aabm_no_nas)
summary(North_aabm_model_full)
res <- simulateResiduals(North_aabm_model_full, plot = T, quantreg=T)

#poisson
North_aabm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*status*season,  family=poisson, data = Summer_north_aabm_no_nas)
res_pois <- simulateResiduals(North_aabm_model_full_poisson, plot = T, quantreg=T)
summary(North_aabm_model_full_poisson)

#gamma
North_aabm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status*season,  family=Gamma(link = "log"), data=Summer_north_aabm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(North_aabm_model_full_gamma, plot = T, quantreg=T)
summary(North_aabm_model_full_gamma)

AICtab(North_aabm_model_full,North_aabm_model_full_poisson, North_aabm_model_full_gamma)

#gamma is the best

#Now changing around model specification:
North_aabm_model_full_gamma<- glm(formula = catch_estimate+1 ~historic_summer*season*status,  family=Gamma(link = "log"), data = Summer_north_aabm_no_nas, na.action = na.fail)
dd3<-dredge(North_aabm_model_full_gamma, fixed= ~ historic_summer)
subset(dd3, delta < 2)
plot(dd3, labAsExpr = TRUE)
#
summary(get.models(dd3, 1)[[1]])

#The model with AIC <2 factors added back in:
North_aabm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ season + status + historic_summer:status +
                                         1 + historic_summer,  family=Gamma(link = "log"), data = Summer_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(North_aabm_model_full_gamma_spec, plot = T, quantreg=T)
summary(North_aabm_model_full_gamma_spec)

AICtab(North_aabm_model_full_gamma, North_aabm_model_full_gamma_spec)



### Selected model:

testDispersion(North_aabm_model_full_gamma_spec)
simulationOutput <- simulateResiduals(fittedModel = North_aabm_model_full_gamma_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Summer_north_aabm_no_nas$historic_summer)

plotResiduals(simulationOutput, form = na.omit(Season_north_aabm$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_north_aabm$season))


#### Adding data back in post modelling
#Adding predicted data
Summer_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")%>%
  filter(season %in% c("spring", "fall"))
Summer_north_aabm_old_new<-predict.glm(North_aabm_model_full_gamma_spec, newdata =  Summer_north_aabm_old, type = "response")
Summer_north_aabm_old_new_2<-Summer_north_aabm_old %>%   mutate(catch_estimate_predicted = Summer_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_north_aabm_combined<- rbind(Summer_north_aabm_old_new_2, Season_north_aabm2)

# NBC AABM summer only  ---------------------------------------------------------------

Summer_north_aabm<-Sport_mark_rate_finescale_combined%>%
                        filter(YEAR %in% c(2015:2023)) %>%
                        filter(finescale_fishery_old == "NBC AABM S")%>%
                        filter(season=="summer")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_north_aabm_no_nas<-Summer_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

Summer_north_aabm_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status,  family=gaussian, data = Summer_north_aabm_no_nas)
res <- simulateResiduals(Summer_north_aabm_full, plot = T, quantreg=T)

#gamma
Summer_north_aabm_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status,  family=Gamma(link = "log"), data=Summer_north_aabm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Summer_north_aabm_gamma, plot = T, quantreg=T)


AICtab(Summer_north_aabm_full, Summer_north_aabm_gamma)

#Now changing around model specification:
 Summer_north_aabm_model_gamma<- glm(formula = catch_estimate+1 ~historic_summer*status,  family=Gamma(link = "log"), data = Summer_north_aabm_no_nas, na.action = na.fail)
dd3<-dredge( Summer_north_aabm_model_gamma, fixed= ~ historic_summer)
subset(dd3, delta < 2)
plot(dd3, labAsExpr = TRUE)
#
summary(get.models(dd3, 1)[[1]])

#The model with AIC <2 factors added back in:
Summer_north_aabm_model_gamma_spec<- glm(formula = catch_estimate + 1 ~ status + historic_summer:status +
                                         1 + historic_summer,  family=Gamma(link = "log"), data = Summer_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(Summer_north_aabm_model_gamma_spec, plot = T, quantreg=T)


AICtab(Summer_north_aabm_model_gamma,Summer_north_aabm_model_gamma_spec)



#### Adding data back in post modelling
#Adding predicted data
Summer_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")%>%
  filter(season=="summer")
Summer_north_aabm_old_new<-predict.glm(Summer_north_aabm_model_gamma_spec, newdata =  Summer_north_aabm_old, type = "response")
Summer_north_aabm_old_new_2<-Summer_north_aabm_old %>%   mutate(catch_estimate_predicted = Summer_north_aabm_old_new)

Summer_north_aabm2<-Summer_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_north_aabm_combined<- rbind(Summer_north_aabm_old_new_2, Summer_north_aabm2)


# NBC ISBM Spring and Fall  ----------------------------------------------------------------

Season_nbc_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("spring", "fall"))

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_nbc_isbm_no_nas<-Season_nbc_isbm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

nbc_isbm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status*season,  family=gaussian, data = Season_nbc_isbm_no_nas)
summary(nbc_isbm_model_full)
res <- simulateResiduals(nbc_isbm_model_full, plot = T, quantreg=T)

#poisson
nbc_isbm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*status*season,  family=poisson, data = Season_nbc_isbm_no_nas)
res_pois <- simulateResiduals(nbc_isbm_model_full_poisson, plot = T, quantreg=T)
summary(nbc_isbm_model_full_poisson)

#gamma
nbc_isbm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status*season,  family=Gamma(link = "log"), data=Season_nbc_isbm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(nbc_isbm_model_full_gamma, plot = T, quantreg=T)
summary(nbc_isbm_model_full_gamma)

AICtab(nbc_isbm_model_full,nbc_isbm_model_full_poisson, nbc_isbm_model_full_gamma)

#gamma is the best

#Now changing around model specification:
nbc_isbm_model_full_gamma<- glm(formula = catch_estimate+1 ~historic_summer*season*status,  family=Gamma(link = "log"), data = Season_nbc_isbm_no_nas, na.action = na.fail)
dd3<-dredge(nbc_isbm_model_full_gamma, fixed= ~ historic_summer)
subset(dd3, delta < 2)
plot(dd3, labAsExpr = TRUE)
#
summary(get.models(dd3, 1)[[1]])

#The model with AIC <2 factors added back in:
nbc_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ season + status + season:status +
                                       1 + historic_summer,  family=Gamma(link = "log"), data = Season_nbc_isbm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(nbc_isbm_model_full_gamma_spec, plot = T, quantreg=T)
summary(nbc_isbm_model_full_gamma_spec)

AICtab(nbc_isbm_model_full_gamma, nbc_isbm_model_full_gamma_spec)



### Selected model:

testDispersion(nbc_isbm_model_full_gamma_spec)
simulationOutput <- simulateResiduals(fittedModel = nbc_isbm_model_full_gamma_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_nbc_isbm_no_nas$historic_summer)

plotResiduals(simulationOutput, form = na.omit(Season_nbc_isbm$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_nbc_isbm$season))


#### Adding data back in post modelling
#Adding predicted data
Season_nbc_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2008:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("spring", "fall"))
Season_nbc_isbm_old_new<-predict.glm(nbc_isbm_model_full_gamma_spec, newdata =  Season_nbc_isbm_old, type = "response")
Season_nbc_isbm_old_new_2<-Season_nbc_isbm_old %>%   mutate(catch_estimate_predicted = Season_nbc_isbm_old_new)

Season_nbc_isbm2<-Season_nbc_isbm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_nbc_isbm_combined<- rbind(Season_nbc_isbm_old_new_2, Season_nbc_isbm2)

# NBC ISBM Summer ----------------------------------------------------------------

Summer_nbc_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("summer"))

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_nbc_isbm_no_nas<-Summer_nbc_isbm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

summer_nbc_isbm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status,  family=gaussian, data = Summer_nbc_isbm_no_nas)
res <- simulateResiduals(summer_nbc_isbm_model_full, plot = T, quantreg=T)

#poisson
summer_nbc_isbm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*status,  family=poisson, data = Summer_nbc_isbm_no_nas)
res_pois <- simulateResiduals(summer_nbc_isbm_model_full_poisson, plot = T, quantreg=T)

#gamma
summer_nbc_isbm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status,  family=Gamma(link = "log"), data=Summer_nbc_isbm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(summer_nbc_isbm_model_full_gamma, plot = T, quantreg=T)

AICtab(summer_nbc_isbm_model_full,summer_nbc_isbm_model_full_poisson, summer_nbc_isbm_model_full_gamma)

#gamma is the best

#Now changing around model specification:
summer_nbc_isbm_model_full_gamma<- glm(formula = catch_estimate+1 ~historic_summer*status,  family=Gamma(link = "log"), data = Summer_nbc_isbm_no_nas, na.action = na.fail)
dd4<-dredge(summer_nbc_isbm_model_full_gamma, fixed= ~ historic_summer)
subset(dd4, delta < 2)
plot(dd4, labAsExpr = TRUE)
#
summary(get.models(dd4, 1)[[1]])

#The model with AIC <2 factors added back in:
summer_nbc_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ status + 1 + historic_summer,  family=Gamma(link = "log"), data = Summer_nbc_isbm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(summer_nbc_isbm_model_full_gamma_spec, plot = T, quantreg=T)

AICtab(summer_nbc_isbm_model_full_gamma, summer_nbc_isbm_model_full_gamma_spec)


#### Adding data back in post modelling
#Adding predicted data
Summer_nbc_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2008:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("summer"))
Summer_nbc_isbm_old_new<-predict.glm(summer_nbc_isbm_model_full_gamma_spec, newdata =  Summer_nbc_isbm_old, type = "response")
Summer_nbc_isbm_old_new_2<-Summer_nbc_isbm_old %>%   mutate(catch_estimate_predicted = Summer_nbc_isbm_old_new)

Summer_nbc_isbm2<-Summer_nbc_isbm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_nbc_isbm_combined<- rbind(Summer_nbc_isbm_old_new_2, Summer_nbc_isbm2)


# CBC ISBM ----------------------------------------------------------------

Season_cbc_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "CBC S")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_cbc_isbm_no_nas<-Season_cbc_isbm %>% drop_na(any_of(c("historic_summer", "status")))

cbc_isbm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*status,  family=gaussian, data = Season_cbc_isbm_no_nas)
summary(cbc_isbm_model_full)
res <- simulateResiduals(cbc_isbm_model_full, plot = T, quantreg=T)

#poisson
cbc_isbm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*status,  family=poisson, data = Season_cbc_isbm_no_nas)
res_pois <- simulateResiduals(cbc_isbm_model_full_poisson, plot = T, quantreg=T)
summary(cbc_isbm_model_full_poisson)

#gamma
cbc_isbm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*status,  family=Gamma(link = "log"), data=Season_cbc_isbm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(cbc_isbm_model_full_gamma, plot = T, quantreg=T)
summary(cbc_isbm_model_full_gamma)

AICtab(cbc_isbm_model_full,cbc_isbm_model_full_poisson, cbc_isbm_model_full_gamma)

#gamma is the best

#Now changing around model specification:
cbc_isbm_model_full_gamma<- glm(formula = catch_estimate+1 ~historic_summer*status,  family=Gamma(link = "log"), data = Season_cbc_isbm_no_nas, na.action = na.fail)
dd3<-dredge(cbc_isbm_model_full_gamma, fixed= ~ historic_summer)
subset(dd3, delta < 2)
plot(dd3, labAsExpr = TRUE)
#
summary(get.models(dd3, 1)[[1]])

#The model with AIC <2 factors added back in:
cbc_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~  status +
                                         1 + historic_summer,  family=Gamma(link = "log"), data = Season_cbc_isbm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(cbc_isbm_model_full_gamma_spec, plot = T, quantreg=T)
summary(cbc_isbm_model_full_gamma_spec)

AICtab(cbc_isbm_model_full_gamma, cbc_isbm_model_full_gamma_spec)



### Selected model:

testDispersion(cbc_isbm_model_full_gamma_spec)
simulationOutput <- simulateResiduals(fittedModel = cbc_isbm_model_full_gamma_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_cbc_isbm_no_nas$historic_summer)

plotResiduals(simulationOutput, form = na.omit(Season_cbc_isbm$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_cbc_isbm$season))


#### Adding data back in post modelling
#Adding predicted data
Season_cbc_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2008:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "CBC S")
Season_cbc_isbm_old_new<-predict.glm(cbc_isbm_model_full_gamma_spec, newdata =  Season_cbc_isbm_old, type = "response")
Season_cbc_isbm_old_new_2<-Season_cbc_isbm_old %>%   mutate(catch_estimate_predicted = Season_cbc_isbm_old_new)

Season_cbc_isbm2<-Season_cbc_isbm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_cbc_isbm_combined<- rbind(Season_cbc_isbm_old_new_2, Season_cbc_isbm2)




