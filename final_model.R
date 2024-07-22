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



# Southern BC Spring and Fall iREC only-------------------------------------------------------------

Sport_mark_rate_finescale_combined<-read_rds("Sport_mark_rate_finescale_combined.RDS")


### Data needed
Season_south_sf<-Sport_mark_rate_finescale_combined %>%
  filter(YEAR %in% c(2013:2023))%>%
  filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
  filter(season %in% c("spring", "fall"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_south_no_nas<-Season_south_sf %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))
write_rds(Season_south_no_nas, "Season_south_no_nas.RDS")


### Model chosen
Season_model_gamma_full_spec<- glm(formula =catch_estimate + 3 ~ finescale_fishery_old + season +
                                     status + creel_plus_summer:finescale_fishery_old + creel_plus_summer:status +
                                     finescale_fishery_old:season + 1 + creel_plus_summer,  family=Gamma(link = "log"), data = Season_south_no_nas)

# Season_model_full_gamma_glmm_spec<- glmmTMB(formula = (catch_estimate + 3) ~ creel_plus_summer + season + status +
#                                               (1 | finescale_fishery_old) + creel_plus_summer:status,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)

#summary(Season_model_gamma_full_spec)
#### Adding data back in post modelling
#Adding predicted data
Season_south_old_sf<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("spring", "fall"))
Season_south_old_new<-predict(Season_model_gamma_full_spec, newdata =  Season_south_old_sf, type = "response")
Season_south_old_new_2<-Season_south_old_sf %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south_sf %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)
write_rds(Season_south_combined, "Season_south_combined.RDS")



# Southern BC Summer ------------------------------------------------------

Summer_south<-Sport_mark_rate_finescale_combined %>%
  filter(YEAR %in% c(2013:2023))%>%
  filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
  filter(season %in% c("summer"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_south_no_nas<-Summer_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))
write_rds(Summer_south_no_nas, "Summer_south_no_nas.RDS")

Summer_model_gamma_full_spec<- glm(formula = catch_estimate + 3 ~ finescale_fishery_old + status +
                                     creel_plus_summer:status + finescale_fishery_old:status +
                                     1 + creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south_no_nas)


Summer_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("summer"))
Summer_south_old_new<-predict.glm(Summer_model_gamma_full_spec, newdata =  Summer_south_old, type = "response")
Summer_south_old_new_2<-Summer_south_old %>%   mutate(catch_estimate_predicted = Summer_south_old_new)

Summer_south2<-Summer_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_south_combined<- rbind(Summer_south_old_new_2, Summer_south2)
write_rds(Summer_south_combined, "Summer_south_combined.RDS")


# NBC AABM Fall and Spring---------------------------------------------------------------------
### Data needed
Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "NBC AABM S") %>%
  filter(season %in% c("spring", "fall"))
Season_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))

write_rds(Season_north_aabm_no_nas, "Season_north_aabm_no_nas.RDS")

###Chosen model
North_aabm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ season + status + historic_summer:season +
                                         1 + historic_summer,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)

#### Adding data back in post modelling
#Adding predicted data
Season_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")%>%
  filter(season %in% c("spring", "fall"))
Season_north_aabm_old_new<-predict.glm(North_aabm_model_full_gamma_spec, newdata =  Season_north_aabm_old, type = "response")
Season_north_aabm_old_new_2<-Season_north_aabm_old %>%   mutate(catch_estimate_predicted = Season_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_aabm_combined<- rbind(Season_north_aabm_old_new_2, Season_north_aabm2)
write_rds(Season_north_aabm_combined, "Season_north_aabm_combined.RDS")

# NBC AABM Summer---------------------------------------------------------------------
### Data needed

Summer_north_aabm<-Sport_mark_rate_finescale_combined%>%
  filter(YEAR %in% c(2015:2023)) %>%
  filter(finescale_fishery_old == "NBC AABM S")%>%
  filter( season=="summer")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_north_aabm_no_nas<-Summer_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))
write_rds(Summer_north_aabm_no_nas, "Summer_north_aabm_no_nas.RDS")

###Chosen model
Summer_north_aabm_model_gamma_spec<- glm(formula = catch_estimate + 1 ~ status + historic_summer:status +
                                           1 + historic_summer,  family=Gamma(link = "log"), data = Summer_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(Summer_north_aabm_model_gamma_spec, plot = T, quantreg=T)


#Adding predicted data
Summer_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")%>%
  filter(season=="summer")
Summer_north_aabm_old_new<-predict.glm(Summer_north_aabm_model_gamma_spec, newdata =  Summer_north_aabm_old, type = "response")
Summer_north_aabm_old_new_2<-Summer_north_aabm_old %>%   mutate(catch_estimate_predicted = Summer_north_aabm_old_new)

Summer_north_aabm2<-Summer_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_north_aabm_combined<- rbind(Summer_north_aabm_old_new_2, Summer_north_aabm2)
write_rds(Summer_north_aabm_combined, "Summer_north_aabm_combined.RDS")




# NBC ISBM Spring and Fall ----------------------------------------------------------------

### Data needed

Season_nbc_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("spring", "fall"))
Season_nbc_isbm_no_nas<-Season_north_isbm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))
write_rds(Season_nbc_isbm_no_nas, "Season_nbc_isbm_no_nas.RDS")


###Chosen model
nbc_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ season + status + season:status +
                                       1 + historic_summer,  family=Gamma(link = "log"), data = Season_nbc_isbm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(nbc_isbm_model_full_gamma_spec, plot = T, quantreg=T)


#### Adding data back in post modelling
#Adding predicted data
Season_north_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2008:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("spring", "fall"))
Season_north_isbm_old_new<-predict.glm(nbc_isbm_model_full_gamma_spec, newdata =  Season_north_isbm_old, type = "response")
Season_north_isbm_old_new_2<-Season_north_isbm_old %>%   mutate(catch_estimate_predicted = Season_north_isbm_old_new)

Season_north_isbm2<-Season_north_isbm  %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_isbm_combined<- rbind(Season_north_isbm_old_new_2, Season_north_isbm2)
write_rds(Season_north_isbm_combined, "Season_north_isbm_combined.RDS")


# NBC ISBM Summer ----------------------------------------------------------------

### Data needed
Summer_north_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("summer"))
Summer_north_isbm_no_nas<-Summer_north_isbm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))
write_rds(Summer_north_isbm_no_nas, "Summer_north_isbm_no_nas.RDS")

#Chosen model
Summer_north_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~ status + 1 + historic_summer,  family=Gamma(link = "log"), data = Summer_north_isbm_no_nas)
res_spec <- simulateResiduals(Summer_north_isbm_model_full_gamma_spec, plot = T, quantreg=T)


#### Adding data back in post modelling
#Adding predicted data
Summer_north_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2008:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC ISBM S")%>%
  filter(season %in% c("summer"))
Summer_north_isbm_old_new<-predict.glm(Summer_north_isbm_model_full_gamma_spec, newdata =  Summer_north_isbm_old, type = "response")
Summer_north_isbm_old_new_2<-Summer_north_isbm_old %>%   mutate(catch_estimate_predicted = Summer_north_isbm_old_new)

Summer_north_isbm2<-Summer_north_isbm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_north_isbm_combined<- rbind(Summer_north_isbm_old_new_2, Summer_north_isbm2)
write_rds(Summer_north_isbm_combined, "Summer_north_isbm_combined.RDS")

# CBC ISBM ----------------------------------------------------------------
Season_cbc_isbm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2015:2023)) %>% filter(finescale_fishery_old == "CBC S")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_cbc_isbm_no_nas<-Season_cbc_isbm %>% drop_na(any_of(c("historic_summer", "status")))
write_rds(Season_cbc_isbm_no_nas, "Season_cbc_isbm_no_nas.RDS")

#Chosen model:
cbc_isbm_model_full_gamma_spec<- glm(formula = catch_estimate + 1 ~  status + 1 + historic_summer,  family=Gamma(link = "log"), data = Season_cbc_isbm_no_nas)


#### Adding data back in post modelling
#Adding predicted data
Season_cbc_isbm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2014)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "CBC S")
Season_cbc_isbm_old_new<-predict.glm(cbc_isbm_model_full_gamma_spec, newdata =  Season_cbc_isbm_old, type = "response")
Season_cbc_isbm_old_new_2<-Season_cbc_isbm_old %>%   mutate(catch_estimate_predicted = Season_cbc_isbm_old_new)

Season_cbc_isbm2<-Season_cbc_isbm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_cbc_isbm_combined<- rbind(Season_cbc_isbm_old_new_2, Season_cbc_isbm2)
write_rds(Season_cbc_isbm_combined, "Season_cbc_isbm_combined.RDS")




# Combining all modelled data into season south combined ------------------
models_combined<-rbind(Season_south_combined, Summer_south_combined)
models_combined<- rbind(models_combined, Season_north_aabm_combined)
models_combined<- rbind(models_combined, Season_north_isbm_combined)
models_combined<- rbind(models_combined, Season_cbc_isbm_combined)
models_combined<- rbind(models_combined, Summer_north_aabm_combined)
models_combined<- rbind(models_combined, Summer_north_isbm_combined)

write_rds(models_combined, "models_combined.RDS")


# Combining all modelled data predicted only ------------------
models_combined_predicted<-models_combined %>% filter(pred_cat == "predicted")
write_rds(models_combined_predicted, "models_combined_predicted.RDS")

## Marked kept only
models_combined_predicted_marked_kept<-models_combined_predicted %>% filter(status=="marked_Kept_total")
