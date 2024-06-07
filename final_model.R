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



# Southern BC -------------------------------------------------------------

### Data needed
Season_south<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023)) %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))
Season_south_no_nas<-Season_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))


### Model chosen
Season_model_gamma_full_spec<- glm(formula = catch_estimate+3 ~creel_plus_summer+finescale_fishery_old+season+status+
                                     creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:status+
                                     finescale_fishery_old:season + finescale_fishery_old:status + season:status ,  family=Gamma(link = "log"), data = Season_south_no_nas)


#### Adding data back in post modelling
#Adding predicted data
Season_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))
Season_south_old_new<-predict.glm(Season_model_gamma_full_spec, newdata =  Season_south_old, type = "response")
Season_south_old_new_2<-Season_south_old %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)





# NBC ---------------------------------------------------------------------
### Data needed
Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2013:2023)) %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))


###Chosen model
North_aabm_model_full_gamma_spec<- glm(formula = catch_estimate+1 ~historic_summer+season+status+
                                         historic_effort*historic_summer + historic_effort*status +
                                         historic_summer*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)


#### Adding data back in post modelling
#Adding predicted data
Season_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_old_new<-predict.glm(North_aabm_model_full_gamma_spec, newdata =  Season_north_aabm_old, type = "response")
Season_north_aabm_old_new_2<-Season_north_aabm_old %>%   mutate(catch_estimate_predicted = Season_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_aabm_combined<- rbind(Season_north_aabm_old_new_2, Season_north_aabm2)


