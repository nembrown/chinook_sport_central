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


### Model chosen
Season_model_gamma_full_spec<- glm(formula =catch_estimate + 3 ~ finescale_fishery_old + season +
                                     status + creel_plus_summer:finescale_fishery_old + creel_plus_summer:status +
                                     finescale_fishery_old:season + 1 + creel_plus_summer,  family=Gamma(link = "log"), data = Season_south_no_nas)



#### Adding data back in post modelling
#Adding predicted data
Season_south_old_sf<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("spring", "fall"))
Season_south_old_new<-predict.glm(Season_model_gamma_full_spec, newdata =  Season_south_old_sf, type = "response")
Season_south_old_new_2<-Season_south_old_sf %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south_sf %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)



# Southern BC Summer ------------------------------------------------------

Summer_south<-Sport_mark_rate_finescale_combined %>%
  filter(YEAR %in% c(2013:2023))%>%
  filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
  filter(season %in% c("summer"))


#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Summer_south_no_nas<-Summer_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))

Summer_model_gamma_full_spec<- glm(formula = catch_estimate + 3 ~ finescale_fishery_old + status +
                                     creel_plus_summer:status + finescale_fishery_old:status +
                                     1 + creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south_no_nas)


Summer_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))%>%
  filter(season %in% c("summer"))
Summer_south_old_new<-predict.glm(Summer_model_gamma_full_spec, newdata =  Summer_south_old, type = "response")
Summer_south_old_new_2<-Summer_south_old %>%   mutate(catch_estimate_predicted = Summer_south_old_new)

Summer_south2<-Summer_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Summer_south_combined<- rbind(Summer_south_old_new_2, Summer_south2)


# NBC ---------------------------------------------------------------------
### Data needed
Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2013:2023)) %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "status", "finescale_fishery_old", "season", "historic_effort")))


###Chosen model
North_aabm_model_full_gamma_spec<- glm(formula = catch_estimate+1 ~season + status + 1 + historic_summer,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)


#### Adding data back in post modelling
#Adding predicted data
Season_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_old_new<-predict.glm(North_aabm_model_full_gamma_spec, newdata =  Season_north_aabm_old, type = "response")
Season_north_aabm_old_new_2<-Season_north_aabm_old %>%   mutate(catch_estimate_predicted = Season_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_aabm_combined<- rbind(Season_north_aabm_old_new_2, Season_north_aabm2)


# Combining all modelled data into season south combined ------------------
Season_south_combined<-rbind(Season_south_combined, Summer_south_combined)
Season_south_combined<- rbind(Season_south_combined, Season_north_aabm_combined)



write_rds(Season_south_combined, "Season_south_combined.RDS")
