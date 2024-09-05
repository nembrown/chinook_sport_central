#load packages
library(ROracle)
library(pacRecCatch)
library(tidyverse)
library(odbc)
library(lubridate)
library(ggnewscale)
library(patchwork)
library(DHARMa)
library(lme4)
library(bbmle)
library(SuppDists)
library(MuMIn)

#utils
"%notin%" <- Negate("%in%")

col1 = "#d8e1cf"
col2 = "#438484"
col3 = "lightblue"
col4 = "lightblue4"
col5 = "pink"
col6 = "darkred"
colclear= "#1C00ff00"

resize <- function(g, fig_width=NA, fig_height=NA) {
  g_deparsed <- paste0(deparse(function() g), collapse = '')
  sub_chunk <- paste0("`","``{r sub_chunk_", floor(runif(1) * 10000), ", fig.height=", fig_height, ", fig.width=", fig_width, ", warning=FALSE, echo=FALSE}","\n(", g_deparsed, ")()","\n`","``")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}


Sport_mark_rate_finescale<-wrangle_rec_catch("pacRecCatch_db.yaml", by="area")

Sport_mark_rate_finescale_combined<-wrangle_rec_catch("pacRecCatch_db.yaml", by="fishery")

Season_south_no_nas<-read_rds("Season_south_no_nas.RDS")
Summer_south_no_nas<-read_rds("Summer_south_no_nas.RDS")
Season_north_aabm_no_nas<-read_rds("Season_north_aabm_no_nas.RDS")
Season_north_isbm_no_nas<-read_rds("Season_north_isbm_no_nas.RDS")
Season_cbc_isbm_no_nas<-read_rds("Season_cbc_isbm_no_nas.RDS")
Summer_north_aabm_no_nas<-read_rds("Summer_north_aabm_no_nas.RDS")
Summer_north_isbm_no_nas<-read_rds("Summer_north_isbm_no_nas.RDS")

Season_south_combined<-read_rds("Season_south_combined.RDS")
Summer_south_combined<-read_rds("Summer_south_combined.RDS")
Season_north_aabm_combined<-read_rds("Season_north_aabm_combined.RDS")
Summer_north_aabm_combined<-read_rds("Summer_north_aabm_combined.RDS")
Season_north_isbm_combined<-read_rds("Season_north_isbm_combined.RDS")
Season_cbc_isbm_combined<-read_rds("Season_cbc_isbm_combined.RDS")
Summer_north_isbm_combined<-read_rds("Summer_north_isbm_combined.RDS")

#models_combined<- read_rds("models_combined.RDS")
models_combined<-model_rec_catch(Sport_mark_rate_finescale_combined)


Sport_mark_rate_mrr_past<-models_combined %>% filter(YEAR<2013) %>%
  pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = catch_estimate_predicted) %>%
  mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
         ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total))%>%
  mutate(unmarked_release=1-ukr) %>%
  mutate(mrr_corrected = case_when(
    YEAR %in% c(2005:2007) ~ (marked_Released_total+unmarked_Released_total)/(marked_Kept_total+unmarked_Kept_total+marked_Released_total+unmarked_Released_total),
    YEAR %in% c(2008:2012) & finescale_fishery != "CA JDF S SUMMER" ~ (marked_Released_total+unmarked_Released_total)/(marked_Kept_total+unmarked_Kept_total+marked_Released_total+unmarked_Released_total),
    YEAR %in% c(2008:2012) & finescale_fishery == "CA JDF S SUMMER" ~ mrr,
    TRUE ~ mrr)) %>%
  mutate(unmarked_release_corrected = case_when(
    YEAR %in% c(2005:2007) ~ mrr_corrected,
    YEAR %in% c(2008:2012) & finescale_fishery != "CA JDF S SUMMER" ~ mrr_corrected,
    YEAR %in% c(2008:2012) & finescale_fishery == "CA JDF S SUMMER" ~ unmarked_release,
    TRUE ~ unmarked_release
  )) %>%
  mutate(ukr_corrected = 1-unmarked_release_corrected)

Sport_mark_rate_mrr_past$mrr_corrected[is.na(Sport_mark_rate_mrr_past$mrr_corrected)]<-0
Sport_mark_rate_mrr_past$ukr_corrected[is.na(Sport_mark_rate_mrr_past$ukr_corrected)]<-1

Sport_mark_rate_mrr_past_corrected<-Sport_mark_rate_mrr_past %>% dplyr::select(-mrr, -ukr, -unmarked_release, -unmarked_release_corrected) %>%
  rename(mrr=mrr_corrected, ukr=ukr_corrected)

Sport_mark_rate_mrr_past_corrected_terminal<- Sport_mark_rate_mrr_past_corrected %>%
  mutate(finescale_fishery = fct_recode(finescale_fishery, "TNORTH S" = "NBC ISBM S SUMMER",
                                        "TCENTRAL S" = "CBC S",
                                        "TNGS S" = "NGS S SUMMER",
                                        "TSGS S" = "SGS S SUMMER",
                                        "TWCVI S" = "SWCVI S SUMMER ISBM",
                                        "TJOHN ST S" = "JNST S SUMMER",
                                        "TCA JDF S" = "CA JDF S SUMMER")) %>%
  filter(finescale_fishery %in% c("TNORTH S", "TCENTRAL S", "TNGS S", "TSGS S", "TWCVI S", "TJOHN ST S", "TCA JDF S"))


Sport_mark_rate_mrr_past_corrected<- bind_rows(Sport_mark_rate_mrr_past_corrected, Sport_mark_rate_mrr_past_corrected_terminal)



Sport_mark_rate_mrr_recent<-Sport_mark_rate_finescale %>% filter(YEAR>2012, AREA !="Area 22", !is.na(finescale_fishery)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, finescale_fishery), names_from=status, values_from = catch_estimate) %>%
  mutate(total_catch = marked_Kept_total+marked_Released_total + unmarked_Kept_total+unmarked_Released_total,
         mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
         ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total))

Sport_mark_rate_mrr_recent$mrr[is.na(Sport_mark_rate_mrr_recent$mrr)]<-0
Sport_mark_rate_mrr_recent$ukr[is.na(Sport_mark_rate_mrr_recent$ukr)]<-1

Sport_mark_rate_mrr_recent_sum<-Sport_mark_rate_mrr_recent %>% group_by(YEAR, finescale_fishery) %>% summarise(sum_total_catch = sum(total_catch, na.rm = TRUE))

Sport_mark_rate_mrr_recent<- Sport_mark_rate_mrr_recent %>% left_join(Sport_mark_rate_mrr_recent_sum) %>%
  mutate(component_mrr = (total_catch/sum_total_catch)*mrr,
         component_ukr = (total_catch/sum_total_catch)*ukr)

Sport_mark_rate_mrr_recent_finescale<-Sport_mark_rate_mrr_recent %>% group_by(YEAR, finescale_fishery) %>% summarise(mrr = sum(component_mrr, na.rm = TRUE), ukr=sum(component_ukr, na.rm=TRUE), sum_total_catch=sum(total_catch)) %>%
  mutate(ukr = case_when(
    sum_total_catch == 0 ~ 1,
    TRUE ~ ukr)) %>% dplyr::select(-sum_total_catch)

Sport_mark_data_finescale<-models_combined %>% filter(YEAR>2012) %>%
  pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = catch_estimate_predicted)

Sport_mark_rate_mrr_recent_finescale<- Sport_mark_rate_mrr_recent_finescale %>% left_join(Sport_mark_data_finescale)

Sport_mark_rate_mrr_recent_finescale_terminal<- Sport_mark_rate_mrr_recent_finescale %>%
  mutate(finescale_fishery = fct_recode(finescale_fishery, "TNORTH S" = "NBC ISBM S SUMMER",
                                        "TCENTRAL S" = "CBC S",
                                        "TNGS S" = "NGS S SUMMER",
                                        "TSGS S" = "SGS S SUMMER",
                                        "TWCVI S" = "SWCVI S SUMMER ISBM",
                                        "TJOHN ST S" = "JNST S SUMMER",
                                        "TCA JDF S" = "CA JDF S SUMMER")) %>%
  filter(finescale_fishery %in% c("TNORTH S", "TCENTRAL S", "TNGS S", "TSGS S", "TWCVI S", "TJOHN ST S", "TCA JDF S"))


Sport_mark_rate_mrr_recent_finescale<- bind_rows(Sport_mark_rate_mrr_recent_finescale,Sport_mark_rate_mrr_recent_finescale_terminal)


Sport_mark_rate_mrr_corrected<-bind_rows(Sport_mark_rate_mrr_past_corrected, Sport_mark_rate_mrr_recent_finescale)









#### Figure 1:
### make this by finescale fishery old... sum up.
models_combined_old<-models_combined %>% group_by(YEAR,kept_status,pred_cat, finescale_fishery_old) %>% summarise_if(is.numeric, sum, na.rm = TRUE)

fishery_name_south<- sort(unique(models_combined_old$finescale_fishery_old))


i = 1

m<-ggplot(models_combined_old  %>% filter(finescale_fishery_old==fishery_name_south[i], YEAR %in% c(2013:2023), kept_status=="Kept")) +
  geom_point(size=2.5,  aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
  geom_line(aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, linetype = " Unfiltered creel and logbook"))+
  scale_shape_manual(values=c(1,1), guide="none")+
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))+
  new_scale("shape")+
  scale_colour_viridis_d()+
  geom_point(size=2.5, aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
  geom_line(aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, linetype = "iREC included"))+
  scale_shape_manual(values=c(16,17), guide="none")+
  scale_linetype_manual(values=c(2,1))+
  ggtitle(paste(fishery_name_south[i])) + theme_bw() + scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))+ ylab("Catch estimate") + xlab("Year")+
  labs(linetype = "Data source")

m
#
# m<-ggplot(models_combined_old  %>% filter(YEAR %in% c(2013:2023), kept_status=="Kept")) +
#   geom_point(size=2.5,  aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
#   geom_line(aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, linetype = " Unfiltered creel and logbook"))+
#   scale_shape_manual(values=c(1,1), guide="none")+
#   scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))+
#   new_scale("shape")+
#   scale_colour_viridis_d()+
#   geom_point(size=2.5, aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
#   geom_line(aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, linetype = "iREC included"))+
#   scale_shape_manual(values=c(16,17), guide="none")+
#   scale_linetype_manual(values=c(2,1))+
#   ggtitle(paste(fishery_name_south[i])) + theme_bw() + scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023))+ ylab("Catch estimate") + xlab("Year")+
#   labs(linetype = "Data source")+
#   facet_wrap(~finescale_fishery_old)
#
# m


#### Figure 2
Summer_south<-Sport_mark_rate_finescale_combined %>%
  filter(YEAR %in% c(2013:2023))%>%
  filter(!str_detect(finescale_fishery, "CBC|NBC")) %>%
  filter(season %in% c("summer"))

Summer_south_no_nas<-Summer_south %>% drop_na(any_of(c("creel_plus_summer", "status", "finescale_fishery_old")))

Summer_model<- glm(formula = catch_estimate ~ finescale_fishery_old + status + creel_plus_summer:finescale_fishery_old + creel_plus_summer:status +
                     finescale_fishery_old:status + 1 + creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south_no_nas)

#### Adding confidence intervals based on model to a dataframe for plotting purposes
#based on this blog: https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/

family.set <- family(Summer_model)
ilink.family.set<- family.set$linkinv

mod<-Summer_model

ndata<-Summer_south_no_nas %>% group_by(status, finescale_fishery_old, finescale_fishery, season) %>%  tidyr::expand(creel_plus_summer = seq(0, max(creel_plus_summer), length=100))


## add the fitted values by predicting from the model for the new data
ndata<- add_column(ndata, fit = predict(mod, newdata = ndata, type = 'response'))
ndata<- bind_cols(ndata, setNames(as_tibble(predict(mod, ndata, se.fit = TRUE)[1:2]),
                                  c('fit_link','se_link')))

ndata <- mutate(ndata,
                fit_resp  = ilink.family.set(fit_link),
                right_upr = ilink.family.set(fit_link + (2 * se_link)),
                right_lwr = ilink.family.set(fit_link - (2 * se_link)))

fishery_name_south<- sort(unique(Summer_south_no_nas$finescale_fishery))

i =1



dataminmax_marked_kept<- Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Kept_total")
dataminmax_marked_released<- Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Released_total")
dataminmax_unmarked_kept<- Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Kept_total")
dataminmax_unmarked_released<- Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Released_total")


g1<-ggplot(Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Kept_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
  geom_point()+
  geom_line(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Kept_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
  geom_ribbon(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Kept_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
  theme_bw() +
  scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#440154"))+  scale_fill_manual(values=c("#440154"))+
  coord_cartesian(ylim = c(0,max(dataminmax_marked_kept$catch_estimate)), xlim = c(0,max(dataminmax_marked_kept$creel_plus_summer)))+
  labs(col="Status", fill="Status") +
  ggtitle('CA JDF S Summer')

g2<-ggplot(Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Released_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
  geom_point()+
  geom_line(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Released_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
  geom_ribbon(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="marked_Released_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
  theme_bw() +
  scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#31688e"))+  scale_fill_manual(values=c("#31688e"))+
  coord_cartesian(ylim = c(0,max(dataminmax_marked_released$catch_estimate)), xlim = c(0,max(dataminmax_marked_released$creel_plus_summer)))+
  labs(col="Status", fill="Status")

g3<-ggplot(Summer_south_no_nas %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Kept_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
  geom_point()+
  geom_line(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Kept_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
  geom_ribbon(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Kept_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
  theme_bw() +
  scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#35b779"))+  scale_fill_manual(values=c("#35b779"))+
  coord_cartesian(ylim = c(0,max(dataminmax_unmarked_kept$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_kept$creel_plus_summer)))+
  labs(col="Status", fill="Status")


g4<-ggplot(Summer_south_no_nas%>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Released_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
  geom_point()+
  geom_line(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Released_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
  geom_ribbon(ndata %>% filter(finescale_fishery==fishery_name_south[i], status=="unmarked_Released_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status, col=status), alpha = 0.10)+
  theme_bw() +
  scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#fde725"))+  scale_fill_manual(values=c("#fde725"))+
  coord_cartesian(ylim = c(0,max(dataminmax_unmarked_released$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_released$creel_plus_summer)))+
  labs(col="Status", fill="Status")


g<-g1+g2 + g3+g4 + plot_layout(guides = "collect")

g

############### Figure 3

cnr_canada_sport<-read.csv("cnr_canada_sport.csv") %>% as_tibble() %>% filter(year>2004)

fishery_map<-models_combined %>% dplyr::select(finescale_fishery_old, finescale_fishery) %>%
  mutate(erafishery = case_when(
    finescale_fishery_old %in% c("NWCVI S AABM", "SWCVI S AABM") ~ "WCVI AABM S",
    finescale_fishery_old %in% c("NGS S", "SGS S") ~  "GEO ST S",
    finescale_fishery_old == "CA JDF S" ~ "BC JF S",
    finescale_fishery_old %in%  c("NWCVI S ISBM", "SWCVI S ISBM") ~ "WCVI ISBM S",
    TRUE ~ finescale_fishery_old
  )) %>% unique()

cnr_canada_sport<-cnr_canada_sport %>% left_join(fishery_map) %>% rename(YEAR=year)

Sport_mark_rate_mrr<- Sport_mark_rate_mrr_corrected %>% left_join(cnr_canada_sport)
Sport_mark_rate_mrr<-Sport_mark_rate_mrr %>% mutate(Kept_total = marked_Kept_total + unmarked_Kept_total,
                                                    Released_total = marked_Released_total + unmarked_Released_total)


cnr_canada_sport_short<- cnr_canada_sport %>% dplyr::select(-finescale_fishery, -finescale_fishery_old) %>% unique()

Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr %>% dplyr::select(-Kept, -Released, -Release_rate) %>% group_by(erafishery, YEAR) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)
Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr_sum  %>% left_join(cnr_canada_sport_short)

rec_summary<-read.csv("rec_summary_2005-2023.csv") %>% as_tibble()
Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr_sum  %>% left_join(rec_summary)


## Kept only
ggplot() +
  geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Kept_total, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Kept_total, x=YEAR, col="lightblue4"))+
  geom_point(data=Sport_mark_rate_mrr_sum, aes(y=total_kept, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=total_kept, x=YEAR, col="lightgreen"))+
 # geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Kept, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Kept, x=YEAR, col="darkgreen"))+

  scale_color_manual(values=c("lightblue4", "lightgreen"),  labels = c("Revised estimates", "2024 C&E Report"))+
  theme(legend.position="bottom")+
  ylab("Kept Catch")+
  facet_wrap(~erafishery, scales = "free", ncol=4)+
  theme_bw()+ geom_vline(xintercept = 2012)+
  labs(col = "Data source")


################## Figure 4
## Released only
ggplot() +
  geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Released_total, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Released_total, x=YEAR, col="lightblue4"))+
  geom_point(data=Sport_mark_rate_mrr_sum, aes(y=legal_releases, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=legal_releases, x=YEAR, col="lightgreen"))+
 # geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Released, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Released, x=YEAR, col="darkgreen"))+

  scale_color_manual(values=c( "lightblue4", "lightgreen"),  labels = c("Revised estimates", "2024 C&E Report"))+
  theme(legend.position="bottom")+
  ylab("Released Catch")+
  facet_wrap(~erafishery, scales = "free", ncol=4)+
  theme_bw()+ geom_vline(xintercept = 2012)+
  labs(col = "Data source")



#### Figure 5 MRR UKR

#this one:
ggplot() +
  geom_point(data=Sport_mark_rate_mrr, aes(y=(1-ukr), x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=1-ukr, x=YEAR, col="lightblue"))+
  geom_point(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4"))+
  geom_point(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen"))+
  scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen"),  labels = c("Revised Unmarked Release Rate", "Revised Marked Release Rate", "2024 CNR Release Rate"))+
  theme(legend.position="bottom")+
  ylab("Proportion")+
  facet_wrap(~finescale_fishery)+
  theme_bw()+ geom_vline(xintercept = 2012)+
  labs(col="Data source")+
  theme(legend.position = "bottom")


# ggplot() +
#   geom_point(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue"))+
#   geom_point(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4"))+
#   geom_point(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen"))+
#   scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen"),  labels = c("URR", "MRR", "RR"))+
#   theme(legend.position="bottom")+
#   ylab("Proportion")+
#   facet_wrap(~finescale_fishery)+
#   theme_bw()+ geom_vline(xintercept = 2012)





