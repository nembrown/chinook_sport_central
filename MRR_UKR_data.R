

#### MRR URR
source("wrangle_data.R")
source("model_data.R")



Sport_mark_rate_mrr<-Season_south_combined %>%
  pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = catch_estimate_predicted) %>%
  mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
         ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total))
Sport_mark_rate_mrr$mrr[is.na(Sport_mark_rate_mrr$mrr)]<-0
Sport_mark_rate_mrr$ukr[is.na(Sport_mark_rate_mrr$ukr)]<-1

Sport_mark_rate_mrr<- Sport_mark_rate_mrr %>% mutate(unmarked_release=1-ukr)

Sport_mark_rate_mrr_creel<-Sport_mark_rate_finescale_combined %>%
  pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = creel_plus) %>%
  mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
         ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total)) %>%
  mutate(mrrplusukr=mrr + ukr)

ggplot() +
  geom_point(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue"))+
  geom_point(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4"))+
  # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen"))+
  # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen"))+
  scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen", "darkgreen"),  labels = c("URR", "MRR", "URR", "MRR"))+
  theme(legend.position="bottom")+
  ylab("Proportion")+
  facet_wrap(~finescale_fishery)+
  theme_bw()+ geom_vline(xintercept = 2012)

### by month plot:
ggplot(Sport_mark_rate_mrr %>% filter(finescale_fishery_old=="CA JDF S")) +
  geom_point( aes(y=mrr, x=month(MONTH, label=TRUE),  col="lightblue4", group=finescale_fishery_old)) +
  geom_line( aes(y=mrr, x=month(MONTH, label=TRUE),  col="lightblue4", group=finescale_fishery_old))+
  geom_point(aes(y=unmarked_release, x=month(MONTH, label=TRUE),  col="lightblue", group=finescale_fishery_old)) +
  geom_line( aes(y=unmarked_release, x=month(MONTH, label=TRUE),  col="lightblue", group=finescale_fishery_old))+
  scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen", "darkgreen"),  labels = c("Unmarked Release Rate", "Marked Release Rate", "UKR", "MRR"))+
  facet_wrap(~YEAR) +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("MRR")+ theme_bw()+ geom_vline(xintercept = 2012)


ggplot(Sport_mark_rate_mrr_creel) + geom_point(aes(y=ukr, x=YEAR, col="lightblue")) + geom_line(aes(y=ukr, x=YEAR, col="lightblue"))+
  geom_point(aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(aes(y=mrr, x=YEAR, col="lightblue4"))+
  scale_color_manual(values=c("lightblue", "lightblue4"),  labels = c("UKR", "MRR"))+
  theme(legend.position="bottom")+
  ylab("Proportion")+
  facet_wrap(~finescale_fishery)+
  theme_bw()+ geom_vline(xintercept = 2012)


ggplot(Sport_mark_rate_finescale_combined, aes(y=catch_estimate, x=YEAR, col=as.factor(status))) + geom_point() + geom_line()+ facet_wrap(~finescale_fishery)


ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR==2023, status=="marked_Kept_total"), aes(x=creel_plus, y=catch_estimate, col=as.factor(finescale_fishery))) + geom_point()
ggplot(Sport_mark_rate %>% filter(YEAR==2022), aes(x=creel_plus, y=catch_estimate)) + geom_point()


#modelling: 2022:

model_marked<- glm(formula = catch_estimate ~ creel_plus,
                   family = gaussian,
                   data = Sport_mark_kept_2022)

summary(model_marked)


##
##maybe exclude 2016?
Sport_mark_kept<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "marked_Kept_total")
ggplot(Sport_mark_kept, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("marked Kept")


Sport_mark_released<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "marked_Released_total")
ggplot(Sport_mark_released, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("marked Released")

Sport_unmarked_released<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "unmarked_Released_total")
ggplot(Sport_unmarked_released, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("unmarked Released")


Sport_unmarked_kept<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "unmarked_Kept_total")
ggplot(Sport_unmarked_kept, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("unmarked Kept")


ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery=="WCVI AABM S SUMMER"), aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~status, scales="free") + ggtitle("WCVI AABM S SUMMER") + theme_bw()

ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery_old=="WCVI AABM S"), aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("WCVI AABM S") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()



ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023)), aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free")+ theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()


