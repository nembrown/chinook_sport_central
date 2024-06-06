

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



col1 = "#d8e1cf"
col2 = "#438484"
col3 = "lightblue"
col4 = "lightblue4"
col5 = "pink"
col6 = "darkred"
####
yearMonth <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "finescale_fishery_old"), summarise,
                         sum= sum(creel_plus, na.rm = TRUE), sum_historic= sum(historic_plus, na.rm = TRUE), sum_catch_estimate = sum(catch_estimate, na.rm = TRUE), sum_irec = sum(irec_calibrated, na.rm = TRUE))

yearMonth_creel_1<-yearMonth %>% filter(sum!=0, finescale_fishery_old!="NA")
yearMonth_historic_1<-yearMonth %>% filter(sum_historic!=0, finescale_fishery_old!="NA")


ggplot()+
  geom_tile(data=yearMonth_creel_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="creel only estimates")) +
  new_scale_fill() +
  geom_tile(data=yearMonth_historic_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
  scale_fill_gradient(low = col1, high = col2)+
  facet_wrap(~finescale_fishery_old)+
  labs(title = "Coverage by finescale fishery",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


yearMonth_creel_2<-yearMonth %>% filter(sum!=0, sum_catch_estimate==sum, finescale_fishery_old!="NA")
yearMonth_catch_estimate_1<-yearMonth %>% filter(sum_catch_estimate!=sum, sum_catch_estimate!=sum_irec, finescale_fishery_old!="NA")
yearMonth_irec_1<-yearMonth %>% filter(sum_irec!=0, sum_catch_estimate==sum_irec, finescale_fishery_old!="NA")




ggplot()+
  geom_tile(data=yearMonth_creel_2, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="creel only estimates")) +
  new_scale_fill() +
  geom_tile(data=yearMonth_catch_estimate_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col3, high = col4)+
  guides(fill=guide_legend(title="creel plus irec estimates")) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col5, high = col6)+
  facet_wrap(~finescale_fishery_old)+
  guides(fill=guide_legend(title="irec only estimates")) +
  labs(title = "Coverage by finescale fishery",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


#Marked Kept only
ggplot()+
  geom_tile(data=yearMonth_creel %>% filter(status == "marked_Kept_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
  scale_fill_gradient(low = col3, high = col4) +
  facet_wrap(~finescale_fishery)+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="iREC estimates")) +
  labs(title = "Coverage by finescale fishery - marked_Kept_total",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

# Marked Released
ggplot()+
  geom_tile(data=yearMonth_creel %>% filter(status == "marked_Released_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
  scale_fill_gradient(low = col3, high = col4) +
  facet_wrap(~finescale_fishery)+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="iREC estimates")) +
  labs(title = "Coverage by finescale fishery - marked_Released_total",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

#Unmarked Kept only
ggplot()+
  geom_tile(data=yearMonth_creel %>% filter(status == "unmarked_Kept_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
  scale_fill_gradient(low = col3, high = col4) +
  facet_wrap(~finescale_fishery)+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="iREC estimates")) +
  labs(title = "Coverage by finescale fishery - unmarked_Kept_total",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


#Unmarked Released
ggplot()+
  geom_tile(data=yearMonth_creel %>% filter(status == "unmarked_Released_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
  scale_fill_gradient(low = col3, high = col4) +
  facet_wrap(~finescale_fishery)+
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="iREC estimates")) +
  labs(title = "Coverage by finescale fishery - unmarked_Released_total",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

################## TERMINAL

yearMonth_term <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "finescale_fishery_term"), summarise,
                              sum = sum(creel_plus), sum_catch_estimate = sum(catch_estimate), sum_irec = sum(irec_calibrated))

yearMonth_creel_term<-yearMonth_term %>% filter(sum!=0, sum_catch_estimate==sum, finescale_fishery_term!="NA")
yearMonth_catch_estimate_term<-yearMonth_term %>% filter(sum_catch_estimate!=sum, sum_catch_estimate!=sum_irec, finescale_fishery_term!="NA")
yearMonth_irec_term<-yearMonth_term %>% filter(sum_irec!=0, sum_catch_estimate==sum_irec, finescale_fishery_term!="NA")


#overall summary
ggplot()+
  geom_tile(data=yearMonth_creel_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  scale_y_discrete(limits=rev)+
  guides(fill=guide_legend(title="creel only estimates")) +
  new_scale_fill() +
  geom_tile(data=yearMonth_catch_estimate_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col3, high = col4)+
  guides(fill=guide_legend(title="creel plus irec estimates")) +
  new_scale_fill() +
  geom_tile(data=yearMonth_irec_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
  scale_fill_gradient(low = col5, high = col6)+
  facet_wrap(~finescale_fishery_term)+
  guides(fill=guide_legend(title="irec only estimates")) +
  labs(title = "Coverage by terminal finescale fishery",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

