
library(tidyverse)
#run Crest connection
View(Sport)
"%notin%" <- Negate("%in%")
#Sport data is creel and irec and lodge log and logbook

#Take out the North Coast and Central Coast data, and just work with the WCVI

# Filter data based on no infill data, QC creel, final estimates only
Sport_filtered_south<- Sport %>% filter(SOURCE %in% c('Creel Estimate', 'iRec Estimate C', 'Historic Estimate',
                                       'Log Estimate', 'Lodge Manifest', 'Lodge Log', 'Lodge Estimate') ) %>% # no in fill data or errors
                  #filter(STATUS %in% c('Published Estimate - Full Month')) %>% #no preliminary estimates, only published full
                  filter(DATESINC %notin% c("0114", "0106", "1731","1831","0107" ))%>% #take out estimates based on <15 days of fly overs
                  filter(AREA %notin% c(1:10, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 142, 130, NA)) %>%
                  filter(SUB_TYPE == "LEGAL") %>%
                  mutate(REGION2 = case_when(REGION == "QC" ~ "QC",
                                             TRUE ~ REGION2)) %>%
                  mutate(MARKS_DESC = case_when(
                    MARKS_DESC == "Not Adipose Checked" ~ "unchecked",
                    MARKS_DESC == "Not Applicable" ~ "unchecked",
                    MARKS_DESC == "Not Adipose Marked" ~ "unmarked",
                    MARKS_DESC == "Adipose Marked" ~ "marked",
                  ))%>%
                  mutate(SOURCE = case_when(
                    SOURCE == "Creel Estimate" ~ "creel",
                    SOURCE == "Lodge Log" ~ "lodge_log",
                    SOURCE == "Lodge Manifest" ~ "lodge_manifest",
                    SOURCE == "Lodge Estimate" ~ "lodge_estimate",
                    SOURCE == "Log Estimate" ~ "log_estimate",
                    SOURCE == "iRec Estimate C" ~ "irec_calibrated",
                    TRUE ~ SOURCE
                  ))


#Take filtered data and get a by-year estimate to in fill for NAs below
Sport_mark_rate_year<- Sport_filtered_south  %>%
  group_by(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_year = (marked_Kept + marked_Released)/(marked_Kept + marked_Released + unmarked_Kept+unmarked_Released)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_year)

Sport_mark_rate_area<- Sport_filtered_south  %>%
  group_by(AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_area = (marked_Kept + marked_Released)/(marked_Kept + marked_Released + unmarked_Kept+unmarked_Released)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_area)

Sport_mark_rate_REGION2<- Sport_filtered_south  %>%
  group_by(REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_REGION2 = (marked_Kept + marked_Released)/(marked_Kept + marked_Released + unmarked_Kept+unmarked_Released)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(REGION2, MANAGEMENT, SOURCE, marked_prop_REGION2)

allobs2 <- expand(Sport_filtered_south, nesting(AREA, REGION2, MANAGEMENT), YEAR, MONTH, MARKS_DESC, TYPE, SOURCE)


Sport_mark_rate<- Sport_filtered_south  %>%
                  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
                  full_join(allobs2) %>%
                  pivot_wider(id_cols = c(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
                  mutate(marked_prop = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
                  left_join(Sport_mark_rate_year) %>%
                  left_join(Sport_mark_rate_area) %>%
                  left_join(Sport_mark_rate_REGION2) %>%
                  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
                  mutate(marked_prop_use = case_when(
                    is.na(marked_prop) & !is.na(marked_prop_year)~ marked_prop_year,
                    is.na(marked_prop) & is.na(marked_prop_year)& !is.na(marked_prop_area) ~ marked_prop_area,
                    is.na(marked_prop) & is.na(marked_prop_year) & is.na(marked_prop_area)& !is.na(marked_prop_REGION2) ~ marked_prop_REGION2,
                    TRUE~marked_prop )) %>%
                  mutate(marked_Kept_add = marked_prop_use*unchecked_Kept,
                         marked_Released_add = marked_prop_use*unchecked_Released,
                         unmarked_Kept_add = (1-marked_prop_use)*unchecked_Kept,
                         unmarked_Released_add = (1-marked_prop_use)*unchecked_Released) %>%
                  mutate(marked_Kept_total = sum(marked_Kept_add, marked_Kept, na.rm = TRUE),
                         marked_Released_total = sum(marked_Released_add, marked_Released, na.rm=TRUE),
                         unmarked_Kept_total = sum(unmarked_Kept_add, unmarked_Kept, na.rm=TRUE),
                         unmarked_Released_total = sum(unmarked_Released_add, unmarked_Released, na.rm=TRUE)) %>%
                 select(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, marked_Kept_total, unmarked_Kept_total, marked_Released_total, unmarked_Released_total) %>%
                 pivot_longer(cols=c(contains("total")), names_to = "status", values_to = "value") %>%
                 pivot_wider(id_cols = c(YEAR, MONTH, AREA, REGION2, MANAGEMENT, status), names_from = SOURCE, values_from = value) %>%
                 mutate_all(~ifelse(is.nan(.), NA, .)) %>%
                 rowwise() %>%
                 mutate(creel_plus = sum(creel,lodge_log,lodge_manifest,lodge_estimate,log_estimate, na.rm=TRUE)) %>%
                 mutate(catch_estimate = case_when(
                   YEAR > 2011 & MONTH %in% c(5:9) & is.na(creel) ~ as.numeric(irec_calibrated),
                   YEAR > 2011 & MONTH %in% c(1:4,10:12) ~ as.numeric(irec_calibrated),
                   TRUE ~ as.numeric(creel_plus)))




#presumably this is now the correct data at the PFMA level ...
# Now we need to sum up to the appropriate finescale fishery level, and do a calculation of how much more irec gets us.
#Need to split the finescale fisheries now

Sport_mark_rate_finescale<-Sport_mark_rate%>%
                           mutate(finescale_fishery = case_when(
                           AREA%in%c(121,122,123,124,125,126,127) & MANAGEMENT=="AABM" & MONTH%in%c(8:12) ~ "WCVI AABM S FALL",
                           AREA%in%c(121,122,123,124,125,126,127) & MANAGEMENT=="AABM" & MONTH%in%c(1:7) ~ "WCVI AABM S SPRING",
                           AREA%in%c(21,23,24) & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
                           AREA%in%c(21,23,24) & MONTH%in%c(1:7) ~ "WCVI AABM S SPRING",
                           AREA%in%c(25,26,27) & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
                           AREA%in%c(25,26,27) & MONTH%in%c(1:6) ~ "WCVI AABM S SPRING",

                           AREA%in%c(121,122,123,124,125,126,127) & MANAGEMENT=="ISBM" & MONTH%in%c(7:12) ~ "WCVI ISBM S FALL",
                           AREA%in%c(21,22,23,24) & MONTH%in%c(8,9) ~ "WCVI ISBM S FALL",
                           AREA%in%c(25,26,27) & MONTH%in%c(7,8,9) ~ "WCVI ISBM S FALL",

                           (AREA %in%c(13,14,15,16) |REGION2== "GSN")& MONTH%in%c(8:12) ~ "NGS S FALL",
                           (AREA %in%c(13,14,15,16) |REGION2== "GSN")& MONTH%in%c(1:7) ~ "NGS S SPRING",

                           (AREA %in%c(17,18,28,29) |REGION2== "GSS")& MONTH%in%c(8:12) ~ "SGS S FALL",
                           (AREA %in%c(17,18,28,29) |REGION2== "GSS")& MONTH%in%c(1:7) ~ "SGS S SPRING",

                           (AREA %in% c(11,111,12) | REGION2 == "JST") & MONTH%in%c(8:12) ~ "JNST S FALL",
                           (AREA %in% c(11,111,12) | REGION2 == "JST") & MONTH%in%c(1:7) ~ "JNST S SPRING",

                           (AREA %in% c(20) | REGION2 == "JDF") & MONTH%in%c(1:7) ~ "CA JDF S SPRING",
                           (AREA %in% c(20) | REGION2 == "JDF") & MONTH%in%c(8:12) ~ "CA JDF S FALL")) %>%

                           mutate(finescale_fishery_term = case_when(
                             AREA%in%c(23) & MONTH%in%c(8:12) ~ "TWCVI S FALL",
                             AREA %in% c(11,111,12) & MONTH%in%c(8:12) ~ "TJOHN ST S FALL",
                             AREA %in% c(11,111,12) & MONTH%in%c(5:7) ~ "TJOHN ST S SUMMER",
                             (AREA%in% c(17,18,28,29) |  REGION2 == "GSS") & MONTH%in%c(8:12) ~ "TSGS S FALL",
                             (AREA%in% c(17,18,28,29) |  REGION2 == "GSS") & MONTH%in%c(5:7) ~ "TSGS S SUMMER",
                             (AREA%in% c(13,14,15,16) |  REGION2 == "GSN") & MONTH%in%c(8:12) ~ "TNGS S FALL",
                             (AREA%in% c(13,14,15,16) |  REGION2 == "GSN") & MONTH%in%c(5:7) ~ "TNGS S SUMMER",
                             (AREA %in%c(20) | REGION2 == "JDF") & MONTH%in%c(5:8) ~ "TCA JDF S SUMMER"))

Sport_mark_rate_finescale_sum<- Sport_mark_rate_finescale %>%
                                filter(!is.na(finescale_fishery)) %>%
                                group_by(YEAR, status, finescale_fishery) %>%
                                summarise_at(vars(creel:catch_estimate), sum, na.rm=TRUE)

Sport_mark_rate_finescale_term_sum<- Sport_mark_rate_finescale %>%
                                      filter(!is.na(finescale_fishery_term)) %>%
                                      group_by(YEAR, status, finescale_fishery_term) %>%
                                      summarise_at(vars(creel:catch_estimate), sum, na.rm=TRUE) %>%
                                      rename(finescale_fishery = finescale_fishery_term)


Sport_mark_rate_finescale_combined<-full_join(Sport_mark_rate_finescale_term_sum, Sport_mark_rate_finescale_sum)


Sport_mark_rate_mrr<-Sport_mark_rate_finescale_combined %>%
                     pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = catch_estimate) %>%
                      mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
                             ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total)) %>%
                      mutate(mrrplusukr=mrr + ukr)


ggplot(Sport_mark_rate_mrr) + geom_point(aes(y=ukr, x=YEAR), col="lightblue") + geom_line(aes(y=ukr, x=YEAR), col="lightblue")+
  geom_point(aes(y=mrr, x=YEAR), col="lightblue4") + geom_line(aes(y=mrr, x=YEAR), col="lightblue4")+
 # geom_point(aes(y=mrrplusukr, x=YEAR), col="black") + geom_line(aes(y=mrrplusukr, x=YEAR), col="black")+
 # geom_hline(yintercept = 1, col="red")+
  facet_wrap(~finescale_fishery)+
  theme_bw()+  theme(legend.title=element_text(size=20),
                     legend.text=element_text(size=14))


ggplot(Sport_mark_rate_finescale_combined, aes(y=catch_estimate, x=YEAR, col=as.factor(status))) + geom_point() + geom_line()+ facet_wrap(~finescale_fishery)


ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR==2023, status=="marked_Kept_total"), aes(x=creel_plus, y=catch_estimate, col=as.factor(finescale_fishery))) + geom_point()
ggplot(Sport_mark_rate %>% filter(YEAR==2022), aes(x=creel_plus, y=catch_estimate)) + geom_point()


#modelling: 2022:
Sport_mark_kept_2022<-Sport_mark_rate %>% filter(YEAR == 2013:2023, status == "marked_Kept_total")

model_marked<- glm(formula = catch_estimate ~ creel_plus,
                   family = gaussian,
                   data = Sport_mark_kept_2022)

summary(model_marked)

ggplot(Sport_mark_kept_2022, aes(x=catch_estimate, y= creel_plus))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")



col1 = "#d8e1cf"
col2 = "#438484"
col3 = "lightblue"
col4 = "lightblue4"
####
yearMonth <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "status", "finescale_fishery"), summarise,
                         sum    = sum(creel_plus), sum_irec = sum(irec_calibrated))

yearMonth_creel<-yearMonth %>% filter(sum!=0)
yearMonth_irec<-yearMonth %>% filter(sum_irec!=0)


#overall summary
ggplot()+
  geom_tile(data=yearMonth_creel, aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  scale_y_discrete(limits=rev)+
  new_scale_fill() +
  geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
  scale_fill_gradient(low = col3, high = col4)+
  facet_wrap(~finescale_fishery)+
  guides(fill=guide_legend(title="creel estimates")) +
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


