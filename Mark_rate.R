# Mark rate visuals
"%notin%" <- Negate("%in%")
#Sport data is creel and irec and lodge log and logbook

#Take out the North Coast and Central Coast data, and just work with the WCVI

# Filter data based on no infill data, QC creel, final estimates only
Sport_filtered_south<- Sport %>%
  as_tibble() %>%
  filter(SOURCE %in% c('Creel Estimate', 'iRec Estimate C', 'Historic Estimate',
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
  )) %>%
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>%
  summarise(VAL=sum(VAL)) %>% ungroup()

Sport_filtered_south_irec<- bind_rows(Sport_filtered_south, irec) %>% distinct() %>% as_tibble()
Sport_filtered_south_irec

#Take filtered data and get a by-year estimate to in fill for NAs below

#Mark rate by aggregated source
Sport_mark_rate_source<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_source = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  group_by(YEAR, AREA, MONTH, REGION2, MANAGEMENT) %>% summarise(marked_prop_source =mean(marked_prop_source, na.rm=TRUE)) %>%
  select(YEAR, AREA, MONTH, REGION2, MANAGEMENT, marked_prop_source)

#region and month and year
Sport_mark_rate_REGION2<- Sport_filtered_south_irec  %>%
  group_by(YEAR, REGION2, MONTH, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, REGION2, MONTH, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_REGION2 =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE))%>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(YEAR, REGION2, MONTH, MANAGEMENT, SOURCE, marked_prop_REGION2)


#aggregated source region and all years
Sport_mark_rate_REGION2_source<- Sport_filtered_south_irec  %>%
  group_by(REGION2,  MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c( REGION2,  SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_REGION2_source =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE))%>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  group_by(REGION2, MANAGEMENT) %>% summarise(marked_prop_REGION2_source =mean(marked_prop_REGION2_source, na.rm=TRUE)) %>%
  select(REGION2, MANAGEMENT, marked_prop_REGION2_source)


#Mark rate by Area/month, across all years
Sport_mark_rate_area_month<- Sport_filtered_south_irec  %>%
  group_by(AREA, MONTH, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(AREA, MONTH, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_area_month = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(AREA, MONTH, REGION2, MANAGEMENT, SOURCE, marked_prop_area_month)

#Mark rate by year for given area, whole year
Sport_mark_rate_year<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_year = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_year)

#Mark rate by Area, across all years and months
Sport_mark_rate_area<- Sport_filtered_south_irec  %>%
  group_by(AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  replace(is.na(.), 0) %>%
  mutate(marked_prop_area = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_area)



allobs2 <- expand(Sport_filtered_south_irec, nesting(AREA, REGION2, MANAGEMENT), YEAR, MONTH, MARKS_DESC, TYPE, SOURCE)


Sport_mark_rate_only<- Sport_filtered_south_irec  %>%
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  full_join(allobs2) %>%
  pivot_wider(id_cols = c(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  left_join(Sport_mark_rate_source) %>%
  left_join(Sport_mark_rate_year) %>%
  left_join(Sport_mark_rate_area_month) %>%
  left_join(Sport_mark_rate_area) %>%
  left_join(Sport_mark_rate_REGION2) %>%
  left_join(Sport_mark_rate_REGION2_source) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(marked_prop_use = case_when(
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & !is.na(marked_prop_source) & marked_prop_source %notin% c(0,1) ~ marked_prop_source,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & !is.na(marked_prop_REGION2) & marked_prop_REGION2 %notin% c(0,1) ~ marked_prop_REGION2,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & !is.na(marked_prop_area_month) & marked_prop_area_month %notin% c(0,1) ~ marked_prop_area_month,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & (is.na(marked_prop_area_month)| marked_prop_area_month %in% c(0,1)) & !is.na(marked_prop_year) & marked_prop_year %notin% c(0,1) ~ marked_prop_year,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & (is.na(marked_prop_area_month)| marked_prop_area_month %in% c(0,1)) & (is.na(marked_prop_year)| marked_prop_year %in% c(0,1)) & !is.na(marked_prop_area) & marked_prop_area %notin% c(0,1) ~ marked_prop_area,
    TRUE~marked_prop_REGION2_source))



Sport_mark_rate_only<- Sport_mark_rate_only %>% mutate(nfish = sum(marked_Kept, marked_Released, unmarked_Kept, unmarked_Released, na.rm=TRUE), nmark=sum(marked_Kept, marked_Released, na.rm = TRUE), nunmark=sum(unmarked_Kept, unmarked_Released, na.rm = TRUE)) %>%
                       mutate(Date = make_date(year=YEAR, month=MONTH))

ggplot(Sport_mark_rate_only %>% filter(AREA == 16, nfish>50), aes(y=marked_prop, x=Date, col=as.factor(SOURCE))) + geom_point() + geom_line()

ggplot(Sport_mark_rate_only %>% filter(AREA == 17, nfish>50), aes(y=marked_prop, x=Date, col=as.factor(SOURCE))) + geom_point() + geom_line()



ggplot(Sport_mark_rate_only %>% filter(AREA==18, nfish>50), aes(y=marked_prop, x=month(MONTH, label=TRUE), col=SOURCE, group=SOURCE)) +
  geom_point() + geom_line()+  facet_wrap(~YEAR) + ggtitle("Area 18") +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("Proportion marked")


ggplot(Sport_mark_rate_only %>% filter(REGION2=="GSS", nfish>20), aes(y=marked_prop_REGION2, x=month(MONTH, label=TRUE), col=SOURCE, group=SOURCE)) +
  geom_point() + geom_line()+  facet_wrap(~YEAR) + ggtitle("GSS") +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("Proportion marked")

ggplot(Sport_mark_rate_only %>% filter(REGION2=="GSN", nfish>20), aes(y=marked_prop_REGION2, x=month(MONTH, label=TRUE), col=SOURCE, group=SOURCE)) +
  geom_point() + geom_line()+  facet_wrap(~YEAR) + ggtitle("GSN") +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("Proportion marked")

ggplot(Sport_mark_rate_only %>% filter(REGION2=="JDF", nfish>100), aes(y=marked_prop_REGION2, x=month(MONTH, label=TRUE), col=SOURCE, group=SOURCE)) +
  geom_point() + geom_line()+  facet_wrap(~YEAR) + ggtitle("JDF") +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("Proportion marked")


ggplot(Sport_mark_rate_only %>% filter(AREA==23, nfish>20), aes(y=marked_prop_REGION2, x=month(MONTH, label=TRUE), col=SOURCE, group=SOURCE)) +
  geom_point() + geom_line()+  facet_wrap(~YEAR) + ggtitle("Area 23") +scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("Month") + ylab("Proportion marked")+ theme_bw()


### heat map
####
PFMAMonth <- plyr::ddply(Sport_mark_rate_only %>% filter(YEAR %in% 2021:2023, nfish>20), c( "MONTH", "AREA", "SOURCE"), summarise,
                         mr = mean(marked_prop, na.rm=TRUE))

ggplot(data=PFMAMonth%>% filter(mr!=0, SOURCE == "irec_calibrated"), aes(x=month(MONTH, label=TRUE), y=as.factor(AREA), fill = mr))+
  geom_tile(colour = "white") +
  geom_text(aes(label=round(mr*100, 0)))+
  scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging", direction=-1)+
   scale_y_discrete(limits=rev)+
  scale_x_discrete(position = "top")+
  labs(title = "Mark rate by PFMA",
       x = "Month", y = "PFMA") +
  theme_bw() + theme_minimal()



