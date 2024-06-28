#load packages
library(tidyverse)
library(odbc)
library(lubridate)
library(ggnewscale)

#utils
"%notin%" <- Negate("%in%")

#Pull sport data from CREST
source("PullAllSport.R")
# PullAllSport(Start_year) set the calendar year to start querying from to the current year
rslt <- PullAllSport(2005)
quality_report <- rslt[[1]]
estimates <- rslt[[2]]
rm(rslt)

#Load North Coast data from csv.
nbc_aabm<- read.csv("Sport_data_set_NBC_AABM.csv") |>
           as_tibble() |>
           dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE, VAL_calc)|>
           rename(VAL = VAL_calc)|>
           mutate(INCLUDE_15 = 1, VARIANCE = 0)|>
           mutate(season = case_when(
             MONTH %in% c(1:4) ~ "spring",
             MONTH %in% c(5:9) ~ "summer",
             MONTH %in% c(10:12) ~ "fall"
           ))

nbc_isbm<- read.csv("Sport_data_set_NBC_ISBM.csv") |>
  as_tibble() |>
  dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE, VAL_calc)|>
  rename(VAL = VAL_calc)|>
  mutate(INCLUDE_15 = 1, VARIANCE = 0)|>
  mutate(season = case_when(
    MONTH %in% c(1:4) ~ "spring",
    MONTH %in% c(5:9) ~ "summer",
    MONTH %in% c(10:12) ~ "fall"
  ))

cbc_isbm<- read.csv("Sport_data_set_CBC_ISBM.csv") |>
  as_tibble() |>
  dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE, VAL_calc)|>
  rename(VAL = VAL_calc)|>
  mutate(INCLUDE_15 = 1, VARIANCE = 0)|>
  mutate(season = case_when(
    MONTH %in% c(1:4) ~ "spring",
    MONTH %in% c(5:9) ~ "summer",
    MONTH %in% c(10:12) ~ "fall"
  ))

historic_effort<- nbc_aabm |>
  as_tibble() |>
  dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT) |>
  unique()
historic_effort$historic_done<-"yes"

#isolating months and areas where there is definitely creel effort, to be used later:
creel_effort<-  estimates |>
                as_tibble() |>
                 mutate(INCLUDE_15 = case_when(
                   SOURCE != "Creel" ~ 1,
                   YEAR == 2006 & MONTH==8& REGION2=="WCVIS"& MANAGEMENT=="ISBM" ~ 1,
                   YEAR == 2005 & MONTH==7& REGION2=="GSS"& MANAGEMENT=="ISBM" ~ 1,
                   TRUE ~ INCLUDE_15
                 ))|>
                filter(INCLUDE_15 ==1, YEAR<2024, SOURCE=="Creel") |>
                dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT) |>
                unique()
creel_effort$creel_done<-"yes"

#logbook effort
logbook_effort<-  estimates |>
  as_tibble() |>
  mutate(INCLUDE_15 = case_when(
    SOURCE != "Creel" ~ 1,
    YEAR == 2006 & MONTH==8& REGION2=="WCVIS"& MANAGEMENT=="ISBM" ~ 1,
    YEAR == 2005 & MONTH==7& REGION2=="GSS"& MANAGEMENT=="ISBM" ~ 1,
    TRUE ~ INCLUDE_15
  ))|>
  filter(INCLUDE_15 ==1, YEAR<2024, SOURCE %notin% c("Creel", "iREC")) |>
  dplyr::select(YEAR, MONTH, AREA, REGION2, MANAGEMENT) |>
  unique()
logbook_effort$logbook_done<-"yes"

creel_plus_effort<- full_join(creel_effort, logbook_effort) %>%
  mutate(creel_plus_done = case_when(
    logbook_done == "yes" | creel_done == "yes" ~ "yes",
    TRUE ~ "no"
  ))

Sport_filtered_south_irec_unfiltered<-
  estimates |>
  as_tibble() |>
  filter( YEAR<2024)|>
  filter(AREA %notin% c("Area 29 (In River)", "Campbell River", "Quinsam River", "CR-1", "CR-2", "CR-3", "CR-4", "QR-1", "QR-2", "QR-3", "QR-4")) |>
  mutate(AREA = case_when(
    AREA== "Area 29 (Marine)" ~ "Area 29",
    AREA== "Area 19" & REGION2=="JDF" ~ "Area 19 (JDF)",
    AREA== "Area 19" & REGION2=="GSS" ~ "Area 19 (GS)",
    TRUE ~ as.character(AREA))) |>
  filter(SUB_TYPE == "LEGAL") |>
  mutate(REGION2 = case_when(AREA == "Area 2" ~ "NC", TRUE ~ REGION2)) |>
  mutate(MANAGEMENT = case_when(AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W") ~ "AABM", TRUE ~ MANAGEMENT )) |>
  mutate(MARKS_DESC = case_when(
    MARKS_DESC == "Not Adipose Checked" ~ "unchecked",
    MARKS_DESC == "Not Checked" ~ "unchecked",
    MARKS_DESC == "Not Applicable" ~ "unchecked",
    MARKS_DESC == "Not Adipose Marked" ~ "unmarked",
    MARKS_DESC == "Adipose Marked" ~ "marked")) |>
  mutate(SOURCE = case_when(
    SOURCE == "Creel" ~ "creel_unfiltered",
    SOURCE == "Historic" ~ "historic",
    SOURCE %in% c("Lodge Log","Lodge Manifest","Lodge Manifest - Log", "Lodge Estimate", "Log Estimate", "Lodge eLog") ~ "lodge_log",
    SOURCE == "iREC" ~ "irec_calibrated",
    TRUE ~ SOURCE )) |>
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE, INCLUDE_15) |>
  summarise(VARIANCE=sum(VARIANCE), VAL=sum(ESTIMATE)) |> ungroup()|>
  mutate(season = case_when(
    MONTH %in% c(1:4) ~ "spring",
    MONTH %in% c(5:9) ~ "summer",
    MONTH %in% c(10:12) ~ "fall"
  )) |>
  filter(SOURCE == "creel_unfiltered")

Sport_filtered_south_irec<-
  estimates |>
  as_tibble() |>
  mutate(INCLUDE_15 = case_when(
    SOURCE != "Creel" ~ 1,
    YEAR == 2006 & MONTH==8& REGION2=="WCVIS"& MANAGEMENT=="ISBM" ~ 1,
    YEAR == 2005 & MONTH==7& REGION2=="GSS"& MANAGEMENT=="ISBM" ~ 1,
    TRUE ~ INCLUDE_15
  ))|>
  filter(INCLUDE_15 ==1, YEAR<2024)|>
  filter(AREA %notin% c("Area 29 (In River)", "Campbell River", "Quinsam River", "CR-1", "CR-2", "CR-3", "CR-4", "QR-1", "QR-2", "QR-3", "QR-4")) |>
   mutate(AREA = case_when(
    AREA== "Area 29 (Marine)" ~ "Area 29",
    AREA== "Area 19" & REGION2=="JDF" ~ "Area 19 (JDF)",
    AREA== "Area 19" & REGION2=="GSS" ~ "Area 19 (GS)",
    TRUE ~ as.character(AREA))) |>
  filter(SUB_TYPE == "LEGAL") |>
  mutate(REGION2 = case_when(AREA == "Area 2" ~ "NC", TRUE ~ REGION2)) |>
  mutate(MANAGEMENT = case_when(AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W") ~ "AABM", TRUE ~ MANAGEMENT )) |>
  mutate(MARKS_DESC = case_when(
    MARKS_DESC == "Not Adipose Checked" ~ "unchecked",
    MARKS_DESC == "Not Checked" ~ "unchecked",
    MARKS_DESC == "Not Applicable" ~ "unchecked",
    MARKS_DESC == "Not Adipose Marked" ~ "unmarked",
    MARKS_DESC == "Adipose Marked" ~ "marked")) |>
  mutate(SOURCE = case_when(
    SOURCE == "Creel" ~ "creel",
    SOURCE == "Historic" ~ "historic",
    SOURCE %in% c("Lodge Log","Lodge Manifest","Lodge Manifest - Log", "Lodge Estimate", "Log Estimate", "Lodge eLog") ~ "lodge_log",
    SOURCE == "iREC" ~ "irec_calibrated",
    TRUE ~ SOURCE )) |>
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE, INCLUDE_15) |>
  summarise(VARIANCE=sum(VARIANCE), VAL=sum(ESTIMATE)) |> ungroup()|>
  mutate(season = case_when(
    MONTH %in% c(1:4) ~ "spring",
    MONTH %in% c(5:9) ~ "summer",
    MONTH %in% c(10:12) ~ "fall"
  ))

#Take out NC and add back in from csv but KEEP IREC in
#Add back in NBC data from csv
Sport_filtered_south_irec<-Sport_filtered_south_irec |>
                           rbind(Sport_filtered_south_irec_unfiltered)|>
                           mutate(filter_NC = case_when(
                             REGION2 %in% c("NC") & SOURCE != "irec_calibrated" ~ "remove",
                             TRUE ~ "keep") ) |>
                           filter(filter_NC=="keep")|>
                           dplyr::select(-filter_NC)|>
                           rbind(nbc_aabm)|>
                           rbind(nbc_isbm)|>
                           rbind(cbc_isbm)

#Take filtered data and get a by-year estimate to in fill for NAs below

#2. Mark rate averaged across source
Sport_mark_rate_source<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) |>
  mutate(marked_prop_source = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(marked_prop_source, case_when(
    marked_prop_source ==1 | marked_prop_source ==0 ~ NA,
  TRUE ~ marked_prop_source)) %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT) %>% summarise(marked_prop_source =mean(marked_prop_source, na.rm=TRUE)) %>%
  dplyr::select(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, marked_prop_source) %>% ungroup()

#3. Mark rate monthly Regional average
Sport_mark_rate_REGION2<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_REGION2 =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate(marked_prop_REGION2, case_when(
    marked_prop_REGION2 ==1 | marked_prop_REGION2 ==0 ~ NA,
    TRUE ~ marked_prop_REGION2)) %>%
  group_by(YEAR, REGION2, MONTH, season, SOURCE, MANAGEMENT) %>% summarise(marked_prop_REGION2 =mean(marked_prop_REGION2, na.rm=TRUE)) %>%
  dplyr::select(YEAR, MONTH, season, REGION2, SOURCE, MANAGEMENT, marked_prop_REGION2) %>% ungroup()

#4. Mark rate seasonal area average
Sport_mark_rate_seasonal<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_seasonal =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate(marked_prop_seasonal, case_when(
    marked_prop_seasonal ==1 | marked_prop_seasonal  ==0 ~ NA,
    TRUE ~ marked_prop_seasonal)) %>%
  group_by(YEAR, season, AREA, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_seasonal =mean(marked_prop_seasonal, na.rm=TRUE)) %>%
  dplyr::select(YEAR, season, AREA, REGION2, SOURCE, MANAGEMENT, marked_prop_seasonal) %>% ungroup()

#5. Mark rate seasonal regional average
Sport_mark_rate_seasonal_reg<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_seasonal_reg =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate(marked_prop_seasonal_reg, case_when(
    marked_prop_seasonal_reg ==1 | marked_prop_seasonal_reg ==0 ~ NA,
    TRUE ~ marked_prop_seasonal_reg)) %>%
  group_by(YEAR,season, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_seasonal_reg =mean(marked_prop_seasonal_reg, na.rm=TRUE)) %>%
  dplyr::select(YEAR, season, REGION2, SOURCE, MANAGEMENT, marked_prop_seasonal_reg) %>% ungroup()

#6. Mark rate area month across years
Sport_mark_rate_area_month<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_area_month =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate(marked_prop_area_month , case_when(
    marked_prop_area_month  ==1 | marked_prop_area_month  ==0 ~ NA,
    TRUE ~ marked_prop_area_month )) %>%
  group_by(AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_area_month =mean(marked_prop_area_month, na.rm=TRUE)) %>%
  dplyr::select(AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT, marked_prop_area_month) %>% ungroup()

#7. Mark rate regional month across years
Sport_mark_rate_region_month<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_region_month =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate(marked_prop_region_month , case_when(
    marked_prop_region_month ==1 | marked_prop_region_month ==0 ~ NA,
    TRUE ~ marked_prop_region_month)) %>%
   group_by( MONTH, season, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_region_month =mean(marked_prop_region_month, na.rm=TRUE)) %>%
  dplyr::select(MONTH, season, REGION2, MANAGEMENT, SOURCE, marked_prop_region_month) %>% ungroup()


#expand to include all combinations

allobs2 <- tidyr::expand(Sport_filtered_south_irec, nesting(AREA, REGION2, MANAGEMENT), YEAR, nesting(MONTH, season), MARKS_DESC, TYPE, SOURCE) %>%
           mutate(bad_combos = case_when(
             AREA %in% c("Area 25", "Area 26", "Area 27") & MANAGEMENT == "ISBM" & MONTH %in% c(1:6,10:12) ~ "bad",
             AREA %in% c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal") & MANAGEMENT == "ISBM" & MONTH %in% c(1:7,10:12) ~ "bad",
             TRUE ~ "good")) %>%
            filter(bad_combos == "good") %>%
            dplyr::select(-bad_combos)


#Join with mark rate data and expand unchecked
Sport_mark_rate<- Sport_filtered_south_irec  %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL), sum_VARIANCE=sum(VARIANCE)) %>%
  ungroup() %>%
  full_join(allobs2) %>%
  pivot_wider(id_cols = c(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  group_by(YEAR, MONTH, AREA, season, REGION2, MANAGEMENT, SOURCE) %>%
  mutate(marked_prop = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  left_join(Sport_mark_rate_source) %>%
  left_join(Sport_mark_rate_seasonal_reg) %>%
  left_join(Sport_mark_rate_area_month) %>%
  left_join(Sport_mark_rate_REGION2) %>%
  left_join(Sport_mark_rate_seasonal)%>%
  left_join(Sport_mark_rate_region_month) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(marked_prop_use1 = case_when(
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & !is.na(marked_prop_source) & marked_prop_source %notin% c(0,1) ~ marked_prop_source,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & !is.na(marked_prop_REGION2) & marked_prop_REGION2 %notin% c(0,1) ~ marked_prop_REGION2,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & !is.na(marked_prop_seasonal) & marked_prop_seasonal %notin% c(0,1) ~ marked_prop_seasonal,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & (is.na(marked_prop_seasonal)| marked_prop_seasonal %in% c(0,1)) & !is.na(marked_prop_seasonal_reg) & marked_prop_seasonal_reg %notin% c(0,1) ~ marked_prop_seasonal_reg,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & (is.na(marked_prop_seasonal)| marked_prop_seasonal %in% c(0,1)) & (is.na(marked_prop_seasonal_reg) | marked_prop_seasonal_reg %in% c(0,1)) & !is.na(marked_prop_area_month) & marked_prop_area_month %notin% c(0,1) ~ marked_prop_area_month,
    (is.na(marked_prop) | marked_prop %in% c(0,1)) & (is.na(marked_prop_source)| marked_prop_source %in% c(0,1))  & (is.na(marked_prop_REGION2)| marked_prop_REGION2 %in% c(0,1)) & (is.na(marked_prop_seasonal)| marked_prop_seasonal %in% c(0,1)) & (is.na(marked_prop_seasonal_reg) | marked_prop_seasonal_reg %in% c(0,1)) & (is.na(marked_prop_area_month) |marked_prop_area_month %in% c(0,1))& !is.na(marked_prop_region_month) & marked_prop_region_month %notin% c(0,1) ~ marked_prop_region_month,
    TRUE ~ marked_prop)) %>%
  mutate(marked_prop_use = case_when(
    is.na(marked_prop_use1) ~ 0.5,
    TRUE ~ marked_prop_use1)) %>%
  mutate(marked_Kept_add = marked_prop_use*unchecked_Kept,
         marked_Released_add = marked_prop_use*unchecked_Released,
         unmarked_Kept_add = (1-marked_prop_use)*unchecked_Kept,
         unmarked_Released_add = (1-marked_prop_use)*unchecked_Released) %>%
  mutate(marked_Kept_total = sum(marked_Kept_add, marked_Kept, na.rm = TRUE),
         marked_Released_total = sum(marked_Released_add, marked_Released, na.rm=TRUE),
         unmarked_Kept_total = sum(unmarked_Kept_add, unmarked_Kept, na.rm=TRUE),
         unmarked_Released_total = sum(unmarked_Released_add, unmarked_Released, na.rm=TRUE)) %>%
  ungroup()

Sport_mark_rate2<-Sport_mark_rate %>%
  dplyr::select(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE, marked_Kept_total, unmarked_Kept_total, marked_Released_total, unmarked_Released_total) %>%
  pivot_longer(cols=c(contains("total")), names_to = "status", values_to = "value") %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_wider(id_cols = c(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT), names_from = SOURCE, values_from = value) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rowwise() %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT) %>%
  mutate(creel_plus = sum(creel,lodge_log, na.rm=TRUE),
         creel_unfiltered_plus = sum(creel_unfiltered,lodge_log, na.rm=TRUE),
         historic_plus = sum(historic,lodge_log, na.rm=TRUE)) %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT) %>%
  mutate(catch_estimate_cat = case_when(
    YEAR > 2012 & MONTH %in% c(5:9) & (is.na(creel) | creel==0) ~ "use_irec",
    YEAR > 2012 & MONTH %in% c(1:4,10:12) ~ "use_irec",
    YEAR < 2013 & (is.na(creel_plus) | creel_plus==0) & (!is.na(historic) & historic!=0)~ "use_historic",
    YEAR < 2013 & (is.na(creel_plus) | creel_plus==0) ~ "creel_plus_zero",
    TRUE ~ "use_creel_plus")) %>%
  ungroup() %>% dplyr::select(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, catch_estimate_cat)



Sport_mark_rate3<-Sport_mark_rate %>%
  dplyr::select(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE, marked_Kept_total, unmarked_Kept_total, marked_Released_total, unmarked_Released_total) %>%
  pivot_longer(cols=c(contains("total")), names_to = "status", values_to = "value") %>%
  pivot_wider(id_cols = c(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, status), names_from = SOURCE, values_from = value) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rowwise() %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, status) %>%
  mutate(creel_plus = sum(creel,lodge_log, na.rm=TRUE),
         creel_unfiltered_plus = sum(creel_unfiltered,lodge_log, na.rm=TRUE),
         historic_plus = sum(historic,lodge_log, na.rm=TRUE)) %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT) %>%
  left_join(Sport_mark_rate2) %>%
  mutate(catch_estimate = case_when(
    catch_estimate_cat == "use_irec" ~ as.numeric(irec_calibrated),
    catch_estimate_cat == "use_historic" ~  as.numeric(historic_plus),
    catch_estimate_cat == "use_creel_plus" ~ as.numeric(creel_plus),
    catch_estimate_cat == "creel_zero" ~ as.numeric(creel_plus))) %>%
  ungroup() %>%
  relocate(catch_estimate_cat, .after=status)


#presumably this is now the correct data at the PFMA level ...

# Now we need to sum up to the appropriate finescale fishery level, and do a calculation of how much more irec gets us.
#Need to split the finescale fisheries now
Sport_mark_rate_finescale<-
  Sport_mark_rate3%>% ungroup %>%
  mutate(finescale_fishery = case_when(
    AREA%in%c( "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="fall" ~ "NWCVI S FALL AABM",
    AREA%in%c( "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="spring" ~ "NWCVI S SPRING AABM",
    AREA%in%c( "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="summer" ~ "NWCVI S SUMMER AABM",

    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="AABM" & season=="fall" ~ "SWCVI S FALL AABM",
    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="AABM" & season=="spring" ~ "SWCVI S SPRING AABM",
    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="AABM" & season=="summer" ~ "SWCVI S SUMMER AABM",

    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(10:12) ~ "SWCVI S FALL AABM",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & season=="spring" ~ "SWCVI S SPRING AABM",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(5:7) ~ "SWCVI S SUMMER AABM",

    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10:12) ~ "NWCVI S FALL AABM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & season=="spring" ~ "NWCVI S SPRING AABM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(5,6) ~ "NWCVI S SUMMER AABM",

    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="ISBM" & MONTH%in%c(8:9) ~ "SWCVI S SUMMER ISBM",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)") & MONTH%in%c(8,9) ~ "SWCVI S SUMMER ISBM",

    AREA%in%c("Area 125", "Area 126", "Area 127") & MANAGEMENT=="ISBM" & MONTH%in%c(7:9) ~ "NWCVI S SUMMER ISBM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "NWCVI S SUMMER ISBM",

    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="fall" ~ "NGS S FALL",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="spring" ~ "NGS S SPRING",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="summer" ~ "NGS S SUMMER",


    (AREA %in%c("Area 17", "Area 18", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="fall" ~ "SGS S FALL", #this captures 19
    (AREA %in%c("Area 17", "Area 18", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="spring" ~ "SGS S SPRING",
    (AREA %in%c("Area 17", "Area 18", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="summer" ~ "SGS S SUMMER",


    (AREA %in% c("Area 11", "Area 111", "Area 12") | REGION2 == "JST") & season=="fall" ~ "JNST S FALL",
    (AREA %in% c("Area 11", "Area 111", "Area 12")| REGION2 == "JST") & season=="spring" ~ "JNST S SPRING",
    (AREA %in% c("Area 11", "Area 111", "Area 12")| REGION2 == "JST") & season=="summer" ~ "JNST S SUMMER",


    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="spring"   ~ "CBC S",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="fall"  ~ "CBC S",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="summer"   ~ "CBC S",


    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& season=="spring"   ~ "NBC AABM S SPRING",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& season=="fall"  ~ "NBC AABM S FALL",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& season=="summer"  ~ "NBC AABM S SUMMER",


    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& season=="spring"  ~ "NBC ISBM S SPRING",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& season=="fall"  ~ "NBC ISBM S FALL",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& season=="summer"  ~ "NBC ISBM S SUMMER",


    (AREA %in% c( "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="spring"  ~ "CA JDF S SPRING", #this captures 19
    (AREA %in% c( "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="fall"  ~ "CA JDF S FALL",
    (AREA %in% c( "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="summer"  ~ "CA JDF S SUMMER")) %>%


  mutate(finescale_fishery_old = case_when(
    AREA%in%c( "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & MONTH%in%c(1:12) ~ "NWCVI S AABM",
    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="AABM" & MONTH%in%c(1:12) ~ "SWCVI S AABM",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(1:7, 10:12) ~ "SWCVI S AABM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(1:6, 10:12) ~ "NWCVI S AABM",
    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="ISBM" & MONTH%in%c(7:12) ~ "SWCVI S ISBM",
    AREA%in%c("Area 125", "Area 126", "Area 127") & MANAGEMENT=="ISBM" & MONTH%in%c(7:12) ~ "NWCVI S ISBM",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)") & MONTH%in%c(8,9) ~ "SWCVI S ISBM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "NWCVI S ISBM",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& MONTH%in%c(1:12) ~ "NGS S",
    (AREA %in%c("Area 17", "Area 18",  "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& MONTH%in%c(1:12) ~ "SGS S",
    (AREA %in% c("Area 11", "Area 111", "Area 12") | REGION2 == "JST") & MONTH%in%c(1:12) ~ "JNST S",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")& MONTH%in%c(1:12)  ~ "CBC S",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& MONTH%in%c(1:12)  ~ "NBC AABM S",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& MONTH%in%c(1:12)  ~ "NBC ISBM S",
    (AREA %in% c( "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & MONTH%in%c(1:12) ~ "CA JDF S"))



Sport_mark_rate_finescale<- Sport_mark_rate_finescale %>% ungroup %>%
  mutate(summer_coverage_tf = case_when(
    finescale_fishery_old == "CA JDF S" & MONTH %in% c(7:8)  ~ "yes",
    finescale_fishery_old == "JNST S" & MONTH %in% c(7:8)  ~ "yes",
    finescale_fishery_old == "NGS S" & MONTH %in% c(7:8)  ~ "yes",
    finescale_fishery_old == "SGS S" & MONTH %in% c(7:8)& YEAR %notin% c(2016)  ~ "yes",
    finescale_fishery_old == "SWCVI S ISBM" & MONTH %in% c(8) ~ "yes",
    finescale_fishery_old == "SWCVI S AABM" & MONTH %in% c(7:8)  ~ "yes",
    finescale_fishery_old == "SWCVI S" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "NWCVI S ISBM" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "NWCVI S AABM" & MONTH %in% c(7:8)  ~ "yes",
    finescale_fishery_old == "NWCVI S" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "CBC S" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "NBC AABM S" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "NBC ISBM S" & MONTH %in% c(7:8) ~ "yes",
    .default = "no"))

Sport_mark_rate_finescale_sum<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery)) %>%
  group_by(YEAR, status, finescale_fishery_old, finescale_fishery) %>%
  summarise_at(vars(creel:catch_estimate), sum, na.rm=TRUE)

Sport_creel_finescale_summer<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old), summer_coverage_tf=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old) %>%
  summarise_at(vars(creel_plus), sum, na.rm=TRUE) %>%
  rename(creel_plus_summer=creel_plus)

Sport_historic_finescale_summer<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old), summer_coverage_tf=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old) %>%
  summarise_at(vars(historic), sum, na.rm=TRUE) %>%
  rename(historic_summer=historic)

Sport_creel_finescale_creel_effort<- Sport_mark_rate_finescale %>%
  full_join(creel_plus_effort) %>%
  mutate(creel_plus_done = case_when(
    is.na(creel_plus_done)~ "no",
    TRUE ~ creel_plus_done)) %>%
  filter(!is.na(finescale_fishery_old), summer_coverage_tf=="yes") %>%
  group_by(YEAR, finescale_fishery_old, finescale_fishery) %>% count(creel_plus_done) %>%
  pivot_wider(names_from = creel_plus_done, values_from = n) %>%
  mutate(creel_effort = sum(yes, na.rm=TRUE)/sum(no, yes, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::select(YEAR, finescale_fishery_old,creel_effort)

Sport_creel_finescale_historic_effort<- Sport_mark_rate_finescale %>%
  full_join(historic_effort) %>%
  mutate(historic_done = case_when(
    is.na(historic_done)~ "no",
    TRUE ~ historic_done)) %>%
  filter(!is.na(finescale_fishery_old), summer_coverage_tf=="yes") %>%
  group_by(YEAR, finescale_fishery_old, finescale_fishery) %>% count(historic_done) %>%
  pivot_wider(names_from = historic_done, values_from = n) %>%
  mutate(historic_effort = sum(yes, na.rm=TRUE)/sum(no, yes, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::select(YEAR, finescale_fishery_old,historic_effort)


#Year, finescale fishery
Sport_mark_rate_finescale_combined<-left_join(Sport_mark_rate_finescale_sum, Sport_creel_finescale_summer)
Sport_mark_rate_finescale_combined<-left_join(Sport_mark_rate_finescale_combined, Sport_historic_finescale_summer)
Sport_mark_rate_finescale_combined<-left_join(Sport_mark_rate_finescale_combined, Sport_creel_finescale_creel_effort)
Sport_mark_rate_finescale_combined<-left_join(Sport_mark_rate_finescale_combined, Sport_creel_finescale_historic_effort)

Sport_mark_rate_finescale_combined<-Sport_mark_rate_finescale_combined %>% mutate(mark_status = case_when(
  status %in% c("marked_Kept_total", "marked_Released_total") ~ "marked",
  TRUE ~ "unmarked"
))%>% mutate(kept_status = case_when(
  status %in% c("marked_Kept_total", "unmarked_Kept_total") ~ "Kept",
  TRUE ~ "Released"
))

Sport_mark_rate_finescale_combined<-Sport_mark_rate_finescale_combined %>% mutate(season = case_when(
  str_detect(finescale_fishery, "SUMMER")  ~ "summer",
  str_detect(finescale_fishery, "SPRING")  ~ "spring",
  str_detect(finescale_fishery, "FALL")  ~ "fall"))

write_rds(Sport_mark_rate_finescale, "Sport_mark_rate_finescale.RDS")
write_rds(Sport_mark_rate_finescale_combined, "Sport_mark_rate_finescale_combined.RDS")
