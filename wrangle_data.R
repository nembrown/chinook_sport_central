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

#Take out NC and CC and add back in from csv but KEEP IREC in
#Add back in NBC data from csv
Sport_filtered_south_irec<-Sport_filtered_south_irec |>
                           rbind(Sport_filtered_south_irec_unfiltered)|>
                           mutate(filter_NC = case_when(
                             REGION2 %in% c("NC", "CC") & SOURCE != "irec_calibrated" ~ "remove",
                             TRUE ~ "keep") ) |>
                           filter(filter_NC=="keep")|>
                           dplyr::select(-filter_NC)|>
                           rbind(nbc_aabm)

#Take filtered data and get a by-year estimate to in fill for NAs below

#2. Mark rate averaged across source
Sport_mark_rate_source<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) |>
  mutate(marked_prop_source = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT) %>% summarise(marked_prop_source =mean(marked_prop_source, na.rm=TRUE)) %>%
  dplyr::select(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, marked_prop_source) %>% ungroup()

#3. Mark rate monthly Regional average
Sport_mark_rate_REGION2<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_REGION2 =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  group_by(YEAR, REGION2, MONTH, season, SOURCE, MANAGEMENT) %>% summarise(marked_prop_REGION2 =mean(marked_prop_REGION2, na.rm=TRUE)) %>%
  dplyr::select(YEAR, MONTH, season, REGION2, SOURCE, MANAGEMENT, marked_prop_REGION2) %>% ungroup()

#4. Mark rate seasonal area average
Sport_mark_rate_seasonal<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_seasonal =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  group_by(YEAR, season, AREA, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_seasonal =mean(marked_prop_seasonal, na.rm=TRUE)) %>%
  dplyr::select(YEAR, season, AREA, REGION2, SOURCE, MANAGEMENT, marked_prop_seasonal) %>% ungroup()

#5. Mark rate seasonal regional average
Sport_mark_rate_seasonal_reg<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_seasonal_reg =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  group_by(YEAR,season, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_seasonal_reg =mean(marked_prop_seasonal_reg, na.rm=TRUE)) %>%
  dplyr::select(YEAR, season, REGION2, SOURCE, MANAGEMENT, marked_prop_seasonal_reg) %>% ungroup()

#6. Mark rate area month across years
Sport_mark_rate_area_month<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_area_month =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  group_by(AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT) %>% summarise(marked_prop_area_month =mean(marked_prop_area_month, na.rm=TRUE)) %>%
  dplyr::select(AREA, MONTH, season, REGION2, SOURCE, MANAGEMENT, marked_prop_area_month) %>% ungroup()

#7. Mark rate regional month across years
Sport_mark_rate_region_month<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, season, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, season), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_region_month =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
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
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(6:7) ~ "SWCVI S SUMMER AABM",

    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10:12) ~ "NWCVI S FALL AABM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & season=="spring" ~ "NWCVI S SPRING AABM",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(6) ~ "NWCVI S SUMMER AABM",

    AREA%in%c("Area 121", "Area 123", "Area 124") & MANAGEMENT=="ISBM" & MONTH%in%c(7:9) ~ "SWCVI S SUMMER ISBM",
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


    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="spring"   ~ "CBC S SPRING",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="fall"  ~ "CBC S FALL",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107") & season=="summer"   ~ "CBC S SUMMER",


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

ggplot(Sport_mark_rate_finescale_combined %>% filter(season=="summer")%>% filter(!str_detect(finescale_fishery, "CBC|NBC")), aes(y=creel_plus_summer+1, x=creel_effort)) +
  geom_point() + geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~finescale_fishery, scales="free")

ggplot(Sport_mark_rate_finescale_combined %>% filter(season=="summer")%>% filter(str_detect(finescale_fishery, "CBC|NBC")), aes(y=historic_summer+1, x=historic_effort)) +
  geom_point() + geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~finescale_fishery, scales="free")


#### Modelling
library(glmmTMB)
library(DHARMa)
 library(fitdistrplus)
# #library(MASS)
# library(car)
 library(lme4)
library(bbmle)
library(SuppDists)
library(MuMIn)

Season_south<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2013:2023)) %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_south_no_nas<-Season_south %>% drop_na(any_of(c("creel_plus_summer", "mark_status","status", "finescale_fishery_old", "season", "creel_effort", "kept_status")))

Season_model_full<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=gaussian, data = Season_south_no_nas)
summary(Season_model_full)
res <- simulateResiduals(Season_model_full, plot = T, quantreg=T)

#poisson
Season_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=poisson, data = Season_south_no_nas)
res_pois <- simulateResiduals(Season_model_full_poisson, plot = T, quantreg=T)
summary(Season_model_full_poisson)

#gamma
Season_model_full_gamma<- glm(formula = (catch_estimate+3) ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data=Season_south_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(Season_model_full_gamma, plot = T, quantreg=T)
summary(Season_model_full_gamma)

AICtab(Season_model_full,Season_model_full_poisson, Season_model_full_gamma)

#drop_ a single term from the full interaction model:
Season_model_gamma_drop_kept<- glm(formula = catch_estimate + 1 ~creel_plus_summer*mark_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept <- simulateResiduals(Season_model_gamma_drop_kept, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept)

Season_model_gamma_drop_mark<- glm(formula = catch_estimate + 1 ~creel_plus_summer*kept_status*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_mark <- simulateResiduals(Season_model_gamma_drop_mark, plot = T, quantreg=T)
summary(Season_model_gamma_drop_mark)

 Season_model_gamma_drop_fishery<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
 res_gam_drop_fishery <- simulateResiduals(Season_model_gamma_drop_fishery, plot = T, quantreg=T)
 summary(Season_model_gamma_drop_fishery)

Season_model_gamma_drop_season<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_season <- simulateResiduals(Season_model_gamma_drop_season, plot = T, quantreg=T)
summary(Season_model_gamma_drop_season)

Season_model_gamma_drop_effort<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*kept_status*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_effort <- simulateResiduals(Season_model_gamma_drop_effort, plot = T, quantreg=T)
summary(Season_model_gamma_drop_effort)

AICtab(Season_model_full_gamma,  Season_model_gamma_drop_fishery, Season_model_gamma_drop_kept, Season_model_gamma_drop_mark,  Season_model_gamma_drop_season, Season_model_gamma_drop_effort)

#drop_ kept is the best... try sequentially dropping terms:

Season_model_gamma_drop_kept_mark<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_mark <- simulateResiduals(Season_model_gamma_drop_kept_mark, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_mark)

Season_model_gamma_drop_kept_season<- glm(formula = catch_estimate + 1 ~creel_plus_summer*mark_status*finescale_fishery_old*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_season <- simulateResiduals(Season_model_gamma_drop_kept_season, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_season)

Season_model_gamma_drop_kept_fishery<- glm(formula = catch_estimate+3 ~creel_plus_summer*mark_status*season*creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_fishery <- simulateResiduals(Season_model_gamma_drop_kept_fishery, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_fishery)

Season_model_gamma_drop_kept_effort<- glm(formula = catch_estimate+1 ~creel_plus_summer*mark_status*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort <- simulateResiduals(Season_model_gamma_drop_kept_effort, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort)

AICtab(Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_mark, Season_model_gamma_drop_kept_season, Season_model_gamma_drop_kept_fishery, Season_model_gamma_drop_kept_effort)
#####dropping kept only is the best - maybe efffort

##Any further terms? no.
#####kept and effort only.

Season_model_gamma_drop_kept_effort_mark<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_mark_effort <- simulateResiduals(Season_model_gamma_drop_kept_effort_mark, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_mark)

Season_model_gamma_drop_kept_effort_fishery<- glm(formula = catch_estimate+1 ~creel_plus_summer*season*mark_status,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_effort_fishery <- simulateResiduals(Season_model_gamma_drop_kept_effort_fishery, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_fishery)

Season_model_gamma_drop_kept_effort_season<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*mark_status,  family=Gamma(link = "log"), data = Season_south)
# res_gam_drop_kept_effort_season <- simulateResiduals(Season_model_gamma_drop_kept_effort_season, plot = T, quantreg=T)
# summary(Season_model_gamma_drop_kept_effort_season)

AICtab(Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_effort, Season_model_gamma_drop_kept_effort_mark, Season_model_gamma_drop_kept_effort_fishery, Season_model_gamma_drop_kept_effort_season)



#Now changing around model specification:
#First find the model with drop kept and effort that is the best.Then add back in effort.
Season_model_gamma_drop_kept_effort_2<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*mark_status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)
dd<-dredge(Season_model_gamma_drop_kept_effort_2, fixed= ~ creel_plus_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
summary(get.models(dd, 1)[[1]])

#Model with both dropped kept and effort that fits the best:
Season_model_gamma_drop_kept_effort_spec<- glm(formula = catch_estimate+1 ~creel_plus_summer+finescale_fishery_old+season+mark_status+
                                               creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
                                               finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
                                               creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season ,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_effort_spec <- simulateResiduals(Season_model_gamma_drop_kept_effort_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort_spec)

#Make sure the specifications is better than the full interactive model
AICtab(Season_model_gamma_drop_kept_effort, Season_model_gamma_drop_kept_effort_spec)

# try adding effort back in
Season_model_gamma_drop_kept_2<- glm(formula = catch_estimate+1 ~creel_plus_summer*finescale_fishery_old*season*creel_effort*mark_status,  family=Gamma(link = "log"), data = Season_south_no_nas, na.action = na.fail)

#fix the factors that we know are the best here
dd2<-dredge(Season_model_gamma_drop_kept_2, fixed = ~
              creel_plus_summer+finescale_fishery_old+season+mark_status+
              creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
              finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
              creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season )
subset(dd2, delta < 2)
plot(dd2, labAsExpr = TRUE)
summary(get.models(dd2, 1)[[1]])

#Try the model with effort added back in:
Season_model_gamma_drop_kept_spec<- glm(formula = catch_estimate+1 ~creel_plus_summer+finescale_fishery_old+season+creel_effort+mark_status+
                                          creel_plus_summer:creel_effort + creel_effort:finescale_fishery_old + creel_effort:mark_status + creel_effort:season +
                                          creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:mark_status+
                                          finescale_fishery_old:season + finescale_fishery_old:mark_status + season:mark_status +
                                          creel_plus_summer:finescale_fishery_old:mark_status + creel_plus_summer:finescale_fishery_old:season +
                                          creel_plus_summer:creel_effort:mark_status + creel_effort:finescale_fishery_old:season + creel_effort:mark_status:season  ,  family=Gamma(link = "log"), data = Season_south_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(Season_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_spec)

AICtab(Season_model_gamma_drop_kept_spec, Season_model_gamma_drop_kept_effort_spec, Season_model_gamma_drop_kept, Season_model_gamma_drop_kept_effort)
#adding effort does improve the model.



#
# #try adding kept status back in:
# Season_model_gamma_spec<- glm(formula = catch_estimate+3 ~creel_plus_summer+finescale_fishery_old+season+creel_effort+mark_status+ kept_status+
#                                 creel_plus_summer:finescale_fishery_old + creel_plus_summer:season + creel_plus_summer:creel_effort +creel_plus_summer:mark_status+
#                                 finescale_fishery_old:season + finescale_fishery_old:creel_effort+
#                                 season:creel_effort + season:mark_status +
#                                 creel_effort:mark_status +
#                                 creel_plus_summer:finescale_fishery_old:creel_effort +creel_plus_summer:creel_effort:mark_status+
#                                 finescale_fishery_old:season:creel_effort,  family=Gamma(link = "log"), data = Season_south_no_nas)
# res_gam_spec <- simulateResiduals(Season_model_gamma_spec, plot = T, quantreg=T)
# summary(Season_model_gamma_spec)
#
# AICtab(Season_model_gamma_drop_kept_mark, Season_model_gamma_drop_kept_mark_spec, Season_model_gamma_drop_kept_spec, Season_model_gamma_spec, Season_model_gamma_drop_kept)
# #kept status should be dropped fully
#


### Selected model:
Season_model_gamma_drop_kept_spec
res_gam_drop_kept_spec <- simulateResiduals(Season_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(Season_model_gamma_drop_kept_effort_spec)

testDispersion(Season_model_gamma_drop_kept_spec)
simulationOutput <- simulateResiduals(fittedModel = Season_model_gamma_drop_kept_spec, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_south_no_nas$creel_plus_summer)

plotResiduals(simulationOutput, form = na.omit(Season_south$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_south$season))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
# plotConventionalResiduals(fittedModel)
plot(simulationOutput, quantreg = T)



#### Adding data back in post modelling
#Adding predicted data
Season_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(!str_detect(finescale_fishery, "CBC|NBC"))
Season_south_old_new<-predict.glm(Season_model_gamma_drop_kept_spec, newdata =  Season_south_old, type = "response")
Season_south_old_new_2<-Season_south_old %>%   mutate(catch_estimate_predicted = Season_south_old_new)

Season_south2<-Season_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_south_combined<- rbind(Season_south_old_new_2, Season_south2)



#### Testin two different plots:
ggplot(Season_south_combined %>% filter(finescale_fishery=="CA JDF S FALL"), aes(x=creel_plus_summer, y= catch_estimate_predicted+1, col=mark_status, fill=mark_status))+
  geom_point(aes(shape=pred_cat, size=creel_effort))+
  geom_smooth(method="glm", method.args = list(family= Gamma(link = "log")), fullrange=TRUE) + facet_wrap(~mark_status, scales="free") +
  ggtitle(paste("CA JDF S FALL")) + theme_bw() +
  scale_colour_viridis_d(option = "turbo")+  scale_fill_viridis_d(option = "turbo") +scale_size_continuous(range=c(1,3))


Season_south_combined_2 <- add_column(Season_south_combined, fit = predict(Season_model_gamma_drop_kept_spec, newdata = Season_south_combined, type = 'response'))
Season_south_combined_2  <- bind_cols(Season_south_combined_2 , setNames(as_tibble(predict(Season_model_gamma_drop_kept_spec, Season_south_combined , se.fit = TRUE)[1:2]), c('fit_link','se_link')))

family.set <- family(Season_model_gamma_drop_kept_spec)
ilink.family.set<- family.set$linkinv

Season_south_combined_2  <- mutate(Season_south_combined_2 ,
fit_resp  = ilink.family.set(fit_link),
right_upr = ilink.family.set(fit_link + (2 * se_link)),
right_lwr = ilink.family.set(fit_link - (2 * se_link)))


ggplot(Season_south_combined_2 %>% filter(finescale_fishery=="CA JDF S FALL"), aes(x=creel_plus_summer, y= catch_estimate_predicted+1, col=mark_status, fill=mark_status))+
 geom_point(aes(shape=pred_cat, size=creel_effort))+
  geom_line()+
 geom_ribbon(aes(ymin = right_lwr, ymax = right_upr, fill=mark_status), alpha = 0.10)+
  facet_wrap(~mark_status, scales="free") +
  ggtitle(paste("CA JDF S FALL")) + theme_bw() +
  scale_colour_viridis_d(option = "turbo")+  scale_fill_viridis_d(option = "turbo") +scale_size_continuous(range=c(1,3))


plt.gam.hydroid <- ggplot(ndata.hydroid, aes(x = min.10.pH.unscaled, y = fit)) +
  geom_line(aes(colour=oFood.quality)) +
  geom_point(aes(y = hydroid.001, shape=CO2, colour=oFood.quality), data = food.exp.data.12.2019_zscores)+
  xlab(expression("Minimum" ~"10"^"th"~"percentile pH")) + ylab(expression(atop(NA,atop(textstyle(italic("Obelia")~ "abundance"), textstyle("(proportion cover)")))))+
  scale_color_manual(values=colorset2, guide = guide_legend(title="Food supplement", title.position = "top"))+
  scale_fill_manual(values=colorset2, guide = FALSE)+
  scale_shape_manual(values=c(19,17), labels=c("Ambient", "Low pH"), guide = guide_legend(title="pH treatment", title.position = "top"))+
  geom_ribbon(data = ndata.hydroid,aes(ymin = right_lwr, ymax = right_upr, fill=oFood.quality), alpha = 0.10)+
  theme(legend.position='none')
plt.gam.hydroid
ggsave("C:Data//Graphs March 2020//hydroid_pred.png")




### NBC AABM modelling

Season_north_aabm<-Sport_mark_rate_finescale_combined%>% filter(YEAR %in% c(2013:2023)) %>% filter(finescale_fishery_old == "NBC AABM S")

#Modelling comparisons need to be done on models with same # of NAs - so drop nas
Season_north_aabm_no_nas<-Season_north_aabm %>% drop_na(any_of(c("historic_summer", "mark_status", "finescale_fishery_old", "season", "historic_effort", "kept_status")))

North_aabm_model_full<- glm(formula = catch_estimate + 1 ~ historic_summer*mark_status*kept_status*season*historic_effort,  family=gaussian, data = Season_north_aabm_no_nas)
summary(North_aabm_model_full)
res <- simulateResiduals(North_aabm_model_full, plot = T, quantreg=T)

#poisson
North_aabm_model_full_poisson<- glm(formula = catch_estimate + 1 ~historic_summer*mark_status*kept_status*season*historic_effort,  family=poisson, data = Season_north_aabm_no_nas)
res_pois <- simulateResiduals(North_aabm_model_full_poisson, plot = T, quantreg=T)
summary(North_aabm_model_full_poisson)

#gamma
North_aabm_model_full_gamma<- glm(formula = (catch_estimate+1) ~historic_summer*mark_status*kept_status*season*historic_effort,  family=Gamma(link = "log"), data=Season_north_aabm_no_nas, na.action = na.fail)
res_gam <- simulateResiduals(North_aabm_model_full_gamma, plot = T, quantreg=T)
summary(North_aabm_model_full_gamma)

AICtab(North_aabm_model_full,North_aabm_model_full_poisson, North_aabm_model_full_gamma)

#gamma is the best
#drop_ a single term from the full interaction model:
North_aabm_model_gamma_drop_kept<- glm(formula = catch_estimate + 1 ~historic_summer*mark_status*season*historic_effort,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept <- simulateResiduals(North_aabm_model_gamma_drop_kept, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept)

North_aabm_model_gamma_drop_mark<- glm(formula = catch_estimate + 1 ~historic_summer*kept_status*season*historic_effort,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_mark <- simulateResiduals(North_aabm_model_gamma_drop_mark, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_mark)

North_aabm_model_gamma_drop_season<- glm(formula = catch_estimate+5 ~historic_summer*mark_status*kept_status*historic_effort,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_season <- simulateResiduals(North_aabm_model_gamma_drop_season, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_season)

North_aabm_model_gamma_drop_effort<- glm(formula = catch_estimate+1 ~historic_summer*mark_status*kept_status*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_effort <- simulateResiduals(North_aabm_model_gamma_drop_effort, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_effort)

AICtab(North_aabm_model_full_gamma, North_aabm_model_gamma_drop_kept, North_aabm_model_gamma_drop_mark,  North_aabm_model_gamma_drop_season, North_aabm_model_gamma_drop_effort)

## drop_ kept is the best... try sequentially dropping terms:

North_aabm_model_gamma_drop_kept_mark<- glm(formula = catch_estimate+1 ~historic_summer*season*historic_effort,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_mark <- simulateResiduals(North_aabm_model_gamma_drop_kept_mark, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept_mark)

North_aabm_model_gamma_drop_kept_season<- glm(formula = catch_estimate + 5 ~historic_summer*mark_status*historic_effort,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_season <- simulateResiduals(North_aabm_model_gamma_drop_kept_season, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept_season)

North_aabm_model_gamma_drop_kept_effort<- glm(formula = catch_estimate+1 ~historic_summer*mark_status*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_effort <- simulateResiduals(North_aabm_model_gamma_drop_kept_effort, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept_effort)

AICtab(North_aabm_model_gamma_drop_kept, North_aabm_model_gamma_drop_kept_mark, North_aabm_model_gamma_drop_kept_season, North_aabm_model_gamma_drop_kept_effort)

#####dropping kept only is the best.
#Now changing around model specification:
North_aabm_model_gamma_drop_kept_2<- glm(formula = catch_estimate+1 ~historic_summer*season*historic_effort*mark_status,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas, na.action = na.fail)
dd<-dredge(North_aabm_model_gamma_drop_kept_2, fixed= ~ historic_summer)
subset(dd, delta < 2)
plot(dd, labAsExpr = TRUE)
#
summary(get.models(dd, 1)[[1]])

#The model with AIC <2 factors added back in:
North_aabm_model_gamma_drop_kept_spec<- glm(formula = catch_estimate+1 ~historic_summer+season+mark_status+historic_effort*historic_summer +
                                                      historic_effort*mark_status + historic_summer*mark_status + historic_summer*season + mark_status*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(North_aabm_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept_spec)

AICtab(North_aabm_model_gamma_drop_kept, North_aabm_model_gamma_drop_kept_spec)
#kept status should be dropped fully but mark status dropped only partially.



### Selected model:
North_aabm_model_gamma_drop_kept_spec<- glm(formula = catch_estimate+1 ~historic_summer+season+mark_status+historic_effort*historic_summer +
                                              historic_effort*mark_status + historic_summer*mark_status + historic_summer*season + mark_status*season,  family=Gamma(link = "log"), data = Season_north_aabm_no_nas)
res_gam_drop_kept_spec <- simulateResiduals(North_aabm_model_gamma_drop_kept_spec, plot = T, quantreg=T)
summary(North_aabm_model_gamma_drop_kept_spec)

testDispersion(North_aabm_model_gamma_drop_kept_spec)
simulationOutput <- simulateResiduals(fittedModel = North_aabm_model_gamma_drop_kept_mark, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
plotResiduals(simulationOutput, Season_north_aabm_no_nas$historic_summer)

plotResiduals(simulationOutput, form = na.omit(Season_north_aabm$finescale_fishery_old))

testCategorical(simulationOutput, catPred = na.omit(Season_north_aabm$season))


#### Adding data back in post modelling
#Adding predicted data
Season_north_aabm_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012)) %>% ungroup() %>% mutate(pred_cat = "predicted") %>% filter(finescale_fishery_old == "NBC AABM S")
Season_north_aabm_old_new<-predict.glm(North_aabm_model_gamma_drop_kept_spec, newdata =  Season_north_aabm_old, type = "response")
Season_north_aabm_old_new_2<-Season_north_aabm_old %>%   mutate(catch_estimate_predicted = Season_north_aabm_old_new)

Season_north_aabm2<-Season_north_aabm %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")
Season_north_aabm_combined<- rbind(Season_north_aabm_old_new_2, Season_north_aabm2)



#### MRR URR

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

