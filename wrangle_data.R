

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
Sport_filtered_south_irec<-
  estimates |>
  as_tibble() |>
  mutate(INCLUDE_15 = case_when(
    SOURCE != "Creel" ~ 1,
    TRUE ~ INCLUDE_15
  ))|>
  filter(INCLUDE_15 ==1, YEAR<2024)|>
  filter(AREA %notin% c("Area 29 (In River)", "Campbell River", "Quinsam River", "CR-1", "CR-2", "CR-3", "CR-4", "QR-1", "QR-2", "QR-3", "QR-4")) |>
   mutate(AREA = case_when(
    AREA== "Area 29 (Marine)" ~ "Area 29",
    TRUE ~ as.character(AREA))) |>
  filter(SUB_TYPE == "LEGAL") |>
  mutate(REGION2 = case_when(AREA == "Area 2" ~ "NC", TRUE ~ REGION2)) |>
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
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) |>
  summarise(VARIANCE=sum(VARIANCE), VAL=sum(ESTIMATE)) |> ungroup()


#Take filtered data and get a by-year estimate to in fill for NAs below

#Mark rate by aggregated source
Sport_mark_rate_source<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, MONTH, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, MONTH, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_source = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  group_by(YEAR, AREA, MONTH, REGION2, MANAGEMENT) %>% summarise(marked_prop_source =mean(marked_prop_source, na.rm=TRUE)) %>%
  select(YEAR, AREA, MONTH, REGION2, MANAGEMENT, marked_prop_source) %>% ungroup()

#region and month and year
Sport_mark_rate_REGION2<- Sport_filtered_south_irec  %>%
  group_by(YEAR, REGION2, MONTH, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, REGION2, MONTH, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_REGION2 =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE))%>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(YEAR, REGION2, MONTH, MANAGEMENT, SOURCE, marked_prop_REGION2)%>% ungroup()


#aggregated source region and all years
Sport_mark_rate_REGION2_source<- Sport_filtered_south_irec  %>%
  group_by(REGION2,  MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c( REGION2,  SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_REGION2_source =sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE))%>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  group_by(REGION2, MANAGEMENT) %>% summarise(marked_prop_REGION2_source =mean(marked_prop_REGION2_source, na.rm=TRUE)) %>%
  select(REGION2, MANAGEMENT, marked_prop_REGION2_source)%>% ungroup()


#Mark rate by Area/month, across all years
Sport_mark_rate_area_month<- Sport_filtered_south_irec  %>%
  group_by(AREA, MONTH, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(AREA, MONTH, REGION2, SOURCE, MANAGEMENT), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_area_month = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(AREA, MONTH, REGION2, MANAGEMENT, SOURCE, marked_prop_area_month)%>% ungroup()

#Mark rate by year for given area, whole year
Sport_mark_rate_year<- Sport_filtered_south_irec  %>%
  group_by(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(YEAR, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_year = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(YEAR, AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_year)%>% ungroup()

#Mark rate by Area, across all years and months
Sport_mark_rate_area<- Sport_filtered_south_irec  %>%
  group_by(AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL)) %>%
  pivot_wider(id_cols = c(AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  mutate(marked_prop_area = sum(marked_Kept,marked_Released, na.rm = TRUE)/sum(marked_Kept,marked_Released,unmarked_Kept,unmarked_Released, na.rm =TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  select(AREA, REGION2, MANAGEMENT, SOURCE, marked_prop_area)%>% ungroup()


#expand to include all combinations

allobs2 <- expand(Sport_filtered_south_irec, nesting(AREA, REGION2, MANAGEMENT), YEAR, MONTH, MARKS_DESC, TYPE, SOURCE)

#Join with mark rate data and expand unchecked
Sport_mark_rate<- Sport_filtered_south_irec  %>%
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>% summarise(sum=sum(VAL), sum_VARIANCE=sum(VARIANCE)) %>%
  ungroup() %>%
  full_join(allobs2) %>%
  pivot_wider(id_cols = c(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE), names_from=c(MARKS_DESC, TYPE), values_from = sum) %>%
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE) %>%
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
    TRUE~marked_prop_REGION2_source)) %>%
  mutate(marked_Kept_add = marked_prop_use*unchecked_Kept,
         marked_Released_add = marked_prop_use*unchecked_Released,
         unmarked_Kept_add = (1-marked_prop_use)*unchecked_Kept,
         unmarked_Released_add = (1-marked_prop_use)*unchecked_Released) %>%
  mutate(marked_Kept_total = sum(marked_Kept_add, marked_Kept, na.rm = TRUE),
         marked_Released_total = sum(marked_Released_add, marked_Released, na.rm=TRUE),
         unmarked_Kept_total = sum(unmarked_Kept_add, unmarked_Kept, na.rm=TRUE),
         unmarked_Released_total = sum(unmarked_Released_add, unmarked_Released, na.rm=TRUE)) %>%
  ungroup() %>%
  select(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, marked_Kept_total, unmarked_Kept_total, marked_Released_total, unmarked_Released_total) %>%
  pivot_longer(cols=c(contains("total")), names_to = "status", values_to = "value") %>%
  pivot_wider(id_cols = c(YEAR, MONTH, AREA, REGION2, MANAGEMENT, status), names_from = SOURCE, values_from = value) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rowwise() %>%
  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, status) %>%
  mutate(creel_plus = sum(creel,lodge_log, na.rm=TRUE),
         historic_plus = sum(historic,lodge_log, na.rm=TRUE)) %>%
  mutate(catch_estimate = case_when(
    YEAR > 2012 & MONTH %in% c(5:9) & (is.na(creel) | creel==0) ~ as.numeric(irec_calibrated),
    YEAR > 2012 & MONTH %in% c(1:4,10:12) ~ as.numeric(irec_calibrated),
    YEAR < 2013 & (is.na(creel_plus) | creel_plus==0) ~ as.numeric(historic_plus),
    TRUE ~ as.numeric(creel_plus))) %>%
  ungroup()

#presumably this is now the correct data at the PFMA level ...

# Now we need to sum up to the appropriate finescale fishery level, and do a calculation of how much more irec gets us.
#Need to split the finescale fisheries now
Sport_mark_rate_finescale<-
  Sport_mark_rate%>% ungroup %>%
  mutate(finescale_fishery = case_when(
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & MONTH%in%c(1:4) ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & MONTH%in%c(5:9) ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(1:4) ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(5:7) ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(1:4) ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(5:6) ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="ISBM" & MONTH%in%c(7:9) ~ "WCVI ISBM S SUMMER",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)") & MONTH%in%c(8,9) ~ "WCVI ISBM S SUMMER",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "WCVI ISBM S SUMMER",

    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& MONTH%in%c(10:12) ~ "NGS S FALL",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& MONTH%in%c(1:4) ~ "NGS S SPRING",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& MONTH%in%c(5:9) ~ "NGS S SUMMER",


    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& MONTH%in%c(10:12) ~ "SGS S FALL", #this captures 19
    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& MONTH%in%c(1:4) ~ "SGS S SPRING",
    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& MONTH%in%c(5:9) ~ "SGS S SUMMER",


    (AREA %in% c("Area 11", "Area 111", "Area 12") | REGION2 == "JST") & MONTH%in%c(10:12) ~ "JNST S FALL",
    (AREA %in% c("Area 11", "Area 111", "Area 12")| REGION2 == "JST") & MONTH%in%c(1:4) ~ "JNST S SPRING",
    (AREA %in% c("Area 11", "Area 111", "Area 12")| REGION2 == "JST") & MONTH%in%c(5:9) ~ "JNST S SUMMER",


    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")& MONTH%in%c(1:4)  ~ "CBC S SPRING",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")& MONTH%in%c(10:12)  ~ "CBC S FALL",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")& MONTH%in%c(5:9)  ~ "CBC S SUMMER",


    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& MONTH%in%c(1:4)  ~ "NBC AABM S SPRING",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& MONTH%in%c(10:12) ~ "NBC AABM S FALL",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& MONTH%in%c(5:9) ~ "NBC AABM S SUMMER",


    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& MONTH%in%c(1:4)  ~ "NBC ISBM S SPRING",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& MONTH%in%c(10:12)  ~ "NBC ISBM S FALL",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& MONTH%in%c(5:9)  ~ "NBC ISBM S SUMMER",


    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & MONTH%in%c(1:4) ~ "CA JDF S SPRING", #this captures 19
    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & MONTH%in%c(10:12) ~ "CA JDF S FALL",
    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & MONTH%in%c(5:9) ~ "CA JDF S SUMMER")) %>%

  mutate(finescale_fishery_term = case_when(
    AREA%in%c("Area 123", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(8:12) ~ "TWCVI S SUMMER",
    AREA %in%  c("Area 11", "Area 111", "Area 12") & MONTH%in%c(10:12) ~ "TJOHN ST S FALL",
    AREA %in%  c("Area 11", "Area 111", "Area 12") & MONTH%in%c(5:9) ~ "TJOHN ST S SUMMER",
    (AREA%in% c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29")  |  REGION2 == "GSS") & MONTH%in%c(10:12) ~ "TSGS S FALL",
    (AREA%in% c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29")  |  REGION2 == "GSS") & MONTH%in%c(5:9) ~ "TSGS S SUMMER",
    (AREA%in% c("Area 13", "Area 14", "Area 15", "Area 16") |  REGION2 == "GSN") & MONTH%in%c(10:12) ~ "TNGS S FALL",
    (AREA%in% c("Area 13", "Area 14", "Area 15", "Area 16") |  REGION2 == "GSN") & MONTH%in%c(5:9) ~ "TNGS S SUMMER",
    (AREA %in%c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")  | REGION2 == "JDF") & MONTH%in%c(5:9) ~ "TCA JDF S SUMMER")) %>%

  mutate(finescale_fishery_old = case_when(
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & MONTH%in%c(1:12) ~ "WCVI AABM S",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(1:7, 10:12) ~ "WCVI AABM S",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(1:6, 10:12) ~ "WCVI AABM S",
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="ISBM" & MONTH%in%c(7:12) ~ "WCVI ISBM S",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)") & MONTH%in%c(8,9) ~ "WCVI ISBM S",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "WCVI ISBM S",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& MONTH%in%c(1:12) ~ "NGS S",
    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& MONTH%in%c(1:12) ~ "SGS S",
    (AREA %in% c("Area 11", "Area 111", "Area 12") | REGION2 == "JST") & MONTH%in%c(1:12) ~ "JNST S",
    AREA %in% c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")& MONTH%in%c(1:12)  ~ "CBC S",
    AREA %in% c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")& MONTH%in%c(1:12)  ~ "NBC AABM S",
    AREA %in% c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")& MONTH%in%c(1:12)  ~ "NBC ISBM S",
    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & MONTH%in%c(1:12) ~ "CA JDF S")) %>%

  mutate(finescale_fishery_old_term = case_when(
    AREA%in%c("Area 123", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(8:12) ~ "TWCVI S",
    AREA %in%  c("Area 11", "Area 111", "Area 12") & MONTH%in%c(5:12) ~ "TJOHN ST S",
    (AREA%in% c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29")  |  REGION2 == "GSS") & MONTH%in%c(5:12) ~ "TSGS S",
    (AREA%in% c("Area 13", "Area 14", "Area 15", "Area 16") |  REGION2 == "GSN") & MONTH%in%c(5:12) ~ "TNGS S",
    (AREA %in%c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")  | REGION2 == "JDF") & MONTH%in%c(5:8) ~ "TCA JDF S"))



Sport_mark_rate_finescale<- Sport_mark_rate_finescale %>% ungroup %>%
  mutate(summer_coverage_tf = case_when(
    finescale_fishery_old == "CA JDF S" & MONTH %in% c(5:9) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "JNST S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "NGS S" & MONTH %in% c(6:9) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "SGS S" & MONTH %in% c(6:9) & YEAR %notin% c(2016, 2020) ~ "yes",
    finescale_fishery_old == "WCVI ISBM S" & MONTH %in% c(7:9) ~ "yes",
    finescale_fishery_old == "WCVI AABM S" & MONTH %in% c(6:9)& YEAR != 2020 ~ "yes",
    finescale_fishery_old == "CBC S" & MONTH %in% c(6:8) ~ "yes",
    finescale_fishery_old == "NBC AABM S" & MONTH %in% c(6:9) ~ "yes",
    finescale_fishery_old == "NBC ISBM S" & MONTH %in% c(6:8) ~ "yes",
    .default = "no")) %>%
  mutate(summer_coverage_tf_term = case_when(
    finescale_fishery_old_term == "TCA JDF S" & MONTH %in% c(5:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TJOHN ST S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TNGS S" & MONTH %in% c(6:9) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TSGS S" & MONTH %in% c(6:9) & YEAR %notin% c(2016, 2020) ~ "yes",
    finescale_fishery_old_term == "TWCVI S" & MONTH %in% c(8:9) ~ "yes",
    TRUE ~ "no")) %>% mutate(spring_coverage_tf = case_when(
      finescale_fishery_old == "CA JDF S" & MONTH %in% c(4) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "JNST S" & MONTH %in% c(6) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "NGS S" & MONTH %in% c(6) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "SGS S" & MONTH %in% c(4,5) & YEAR %notin% c(2016, 2020) ~ "yes",
      finescale_fishery_old == "WCVI AABM S" & MONTH %in% c(6)& YEAR != 2020 ~ "yes",
      finescale_fishery_old == "CBC S" & MONTH %in% c(6) ~ "yes",
      finescale_fishery_old == "NBC AABM S" & MONTH %in% c(6) ~ "yes",
      finescale_fishery_old == "NBC ISBM S" & MONTH %in% c(6) ~ "yes",
      .default = "no")) %>%
  mutate(fall_coverage_tf = case_when(
    finescale_fishery_old == "CA JDF S" & MONTH %in% c(9) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "JNST S" & MONTH %in% c(8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "NGS S" & MONTH %in% c(9) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "SGS S" & MONTH %in% c(9) & YEAR %notin% c(2016) ~ "yes",
    finescale_fishery_old == "WCVI ISBM S" & MONTH %in% c(9) ~ "yes",
    finescale_fishery_old == "WCVI AABM S" & MONTH %in% c(9) ~ "yes",
    finescale_fishery_old == "CBC S" & MONTH %in% c(8) ~ "yes",
    finescale_fishery_old == "NBC AABM S" & MONTH %in% c(9) ~ "yes",
    finescale_fishery_old == "NBC ISBM S" & MONTH %in% c(8) ~ "yes",
    .default = "no")) %>%
  mutate(fall_coverage_tf_term = case_when(
    finescale_fishery_old_term == "TCA JDF S" & MONTH %in% c(8) ~ "yes",
    finescale_fishery_old_term == "TJOHN ST S" & MONTH %in% c(8)  ~ "yes",
    finescale_fishery_old_term == "TNGS S" & MONTH %in% c(9) ~ "yes",
    finescale_fishery_old_term == "TSGS S" & MONTH %in% c(9) & YEAR %notin% c(2016) ~ "yes",
    finescale_fishery_old_term == "TWCVI S" & MONTH %in% c(9) ~ "yes",
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

Sport_creel_finescale_spring<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old), spring_coverage_tf=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old) %>%
  summarise_at(vars(creel_plus), sum, na.rm=TRUE) %>%
  rename(creel_plus_spring=creel_plus)

Sport_creel_finescale_fall<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old), fall_coverage_tf=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old) %>%
  summarise_at(vars(creel_plus), sum, na.rm=TRUE) %>%
  rename(creel_plus_fall=creel_plus)

Sport_mark_rate_finescale_term_sum<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old_term)) %>%
  group_by(YEAR, status, finescale_fishery_old_term, finescale_fishery_term) %>%
  summarise_at(vars(creel:catch_estimate), sum, na.rm=TRUE) %>%
  rename(finescale_fishery = finescale_fishery_term,
         finescale_fishery_old = finescale_fishery_old_term)

Sport_creel_finescale_term_summer<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old_term), summer_coverage_tf_term=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old_term) %>%
  summarise_at(vars(creel_plus), sum, na.rm=TRUE) %>%
  rename(creel_plus_summer=creel_plus) %>%
  rename(finescale_fishery_old = finescale_fishery_old_term)

Sport_creel_finescale_term_fall<- Sport_mark_rate_finescale %>%
  filter(!is.na(finescale_fishery_old_term), fall_coverage_tf_term=="yes") %>%
  group_by(YEAR, status, finescale_fishery_old_term) %>%
  summarise_at(vars(creel_plus), sum, na.rm=TRUE) %>%
  rename(creel_plus_fall=creel_plus) %>%
  rename(finescale_fishery_old = finescale_fishery_old_term)


#Year, finescale fishery
Sport_creel_summer<- bind_rows(Sport_creel_finescale_term_summer, Sport_creel_finescale_summer)
Sport_creel_fall<- bind_rows(Sport_creel_finescale_term_fall, Sport_creel_finescale_fall)
Sport_creel_all_seasons <- left_join(Sport_creel_summer, Sport_creel_fall)
Sport_creel_all_seasons<-left_join(Sport_creel_all_seasons,Sport_creel_finescale_spring)


Sport_mark_rate_finescale_combined<-bind_rows(Sport_mark_rate_finescale_term_sum, Sport_mark_rate_finescale_sum)

Sport_mark_rate_finescale_combined<-left_join(Sport_mark_rate_finescale_combined, Sport_creel_all_seasons)


#### Modelling
library(glmmTMB)
library(DHARMa)
library(fitdistrplus)
#library(MASS)
library(car)
library(lme4)
library(bbmle)


#Summer model

Summer_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S SUMMER","JNST S SUMMER",  "NGS S SUMMER", "SGS S SUMMER", "TCA JDF S SUMMER", "TJOHN ST S SUMMER", "TNGS S SUMMER", "TSGS S SUMMER", "TWCVI S SUMMER", "WCVI AABM S SUMMER", "WCVI ISBM S SUMMER"))

Summer_model<- glmmTMB(formula = catch_estimate ~ creel_plus_summer*status*finescale_fishery,
                       family = gaussian,
                       data = Summer_south)
summary(Summer_model)

Summer_model2<- glmmTMB(formula = catch_estimate ~ creel_plus_summer*finescale_fishery,
                       family = gaussian,
                       data = Summer_south)
summary(Summer_model2)

Summer_model3<- glmmTMB(formula = catch_estimate ~ creel_plus_summer*status,
                        family = gaussian,
                        data = Summer_south)
summary(Summer_model3)

Summer_model4<- glmmTMB(formula = catch_estimate ~ creel_plus_summer,
                        family = gaussian,
                        data = Summer_south)
summary(Summer_model4)

AIC(Summer_model2, Summer_model, Summer_model3, Summer_model4)

## plots
ggplot(Summer_south, aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

ggplot(Summer_south, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

ggplot(Summer_south, aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm") + facet_wrap(~status, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()


#Adding predicted data
Summer_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S SUMMER","JNST S SUMMER",  "NGS S SUMMER", "SGS S SUMMER", "TCA JDF S SUMMER", "TJOHN ST S SUMMER", "TNGS S SUMMER", "TSGS S SUMMER", "TWCVI S SUMMER", "WCVI AABM S SUMMER", "WCVI ISBM S SUMMER")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, creel_plus_summer)
Summer_south_old_new<-predict(Summer_model, newdata =  Summer_south_old)
Summer_south_old_new_2<-Summer_south_old %>%   mutate(creel_estimate_predicted = Summer_south_old_new)

###### Spring model
#spring model
Spring_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S SPRING","JNST S SPRING",  "NGS S SPRING", "SGS S SPRING",  "WCVI AABM S SPRING"))

Spring_model<- glmmTMB(formula = catch_estimate ~ creel_plus_spring*status*finescale_fishery,
                       family = gaussian,
                       data = Spring_south)
summary(Spring_model)

Spring_model2<- glmmTMB(formula = catch_estimate ~ creel_plus_spring*finescale_fishery,
                        family = gaussian,
                        data = Spring_south)
summary(Spring_model2)

Spring_model3<- glmmTMB(formula = catch_estimate ~ creel_plus_spring,
                        family = gaussian,
                        data = Spring_south)
summary(Spring_model3)

Spring_model4<- glmmTMB(formula = catch_estimate ~ creel_plus_summer*finescale_fisher,
                        family = gaussian,
                        data = Spring_south)
summary(Spring_model4)


AIC(Spring_model2, Spring_model, Spring_model3)

#spring plots

ggplot(Spring_south, aes(x=creel_plus_spring, y= catch_estimate, col=status, fill=status))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "gaussian")) + facet_wrap(~finescale_fishery+status, scales="free") + ggtitle("Spring") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

ggplot(Spring_south, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "gaussian")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Spring") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()


ggplot(Spring_south, aes(x=creel_plus_spring, y= catch_estimate))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "gaussian")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Spring") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
#keep model 1
Spring_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S SPRING","JNST S SPRING",  "NGS S SPRING", "SGS S SPRING",  "WCVI AABM S SPRING")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, creel_plus_spring)
Spring_south_old_new<-predict(Spring_model, newdata =  Spring_south_old)
Spring_south_old_new_2<-Spring_south_old %>%   mutate(creel_estimate_predicted = Spring_south_old_new)


###### Fall model
#fall model
fall_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S FALL","JNST S FALL",  "NGS S FALL", "SGS S FALL",  "WCVI AABM S FALL", "TJOHN ST S FALL", "TNGS S FALL", "TSGS S FALL"))

fall_model<- glmmTMB(formula = catch_estimate ~ creel_plus_fall*status*finescale_fishery,
                       family = gaussian,
                       data = fall_south)
summary(fall_model)

fall_model2<- glmmTMB(formula = catch_estimate ~ creel_plus_fall*finescale_fishery,
                        family = gaussian,
                        data = fall_south)
summary(fall_model2)

fall_model3<- glmmTMB(formula = catch_estimate ~ creel_plus_fall*status,
                        family = gaussian,
                        data = fall_south)
summary(fall_model3)

fall_model4<- glmmTMB(formula = catch_estimate ~ creel_plus_fall,
                        family = gaussian,
                        data = fall_south)
summary(fall_model4)


AIC(fall_model2, fall_model)

#fall plots

ggplot(fall_south, aes(x=creel_plus_fall, y= catch_estimate, col=status, fill=status))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "gaussian")) + facet_wrap(~finescale_fishery+status, scales="free") + ggtitle("fall") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()
ggplot(fall_south, aes(x=creel_plus_fall, y= catch_estimate))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "gaussian")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("fall") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
#keep model 1
fall_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S fall","JNST S fall",  "NGS S fall", "SGS S fall",  "WCVI AABM S fall")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, creel_plus_fall)
fall_south_old_new<-predict(fall_model, newdata =  fall_south_old)
fall_south_old_new_2<-fall_south_old %>%   mutate(creel_estimate_predicted = fall_south_old_new)




#### MRR URR



Sport_mark_rate_mrr<-Sport_mark_rate_finescale_combined %>%
                     pivot_wider(id_cols = c(YEAR, MONTH, finescale_fishery_old), names_from=status, values_from = catch_estimate) %>%
                      mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
                             ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total)) %>%
                      mutate(unmarked_release=1-ukr)


Sport_mark_rate_mrr_creel<-Sport_mark_rate_finescale_combined %>%
  pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = creel_plus) %>%
  mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
         ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total)) %>%
  mutate(mrrplusukr=mrr + ukr)

ggplot() +
  geom_point(data=Sport_mark_rate_mrr %>% filter(finescale_fishery_old == "CA JDF S"), aes(y=ukr, x=as.factor(MONTH), col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=ukr, x=YEAR, col="lightblue"))+
  geom_point(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4"))+
 # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen"))+
 # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen"))+
  scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen", "darkgreen"),  labels = c("UKR", "MRR", "UKR", "MRR"))+
  theme(legend.position="bottom")+
  ylab("Proportion")+
  facet_wrap(~YEAR)+
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

