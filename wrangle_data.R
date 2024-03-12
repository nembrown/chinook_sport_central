

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
  summarise(VARIANCE=sum(VARIANCE), VAL=sum(ESTIMATE)) |> ungroup()|>
  mutate(season = case_when(
    MONTH %in% c(1:5) ~ "spring",
    MONTH %in% c(6:8) ~ "summer",
    MONTH %in% c(9:12) ~ "fall"
  ))



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

allobs2 <- tidyr::expand(Sport_filtered_south_irec, nesting(AREA, REGION2, MANAGEMENT), YEAR, nesting(MONTH, season), MARKS_DESC, TYPE, SOURCE)

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
         historic_plus = sum(historic,lodge_log, na.rm=TRUE)) %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT) %>%
  mutate(catch_estimate_cat = case_when(
    YEAR > 2012 & MONTH %in% c(5:9) & (is.na(creel) | creel==0) ~ "use irec",
    YEAR > 2012 & MONTH %in% c(1:4,10:12) ~ "use irec",
    YEAR < 2013 & (is.na(creel_plus) | creel_plus==0) ~ "use historic",
    TRUE ~ "use creel plus")) %>%
  ungroup() %>% dplyr::select(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, catch_estimate_cat)



Sport_mark_rate3<-Sport_mark_rate %>%
  dplyr::select(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, SOURCE, marked_Kept_total, unmarked_Kept_total, marked_Released_total, unmarked_Released_total) %>%
  pivot_longer(cols=c(contains("total")), names_to = "status", values_to = "value") %>%
  pivot_wider(id_cols = c(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, status), names_from = SOURCE, values_from = value) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rowwise() %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT, status) %>%
  mutate(creel_plus = sum(creel,lodge_log, na.rm=TRUE),
         historic_plus = sum(historic,lodge_log, na.rm=TRUE)) %>%
  group_by(YEAR, MONTH, season, AREA, REGION2, MANAGEMENT) %>%
  left_join(Sport_mark_rate2) %>%
  mutate(catch_estimate = case_when(
    catch_estimate_cat == "use irec" ~ as.numeric(irec_calibrated),
    catch_estimate_cat == "use historic" ~  as.numeric(historic_plus),
    catch_estimate_cat == "use creel plus" ~ as.numeric(creel_plus))) %>%
  ungroup() %>%
  relocate(catch_estimate_cat, .after=status)


#presumably this is now the correct data at the PFMA level ...

# Now we need to sum up to the appropriate finescale fishery level, and do a calculation of how much more irec gets us.
#Need to split the finescale fisheries now
Sport_mark_rate_finescale<-
  Sport_mark_rate3%>% ungroup %>%
  mutate(finescale_fishery = case_when(
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="fall" ~ "WCVI AABM S FALL",
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="spring" ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="AABM" & season=="summer" ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & season=="fall" ~ "WCVI AABM S FALL",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & season=="spring" ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(6:7) ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10:12) ~ "WCVI AABM S FALL",
    AREA%in%c("Area 25", "Area 26", "Area 27") & season=="spring" ~ "WCVI AABM S SPRING",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(6) ~ "WCVI AABM S SUMMER",

    AREA%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") & MANAGEMENT=="ISBM" & MONTH%in%c(7:9) ~ "WCVI ISBM S SUMMER",
    AREA%in%c("Area 21", "Area 24", "Area 23 (Barkley)", "Area 23 (Alberni Canal)") & MONTH%in%c(8,9) ~ "WCVI ISBM S SUMMER",
    AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "WCVI ISBM S SUMMER",

    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="fall" ~ "NGS S FALL",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="spring" ~ "NGS S SPRING",
    (AREA %in%c("Area 13", "Area 14", "Area 15", "Area 16") |REGION2== "GSN")& season=="summer" ~ "NGS S SUMMER",


    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="fall" ~ "SGS S FALL", #this captures 19
    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="spring" ~ "SGS S SPRING",
    (AREA %in%c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") |REGION2== "GSS")& season=="summer" ~ "SGS S SUMMER",


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


    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="spring"  ~ "CA JDF S SPRING", #this captures 19
    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="fall"  ~ "CA JDF S FALL",
    (AREA %in% c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)") | REGION2 == "JDF") & season=="summer"  ~ "CA JDF S SUMMER")) %>%

  mutate(finescale_fishery_term = case_when(
    AREA%in%c("Area 123", "Area 23 (Barkley)", "Area 23 (Alberni Canal)")  & MONTH%in%c(8:12) ~ "TWCVI S FALL",
    AREA %in%  c("Area 11", "Area 111", "Area 12") & MONTH%in%c(9:12) ~ "TJOHN ST S FALL",
    AREA %in%  c("Area 11", "Area 111", "Area 12") & MONTH%in%c(5:8) ~ "TJOHN ST S SUMMER",
    (AREA%in% c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29")  |  REGION2 == "GSS") & MONTH%in%c(9:12) ~ "TSGS S FALL",
    (AREA%in% c("Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29")  |  REGION2 == "GSS") & MONTH%in%c(5:8) ~ "TSGS S SUMMER",
    (AREA%in% c("Area 13", "Area 14", "Area 15", "Area 16") |  REGION2 == "GSN") & MONTH%in%c(9:12) ~ "TNGS S FALL",
    (AREA%in% c("Area 13", "Area 14", "Area 15", "Area 16") |  REGION2 == "GSN") & MONTH%in%c(5:8) ~ "TNGS S SUMMER",
    (AREA %in%c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")  | REGION2 == "JDF") & MONTH%in%c(5:8) ~ "TCA JDF S SUMMER")) %>%

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
    finescale_fishery_old == "CA JDF S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "JNST S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "NGS S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old == "SGS S" & MONTH %in% c(6:8) & YEAR %notin% c(2016, 2020) ~ "yes",
    finescale_fishery_old == "WCVI ISBM S" & MONTH %in% c(7:8) ~ "yes",
    finescale_fishery_old == "WCVI AABM S" & MONTH %in% c(6:8)& YEAR != 2020 ~ "yes",
    finescale_fishery_old == "CBC S" & MONTH %in% c(6:8) ~ "yes",
    finescale_fishery_old == "NBC AABM S" & MONTH %in% c(6:8) ~ "yes",
    finescale_fishery_old == "NBC ISBM S" & MONTH %in% c(6:8) ~ "yes",
    .default = "no")) %>%
  mutate(summer_coverage_tf_term = case_when(
    finescale_fishery_old_term == "TCA JDF S" & MONTH %in% c(5:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TJOHN ST S" & MONTH %in% c(6:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TNGS S" & MONTH %in% c(5:8) & YEAR != 2020 ~ "yes",
    finescale_fishery_old_term == "TSGS S" & MONTH %in% c(6:8) & YEAR %notin% c(2016, 2020) ~ "yes",
    finescale_fishery_old_term == "TWCVI S" & MONTH %in% c(8:9) ~ "yes",
    TRUE ~ "no")) %>% mutate(spring_coverage_tf = case_when(
      finescale_fishery_old == "CA JDF S" & MONTH %in% c(4) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "JNST S" & MONTH %in% c(6) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "NGS S" & MONTH %in% c(6) & YEAR != 2020 ~ "yes",
      finescale_fishery_old == "SGS S" & MONTH %in% c(4) & YEAR %notin% c(2020) ~ "yes",
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


Sport_mark_rate_finescale_combined<-Sport_mark_rate_finescale_combined %>% mutate(mark_status = case_when(
  status %in% c("marked_Kept_total", "marked_Released_total") ~ "marked",
  TRUE ~ "unmarked"
))%>% mutate(kept_status = case_when(
  status %in% c("marked_Kept_total", "unmarked_Kept_total") ~ "Kept",
  TRUE ~ "Released"
))




#### Modelling
library(glmmTMB)
library(DHARMa)
 library(fitdistrplus)
# #library(MASS)
# library(car)
 library(lme4)
library(bbmle)
library(SuppDists)

#Summer model

#splitting terminal out.
#Year can not be included as random effect since we use it later

#collect summer data, split into terminal and non terminal fisheries bc they are using the same data so can't be in the same model - not independent
Summer_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S SUMMER","JNST S SUMMER",  "NGS S SUMMER", "SGS S SUMMER", "WCVI AABM S SUMMER", "WCVI ISBM S SUMMER"))
Summer_south_terminal<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("TCA JDF S SUMMER", "TJOHN ST S SUMMER", "TNGS S SUMMER", "TSGS S SUMMER", "TWCVI S SUMMER"))

#Random effects - don't have enough levels of each of the predictors to use random effects: see Ben Bolker's discussion: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#should-i-treat-factor-xxx-as-fixed-or-random, need at least 5 or 6 and >10 is best
#therefore use glm=gaussian for normal and glm=other family to testing other families

#Test 1 - Check distributions check if normal, poisson or gamma distribution is best fitted.
#gaussian
Summer_model_full<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Summer_south)
summary(Summer_model_full)
res <- simulateResiduals(Summer_model_full, plot = T, quantreg=T)

#inverse gaussian
Summer_model_full_inv<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=inverse.gaussian(link = "log"), data = Summer_south)
summary(Summer_model_full_inv)
res_inv <- simulateResiduals(Summer_model_full_inv, plot = T, quantreg=T)

#poisson
Summer_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=poisson, data = Summer_south)
res_pois <- simulateResiduals(Summer_model_full_poisson, plot = T, quantreg=T)
summary(Summer_model_full_poisson)

#gamma
Summer_model_full_gamma<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam <- simulateResiduals(Summer_model_full_gamma, plot = T, quantreg=T)
summary(Summer_model_full_gamma)

AICtab(Summer_model_full,Summer_model_full_poisson, Summer_model_full_gamma, Summer_model_full_inv)
#gamma is the best, poisson doesn't converge

#Test 2 - Change the model specification, full model vs. dropped effects
#testing simplified models

Summer_model_gamma_dropkept<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropkept <- simulateResiduals(Summer_model_gamma_dropkept, plot = T, quantreg=T)
summary(Summer_model_gamma_dropkept)

Summer_model_gamma_dropmark<- glm(formula = catch_estimate ~creel_plus_summer*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropmark <- simulateResiduals(Summer_model_gamma_dropmark, plot = T, quantreg=T)
summary(Summer_model_gamma_dropmark)

Summer_model_gamma_dropmarkkept<- glm(formula = catch_estimate ~creel_plus_summer*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropmarkkept <- simulateResiduals(Summer_model_gamma_dropmarkkept, plot = T, quantreg=T)
summary(Summer_model_gamma_dropmarkkept)

Summer_model_gamma_dropfishery<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfishery <- simulateResiduals(Summer_model_gamma_dropfishery, plot = T, quantreg=T)
summary(Summer_model_gamma_dropfishery)

Summer_model_gamma_dropfisherykept<- glm(formula = catch_estimate ~creel_plus_summer*mark_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfisherykept <- simulateResiduals(Summer_model_gamma_dropfisherykept, plot = T, quantreg=T)
summary(Summer_model_gamma_dropfisherykept)

Summer_model_gamma_dropfisherymark<- glm(formula = catch_estimate ~creel_plus_summer*kept_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfisherymark <- simulateResiduals(Summer_model_gamma_dropfisherymark, plot = T, quantreg=T)
summary(Summer_model_gamma_dropfisherymark)

Summer_model_gamma_dropall<- glm(formula = catch_estimate ~creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropall <- simulateResiduals(Summer_model_gamma_dropall, plot = T, quantreg=T)
summary(Summer_model_gamma_dropall)

AICtab(Summer_model_full_gamma, Summer_model_gamma_dropkept, Summer_model_gamma_dropmark, Summer_model_gamma_dropmarkkept, Summer_model_gamma_dropfishery, Summer_model_gamma_dropfisherykept, Summer_model_gamma_dropfisherymark, Summer_model_gamma_dropall)
#full Gamma is best model

## plots
ggplot(Summer_south, aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
Summer_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S SUMMER","JNST S SUMMER",  "NGS S SUMMER", "SGS S SUMMER",  "WCVI AABM S SUMMER", "WCVI ISBM S SUMMER")) %>% ungroup() %>% mutate(pred_cat = "predicted")
Summer_south_old_new<-predict.glm(Summer_model_full_gamma, newdata =  Summer_south_old, type = "response")
Summer_south_old_new_2<-Summer_south_old %>%   mutate(catch_estimate_predicted = Summer_south_old_new)

Summer_south2<-Summer_south %>% mutate(catch_estimate_predicted = catch_estimate, pred_cat= "observed")

Summer_south_combined<- rbind(Summer_south_old_new_2, Summer_south2)

ggplot(Summer_south_combined, aes(x=creel_plus_summer, y= catch_estimate_predicted, col=finescale_fishery, fill=finescale_fishery, shape=pred_cat))+geom_point(size=2)+geom_abline(slope=1)+
  geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~status+finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()


#terminal model start here
Summer_terminal<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("TCA JDF S SUMMER", "TJOHN ST S SUMMER", "TNGS S SUMMER", "TSGS S SUMMER", "TWCVI S SUMMER"))

#Test 1 - Check distributions check if normal, poisson or gamma distribution is best fitted.
#gaussian
Summer_terminal_model_full<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Summer_terminal)
summary(Summer_terminal_model_full)
res <- simulateResiduals(Summer_terminal_model_full, plot = T, quantreg=T)

#inverse gaussian
Summer_terminal_model_full_inv<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=inverse.gaussian(link = "log"), data = Summer_terminal)
summary(Summer_terminal_model_full_inv)
res_inv <- simulateResiduals(Summer_terminal_model_full_inv, plot = T, quantreg=T)

#poisson
Summer_terminal_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=poisson, data = Summer_terminal)
res_pois <- simulateResiduals(Summer_terminal_model_full_poisson, plot = T, quantreg=T)
summary(Summer_terminal_model_full_poisson)

#gamma
Summer_terminal_model_full_gamma<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_terminal)
res_gam <- simulateResiduals(Summer_terminal_model_full_gamma, plot = T, quantreg=T)
summary(Summer_terminal_model_full_gamma)

AICtab(Summer_terminal_model_full,Summer_terminal_model_full_poisson, Summer_terminal_model_full_gamma, Summer_terminal_model_full_inv)
#gamma is the best, poisson doesn't converge

#Test 2 - Change the model specification, full model vs. dropped effect of kept which is not significant
#testing simplified models
Summer_terminal_model_gamma_dropkept<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropkept <- simulateResiduals(Summer_terminal_model_gamma_dropkept, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropkept)

Summer_terminal_model_gamma_dropmark<- glm(formula = catch_estimate ~creel_plus_summer*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropmark <- simulateResiduals(Summer_terminal_model_gamma_dropmark, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropmark)

Summer_terminal_model_gamma_dropmarkkept<- glm(formula = catch_estimate ~creel_plus_summer*finescale_fishery,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropmarkkept <- simulateResiduals(Summer_terminal_model_gamma_dropmarkkept, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropmarkkept)

Summer_terminal_model_gamma_dropfishery<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfishery <- simulateResiduals(Summer_terminal_model_gamma_dropfishery, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropfishery)

Summer_terminal_model_gamma_dropfisherykept<- glm(formula = catch_estimate ~creel_plus_summer*mark_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfisherykept <- simulateResiduals(Summer_terminal_model_gamma_dropfisherykept, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropfisherykept)

Summer_terminal_model_gamma_dropfisherymark<- glm(formula = catch_estimate ~creel_plus_summer*kept_status,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropfisherymark <- simulateResiduals(Summer_terminal_model_gamma_dropfisherymark, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropfisherymark)

Summer_terminal_model_gamma_dropall<- glm(formula = catch_estimate ~creel_plus_summer,  family=Gamma(link = "log"), data = Summer_south)
res_gam_dropall <- simulateResiduals(Summer_terminal_model_gamma_dropall, plot = T, quantreg=T)
summary(Summer_terminal_model_gamma_dropall)

AICtab(Summer_terminal_model_full_gamma, Summer_terminal_model_gamma_dropkept, Summer_terminal_model_gamma_dropmark, Summer_terminal_model_gamma_dropmarkkept, Summer_terminal_model_gamma_dropfishery, Summer_terminal_model_gamma_dropfisherykept, Summer_terminal_model_gamma_dropfisherymark, Summer_terminal_model_gamma_dropall)
#full Gamma is best model


## plots
ggplot(Summer_terminal, aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="glm", family= Gamma(link = "log")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
Summer_terminal_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("TCA JDF S SUMMER", "TJOHN ST S SUMMER", "TNGS S SUMMER", "TSGS S SUMMER", "TWCVI S SUMMER")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, mark_status, kept_status, status, creel_plus_summer)
Summer_terminal_old_new<-predict.glm(summer_terminal_model_full_gamma, newdata =  Summer_terminal_old, type = "response")
Summer_terminal_old_new_2<-Summer_terminal_old %>%   mutate(creel_estimate_predicted = Summer_terminal_old_new)



###### Spring model
#spring model
Spring_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S SPRING","JNST S SPRING",  "NGS S SPRING", "SGS S SPRING",  "WCVI AABM S SPRING"))


#Test 1 - does spring better predict than summer?
Spring_model_full<- glm(formula = catch_estimate ~creel_plus_spring*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Spring_south)
summary(Spring_model_full)
res_spring <- simulateResiduals(Spring_model_full, plot = T, quantreg=T)

Spring_model_full_summer<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Spring_south)
summary(Spring_model_full_summer)
res <- simulateResiduals(Spring_model_full_summer, plot = T, quantreg=T)

#spring vs. summer --> spring improves the model
AICtab(Spring_model_full, Spring_model_full_summer)

#Test 1 - Check distributions check if normal, poisson or gamma distribution is best fitted.
#gaussian
Spring_model_full<- glm(formula = catch_estimate ~creel_plus_spring*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Spring_south)
summary(Spring_model_full)
res <- simulateResiduals(Spring_model_full, plot = T, quantreg=T)

#inverse gaussian doesn't run
# Spring_model_full_inv<- glm(formula = (catch_estimate+1) ~creel_plus_spring*mark_status*kept_status*finescale_fishery,  family=inverse.gaussian(link = "log"), data = Spring_south)
# summary(Spring_model_full_inv)
# res_inv <- simulateResiduals(Spring_model_full_inv, plot = T, quantreg=T)

#poisson
Spring_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_spring*mark_status*kept_status*finescale_fishery,  family=poisson, data = Spring_south)
res_pois <- simulateResiduals(Spring_model_full_poisson, plot = T, quantreg=T)
summary(Spring_model_full_poisson)

#gamma - needed to add a small amount of catch for the zeros
Spring_model_full_gamma<- glm(formula = (catch_estimate+0.1) ~creel_plus_spring*mark_status*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Spring_south)
res_gam <- simulateResiduals(Spring_model_full_gamma, plot = T, quantreg=T)
summary(Spring_model_full_gamma)

AICtab(Spring_model_full,Spring_model_full_poisson, Spring_model_full_gamma)
#gamma is the best, poisson doesn't converge - same as above

#Test 2 - Change the model specification, full model vs. dropped effects
#testing simplified models

Spring_model_gamma_dropkept<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring*mark_status*finescale_fishery,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropkept <- simulateResiduals(Spring_model_gamma_dropkept, plot = T, quantreg=T)
summary(Spring_model_gamma_dropkept)

Spring_model_gamma_dropmark<- glm(formula = (catch_estimate + 0.1)~creel_plus_spring*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropmark <- simulateResiduals(Spring_model_gamma_dropmark, plot = T, quantreg=T)
summary(Spring_model_gamma_dropmark)

Spring_model_gamma_dropmarkkept<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring*finescale_fishery,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropmarkkept <- simulateResiduals(Spring_model_gamma_dropmarkkept, plot = T, quantreg=T)
summary(Spring_model_gamma_dropmarkkept)

Spring_model_gamma_dropfishery<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring*mark_status*kept_status,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropfishery <- simulateResiduals(Spring_model_gamma_dropfishery, plot = T, quantreg=T)
summary(Spring_model_gamma_dropfishery)

Spring_model_gamma_dropfisherykept<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring*mark_status,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropfisherykept <- simulateResiduals(Spring_model_gamma_dropfisherykept, plot = T, quantreg=T)
summary(Spring_model_gamma_dropfisherykept)

Spring_model_gamma_dropfisherymark<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring*kept_status,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropfisherymark <- simulateResiduals(Spring_model_gamma_dropfisherymark, plot = T, quantreg=T)
summary(Spring_model_gamma_dropfisherymark)

Spring_model_gamma_dropall<- glm(formula = (catch_estimate + 0.1) ~creel_plus_spring,  family=Gamma(link = "log"), data = Spring_south)
res_gam_dropall <- simulateResiduals(Spring_model_gamma_dropall, plot = T, quantreg=T)
summary(Spring_model_gamma_dropall)

AICtab(Spring_model_full_gamma, Spring_model_gamma_dropkept, Spring_model_gamma_dropmark, Spring_model_gamma_dropmarkkept, Spring_model_gamma_dropfishery, Spring_model_gamma_dropfisherykept, Spring_model_gamma_dropfisherymark, Spring_model_gamma_dropall)
#best model is dropkept - so only including mark rate and fishery but not kept status.

## plots
ggplot(Spring_south, aes(x=creel_plus_spring+0.1, y= catch_estimate, col=mark_status, fill=mark_status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="glm", family= Gamma(link = "log")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()


#Adding predicted data
Spring_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S SPRING","JNST S SPRING",  "NGS S SPRING", "SGS S SPRING",  "WCVI AABM S SPRING")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, mark_status, kept_status, status, creel_plus_spring)
Spring_south_old_new<-predict.glm(Spring_model_gamma_dropkept, newdata =  Spring_south_old, type = "response")
Spring_south_old_new_2<-Spring_south_old %>%   mutate(creel_estimate_predicted = Spring_south_old_new)



###### Fall model
#fall model
Fall_south<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("CA JDF S FALL","JNST S FALL",  "NGS S FALL", "SGS S FALL",  "WCVI AABM S FALL"))
Fall_south_terminal<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("TCA JDF S FALL", "TJOHN ST S FALL", "TNGS S FALL", "TSGS S FALL", "TWCVI S FALL"))

#Test 1 - does fall  better predict than summer?
Fall_model_full<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_south)
summary(Fall_model_full)
res_Fall <- simulateResiduals(Fall_model_full, plot = T, quantreg=T)

Fall_model_full_summer<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_south)
summary(Fall_model_full_summer)
res <- simulateResiduals(Fall_model_full_summer, plot = T, quantreg=T)

#Fall vs. summer --> Fall improves the model
AICtab(Fall_model_full, Fall_model_full_summer)

#Test 2 - Check distributions check if normal, poisson or gamma distribution is best fitted.
#gaussian
Fall_model_full<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_south)
summary(Fall_model_full)
res <- simulateResiduals(Fall_model_full, plot = T, quantreg=T)

#poisson
Fall_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=poisson, data = Fall_south)
res_pois <- simulateResiduals(Fall_model_full_poisson, plot = T, quantreg=T)
summary(Fall_model_full_poisson)

#gamma
Fall_model_full_gamma<- glm(formula = (catch_estimate+2) ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam <- simulateResiduals(Fall_model_full_gamma, plot = T, quantreg=T)
summary(Fall_model_full_gamma)

AICtab(Fall_model_full,Fall_model_full_poisson, Fall_model_full_gamma)
#gamma is best but need a minimum catch of 2 fish.

#Test 2 - Change the model specification, full model vs. dropped effects
#testing simplified models

Fall_model_gamma_dropkept<- glm(formula = (catch_estimate+2) ~creel_plus_fall*mark_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropkept <- simulateResiduals(Fall_model_gamma_dropkept, plot = T, quantreg=T)
summary(Fall_model_gamma_dropkept)

Fall_model_gamma_dropmark<- glm(formula = (catch_estimate+2) ~creel_plus_fall*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropmark <- simulateResiduals(Fall_model_gamma_dropmark, plot = T, quantreg=T)
summary(Fall_model_gamma_dropmark)

Fall_model_gamma_dropmarkkept<- glm(formula = (catch_estimate+2) ~creel_plus_fall*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropmarkkept <- simulateResiduals(Fall_model_gamma_dropmarkkept, plot = T, quantreg=T)
summary(Fall_model_gamma_dropmarkkept)

Fall_model_gamma_dropfishery<- glm(formula = (catch_estimate+2) ~creel_plus_fall*mark_status*kept_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfishery <- simulateResiduals(Fall_model_gamma_dropfishery, plot = T, quantreg=T)
summary(Fall_model_gamma_dropfishery)

Fall_model_gamma_dropfisherykept<- glm(formula = (catch_estimate+2) ~creel_plus_fall*mark_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfisherykept <- simulateResiduals(Fall_model_gamma_dropfisherykept, plot = T, quantreg=T)
summary(Fall_model_gamma_dropfisherykept)

Fall_model_gamma_dropfisherymark<- glm(formula = (catch_estimate+2)~creel_plus_fall*kept_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfisherymark <- simulateResiduals(Fall_model_gamma_dropfisherymark, plot = T, quantreg=T)
summary(Fall_model_gamma_dropfisherymark)

Fall_model_gamma_dropall<- glm(formula = (catch_estimate+2) ~creel_plus_fall,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropall <- simulateResiduals(Fall_model_gamma_dropall, plot = T, quantreg=T)
summary(Fall_model_gamma_dropall)

AICtab(Fall_model_full_gamma, Fall_model_gamma_dropkept, Fall_model_gamma_dropmark, Fall_model_gamma_dropmarkkept, Fall_model_gamma_dropfishery, Fall_model_gamma_dropfisherykept, Fall_model_gamma_dropfisherymark, Fall_model_gamma_dropall)
#Gamm dropkept is the best.

## plots
ggplot(Fall_south, aes(x=creel_plus_fall, y= catch_estimate, col=mark_status, fill=mark_status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="glm", family= Gamma(link = "log")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Summer") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
Fall_south_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("CA JDF S FALL","JNST S FALL",  "NGS S FALL", "SGS S FALL",  "WCVI AABM S FALL")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, mark_status, kept_status, status, creel_plus_fall)
Fall_south_old_new<-predict.glm(Fall_model_gamma_dropkept, newdata =  Fall_south_old, type = "response")
Fall_south_old_new_2<-Fall_south_old %>%   mutate(creel_estimate_predicted = Fall_south_old_new)


#terminal model start here
Fall_terminal<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery %in% c("TCA JDF S FALL", "TJOHN ST S FALL", "TNGS S FALL", "TSGS S FALL", "TWCVI S FALL"))

#Test 1 - does fall  better predict than summer?
Fall_terminal_model_full<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_terminal)
summary(Fall_terminal_model_full)
res_term_Fall <- simulateResiduals(Fall_terminal_model_full, plot = T, quantreg=T)

Fall_terminal_model_full_summer<- glm(formula = catch_estimate ~creel_plus_summer*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_terminal)
summary(Fall_terminal_model_full_summer)
res <- simulateResiduals(Fall_terminal_model_full_summer, plot = T, quantreg=T)

#Fall vs. summer --> Fall improves the model
AICtab(Fall_terminal_model_full, Fall_terminal_model_full_summer)

#Test 1 - Check distributions check if normal, poisson or gamma distribution is best fitted.
#gaussian
Fall_terminal_model_full<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=gaussian, data = Fall_terminal)
summary(Fall_terminal_model_full)
res <- simulateResiduals(Fall_terminal_model_full, plot = T, quantreg=T)

#poisson
Fall_terminal_model_full_poisson<- glm(formula = catch_estimate ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=poisson, data = Fall_terminal)
res_pois <- simulateResiduals(Fall_terminal_model_full_poisson, plot = T, quantreg=T)
summary(Fall_terminal_model_full_poisson)

#gamma
Fall_terminal_model_full_gamma<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*mark_status*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_terminal)
res_gam <- simulateResiduals(Fall_terminal_model_full_gamma, plot = T, quantreg=T)
summary(Fall_terminal_model_full_gamma)

AICtab(Fall_terminal_model_full,Fall_terminal_model_full_poisson, Fall_terminal_model_full_gamma)
#gamma is the best, poisson doesn't converge

#Test 2 - Change the model specification, full model vs. dropped effect of kept which is not significant
#testing simplified models
Fall_terminal_model_gamma_dropkept<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*mark_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropkept <- simulateResiduals(Fall_terminal_model_gamma_dropkept, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropkept)

Fall_terminal_model_gamma_dropmark<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*kept_status*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropmark <- simulateResiduals(Fall_terminal_model_gamma_dropmark, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropmark)

Fall_terminal_model_gamma_dropmarkkept<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*finescale_fishery,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropmarkkept <- simulateResiduals(Fall_terminal_model_gamma_dropmarkkept, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropmarkkept)

Fall_terminal_model_gamma_dropfishery<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*mark_status*kept_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfishery <- simulateResiduals(Fall_terminal_model_gamma_dropfishery, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropfishery)

Fall_terminal_model_gamma_dropfisherykept<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*mark_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfisherykept <- simulateResiduals(Fall_terminal_model_gamma_dropfisherykept, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropfisherykept)

Fall_terminal_model_gamma_dropfisherymark<- glm(formula = (catch_estimate + 1) ~creel_plus_fall*kept_status,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropfisherymark <- simulateResiduals(Fall_terminal_model_gamma_dropfisherymark, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropfisherymark)

Fall_terminal_model_gamma_dropall<- glm(formula = (catch_estimate + 1) ~creel_plus_fall,  family=Gamma(link = "log"), data = Fall_south)
res_gam_dropall <- simulateResiduals(Fall_terminal_model_gamma_dropall, plot = T, quantreg=T)
summary(Fall_terminal_model_gamma_dropall)

AICtab(Fall_terminal_model_full_gamma, Fall_terminal_model_gamma_dropkept, Fall_terminal_model_gamma_dropmark, Fall_terminal_model_gamma_dropmarkkept, Fall_terminal_model_gamma_dropfishery, Fall_terminal_model_gamma_dropfisherykept, Fall_terminal_model_gamma_dropfisherymark, Fall_terminal_model_gamma_dropall)
#full Gamma is best model


## plots
ggplot(Fall_terminal, aes(x=creel_plus_fall, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="glm", family= Gamma(link = "log")) + facet_wrap(~finescale_fishery, scales="free") + ggtitle("Fall") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()

#Adding predicted data
Fall_terminal_old<- Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2005:2012), finescale_fishery %in% c("TCA JDF S FALL", "TJOHN ST S FALL", "TNGS S FALL", "TSGS S FALL", "TWCVI S FALL")) %>% ungroup() %>%  dplyr::select(YEAR, status, finescale_fishery_old, finescale_fishery, mark_status, kept_status, status, creel_plus_fall)
Fall_terminal_old_new<-predict.glm(Fall_terminal_model_full_gamma, newdata =  Fall_terminal_old, type = "response")
Fall_terminal_old_new_2<-Fall_terminal_old %>%   mutate(creel_estimate_predicted = Fall_terminal_old_new)





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

