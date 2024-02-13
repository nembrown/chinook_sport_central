irec <- read.csv(here::here("Sport_data_set_iREC Estimates.csv")) %>% as_tibble()
irec<- irec %>% rename(TYPE= DISPOSITION,MARKS_DESC = ADIPOSE_MODIFIER, AREA2=AREA, VAL=ESTIMATE, REGION2=LOGISTICAL_AREA) %>%
                mutate(AREA = parse_number(AREA2), SOURCE = "irec_calibrated") %>%
                filter(AREA %notin% c(1:10, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 142, 130, NA)) %>%
                filter(AREA2 %notin% c("Area 29 (In River)")) %>%
                mutate(MARKS_DESC = case_when(
                  MARKS_DESC == "Not Checked" ~ "unchecked",
                  MARKS_DESC == "Not Applicable" ~ "unchecked",
                  MARKS_DESC == "Not Adipose Marked" ~ "unmarked",
                  MARKS_DESC == "Adipose Marked" ~ "marked")) %>%
                mutate(REGION2 = case_when(
                  AREA2 == "Area 19 (JDF)" | AREA %in%c(20) ~ "JDF",
                  AREA2 == "Area 19 (GS)" | AREA%in% c(17,18,28,29) ~ "GSS",
                  AREA%in% c(13,14,15,16) ~ "GSN",
                  AREA2 == "Area 23 (Barkley)" ~ "WCVIS",
                  AREA %in%c(121, 21, 123, 24, 124) ~ "WCVIS",
                  AREA2 == "Area 23 (Alberni Canal)" ~ "ALB",
                  AREA %in%c(125,126,25,26) ~ "WCVIN",
                  AREA %in%c(27,127) ~ "QUAT",
                  AREA %in%c(11,12,111) ~ "JST",
                  TRUE ~ as.character(REGION2))) %>%
                mutate(MANAGEMENT = case_when(
                  AREA %in%c(125,126,127) & MONTH%in%c(1:6,9:12) ~ "AABM",
                  AREA %in%c(121,123,124) & MONTH%in%c(1:7,9:12) ~ "AABM",
                  AREA%in%c(125,126,127) & MONTH%in%c(7,8) ~ "ISBM",
                  AREA%in%c(121,123,124) & MONTH%in%c(8,9) ~ "ISBM",
                  TRUE ~ "ISBM")) %>%
                  group_by(YEAR, MONTH, AREA, REGION2, MANAGEMENT, SOURCE, MARKS_DESC, TYPE) %>%
                  summarise(VAL=sum(VAL)) %>% ungroup()

View(irec)


unique(irec$AREA2)
