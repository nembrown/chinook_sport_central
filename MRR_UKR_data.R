#### MRR URR
source("wrangle_data.R")
source("final_model.R")


Sport_mark_rate_finescale<-read_rds("Sport_mark_rate_finescale.RDS")

Sport_mark_rate_finescale_combined<-read_rds("Sport_mark_rate_finescale_combined.RDS")

models_combined<- read_rds("models_combined.RDS")


#For now, until we get the data from NBC and CBC just use: Season_south_combined which has the modelled data in it

### Restrict the mrr and ukr to be the same before 2013, since we don't have the data to to confidently allow these to diverge.
#Also there were no MSFs in this time period except for in JDF.
Sport_mark_rate_mrr<-models_combined %>%
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

Sport_mark_rate_mrr$mrr_corrected[is.na(Sport_mark_rate_mrr$mrr_corrected)]<-0
Sport_mark_rate_mrr$ukr_corrected[is.na(Sport_mark_rate_mrr$ukr_corrected)]<-1


Sport_mark_rate_mrr_corrected<-Sport_mark_rate_mrr %>% dplyr::select(-mrr, -ukr, -unmarked_release, -unmarked_release_corrected) %>%
  rename(mrr=mrr_corrected, ukr=ukr_corrected)


write.csv(Sport_mark_rate_mrr_corrected, "Sport_mark_rate_mrr.csv")

# 2) ADD IN MKR AND UKR RESULTS TO cmfMSFReleaseRates: ----
# read in model output csv from Norah
# adjust FineFishery to match format of that in cmfMSFReleaseRates
# load relevant data from cmfMSFReleaseRates
# get distinct data for new fisheries
# combine with model output and adjust values to match above application to CAMP
# export as csv and paste to bottom of the sheet "cmfMSFReleaseRates" in https://psconline.sharepoint.com/:x:/r/sites/CTC_365/_layouts/15/Doc.aspx?sourcedoc=%7b7A1A3344-6105-46E7-AC6A-6428E37C6211%7d&file=finescale_msf_template_split_fisheries.xlsx&action=default&mobileredirect=true&xsdata=MDV8MDJ8bm9lbC5zd2FpbkBkZm8tbXBvLmdjLmNhfGRhNTk5M2M5ZDMzOTRkNjU4NzEzMDhkYzgwMTliODgxfDE1OTRmZGFlYTFkOTQ0MDU5MTVkMDExNDY3MjM0MzM4fDB8MHw2Mzg1MjYwOTUxNzA3MDcwNDB8VW5rbm93bnxUV0ZwYkdac2IzZDhleUpXSWpvaU1DNHdMakF3TURBaUxDSlFJam9pVjJsdU16SWlMQ0pCVGlJNklrMWhhV3dpTENKWFZDSTZNbjA9fDB8fHw%3d&sdata=Z1I4Ym5VV2Y4ZnhCL1R2NTU2SWFHVi9zSjlWOHBiU29HUWVDak5RUFE3WT0%3d
library(dplyr)
# load csv of output from Norah's Modelling:
mkrukr <- read.csv('Sport_mark_rate_mrr.csv', as.is=TRUE) %>%
  mutate(FineFishery = gsub(" FALL","",
                            gsub(" SPRING", "",
                                 gsub(" SUMMER", "",
                                      gsub("NWCVI", "WCVI",
                                           gsub("SWCVI", "WCVI",
                                                gsub("JNST S", "JOHN ST S", finescale_fishery)))))),
         FineFishery = gsub('S AABM', 'AABM S',
                            gsub('S ISBM', 'ISBM Sport',
                                 gsub('NBC', 'NORTH',
                                      gsub('CBC', 'CENTRAL', FineFishery)))))

# load csv of all non-terminal finescale Canadian sport fisheries from "cmfMSFReleaseRates":
cmfMSFReleaseRates<-read.csv('cmfMSFReleaseRates.csv', as.is = TRUE) %>%
  filter(Agency == 'CDFO',
         Terminal == 'FALSE',
         grepl('SPORT|Sport', FineFishery_Description))

mkrukr2 <- mkrukr %>%
  left_join(cmfMSFReleaseRates %>%
              distinct(Agency,ERAFishery_ID, ERAFishery,
                       FineFishery_ID_old, FineFisheryID_new,
                       FineFishery, FineFishery_Description,
                       Terminal, MSF_Regulations_Category,
                       legal_CNR_present)) %>%
  mutate(FineFishery = ifelse(grepl('SWCVI', finescale_fishery), paste0('S', FineFishery),
                              ifelse(grepl('NWCVI', finescale_fishery), paste0('N', FineFishery), FineFishery)),
         FineFishery = ifelse(grepl('FALL', finescale_fishery), paste0(FineFishery, ' FALL'),
                              ifelse(grepl('SPRING', finescale_fishery), paste0(FineFishery, ' SPRING'),
                                     ifelse(grepl('SUMMER', finescale_fishery), paste0(FineFishery, ' SUMMER'), FineFishery))),
         FineFishery_Description = ifelse(grepl('SWCVI', finescale_fishery), paste0('S', FineFishery_Description),
                                          ifelse(grepl('NWCVI', finescale_fishery), paste0('N', FineFishery_Description), FineFishery_Description)),
         FineFishery_Description = ifelse(grepl('FALL', FineFishery), paste0(FineFishery_Description, ' - FALL'),
                                          ifelse(grepl('SPRING', FineFishery), paste0(FineFishery_Description, ' - SPRING'),
                                                 ifelse(grepl('SUMMER', FineFishery), paste0(FineFishery_Description, ' - SUMMER'), FineFishery_Description))),
         FineFisheryID_new = ifelse(grepl('FALL', FineFishery), as.numeric(paste0(FineFishery_ID_old, 1)),
                                    ifelse(grepl('SPRING', FineFishery), as.numeric(paste0(FineFishery_ID_old, 2)),
                                           ifelse(grepl('SUMMER', FineFishery), as.numeric(paste0(FineFishery_ID_old, 3)), FineFishery_ID_old))),
         FineFisheryID_new = ifelse(FineFishery == 'SWCVI AABM S FALL', paste0(FineFishery_ID_old, 4),
                                    ifelse(FineFishery == 'SWCVI AABM S SPRING', paste0(FineFishery_ID_old, 5),
                                           ifelse(FineFishery == 'SWCVI AABM S SUMMER', paste0(FineFishery_ID_old, 6), FineFisheryID_new)))
         # if wanting to add sources in here:
         # catch_mrr_source = "",
         # catch_ukr_source = ""
  ) %>%
  dplyr::select(Agency, Year = YEAR, ERAFishery_ID, ERAFishery, FineFishery_ID_old,
         FineFisheryID_new, FineFishery, FineFishery_Description, Terminal,
         MSF_Regulations_Category, legal_CNR_present,
         Marked_Kept = marked_Kept_total,
         Marked_Released = marked_Released_total,
         catch_mrr = mrr,
         Unmarked_Kept  = unmarked_Kept_total,
         Unmarked_Released = unmarked_Released_total,
         catch_ukr = ukr)

write.csv(mkrukr2, 'cmfMSFReleaseRates_ModelResults.csv', row.names = FALSE)
