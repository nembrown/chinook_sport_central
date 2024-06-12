
### CNR data
source("wrangle_data.R")
source("final_model.R")


#For now, until we get the data from NBC and CBC just use: Season_south_combined which has the modelled data in it
# marked kept and released, roll up to ERA fishery level.
#catch_estimate_predicted

View(Season_south_combined)

#roll up to ERA fishery level for CNR file
#does not include NBC AABM S, NBC ISBM S or CBC S
CNR_roll_up_south <- Season_south_combined %>%
  mutate(finescale_fishery_old = str_replace_all(finescale_fishery_old,
                                                 c("NGS" = "GS",
                                                   "SGS" = "GS",
                                                   "NWCVI S AABM" = "WCVI S AABM",
                                                   "SWCVI S AABM" = "WCVI S AABM",
                                                   "NWCVI S ISBM" = "WCVI S ISBM",
                                                   "SWCVI S ISBM" = "WCVI S ISBM"))) %>%
  group_by(YEAR, finescale_fishery_old, kept_status) %>%
  summarize(total = sum(catch_estimate_predicted))
