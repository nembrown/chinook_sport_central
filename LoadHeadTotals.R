library(dplyr)

LoadHeadTotals <- function() {

  head_df <-
    read.csv("./data/chinook_heads.csv") |>
    as_tibble() |>
    rename_with(tolower) |>
    filter(!is.na(month))

  region_def_df <-
    readxl::read_excel("./data/regions.xlsx") |>
    mutate(start_month = as.integer(start_month),
           end_month = as.integer(end_month))

  head_total_df <-
    head_df |>
    left_join(region_def_df, by = join_by(area == mrp_area, month >= start_month, month <= end_month)) |>
    group_by(year, region) |>
    summarize(head_total = sum(head_total, na.rm=TRUE), .groups="drop")

  return(head_total_df)
}


head_df <-
  LoadHeadTotals() |>
  filter(!is.na(region))


head_catch_df <-
  Season_south_combined |>
  filter(status == "marked_Kept_total") |>
  full_join(head_df, c(YEAR="year", finescale_fishery = "region")) |>
  mutate(YEAR = as.integer(YEAR),
         submit_rate = head_total/catch_estimate_predicted)


head_catch_model_df <-
  head_catch_df |>
  filter(catch_estimate_predicted > 0, !(grepl("NBC", head_catch_df$finescale_fishery) & YEAR < 2015))

head_model <- lm(head_total ~ catch_estimate_predicted + 0, data=head_catch_model_df)


lm_eqn <- function(df){
  m <- lm(head_total ~ catch_estimate_predicted + 0, df)

  eq <- substitute(italic(y) == a %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data = head_catch_model_df, aes(x=catch_estimate_predicted,
                                       y=head_total)) +
  geom_point(aes(color = finescale_fishery),
             size=2) +
  geom_smooth(method='lm', formula= y~x) +
  geom_text(x = 500, y = 5000, label = lm_eqn(head_catch_model_df), parse = TRUE, hjust=0, vjust=0)

head_catch_model_df |>
  filter(catch_estimate_predicted > 20) |>
  ggplot(aes(x=YEAR, y=submit_rate, color=finescale_fishery)) +
  geom_line(size = 3)

region_head_df <-
  head_catch_model_df |>
  mutate(region_name = substring(finescale_fishery,1,6)) |>
  mutate(region_name = paste0(region_name,
                              if_else(grepl("ISBM", finescale_fishery), "ISBM", ""))) |>
  mutate(region_name = trimws(region_name)) |>
  group_by(YEAR, region_name) |>
  summarize(head_total = sum(head_total, na.rm=TRUE),
            catch_estimate_predicted = sum(catch_estimate_predicted, na.rm=TRUE),
            .groups = "drop") |>
  mutate(submit_rate = head_total/catch_estimate_predicted)


ggplot(region_head_df, aes(x=catch_estimate_predicted, y=head_total)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_text(x = 500, y = 5000, label = lm_eqn(region_head_df), parse = TRUE, hjust=0, vjust=0)


ggplot(region_head_df, aes(x=YEAR, y=submit_rate, color=region_name)) +
  geom_line(size = 3)
