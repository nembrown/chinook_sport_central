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

