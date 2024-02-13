list.of.packages <- c("odbc","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://mirror.rcg.sfu.ca/mirror/CRAN/")
lapply(list.of.packages, require, character.only = TRUE)

source("PullAllSport.R")
# PullAllSport(Start_year) set the calendar year to start querying from to the current year 
rslt <- PullAllSport(2023)
quality_report <- rslt[[1]]
estimates <- rslt[[2]]
rm(rslt)
View(quality_report)
View(estimates)

