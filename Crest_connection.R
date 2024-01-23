#Need to be connected to DFO VPN
#Needs fetchSport.bat, all_sport.txt, and makecon.B

list.of.packages <- c("xml2","readr","safer","compiler","DBI","odbc","writexl","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://mirror.rcg.sfu.ca/mirror/CRAN/")
message(.libPaths())
lapply(list.of.packages, require, character.only = TRUE)

lic.years <- array(c(2005:2023))
lic.year.str = paste(lic.years, collapse=",")

loadcmp("makecon.B")
pwd<- paste(readLines("crest_password.txt"))
con <- buildcon(pwd)

s1a = gsub("XXXYEARXXX", lic.year.str, paste(readLines("all_sport.txt"), collapse="\n"))
Q = dbSendQuery(con, s1a)
message("Loading Sport Estimates")
et <- system.time(Sport <- dbFetch(Q))
message("Done loading")
message(et[3])
xlname <- paste0(path.expand('~'),"\\Sport_data_set_", format(now(), "%Y%m%d_%H%M%S_"),".xlsx")
write_xlsx(Sport,xlname)
Sys.sleep(5)
shell.exec(xlname)
Sys.sleep(10)

