

# UPDATE FINESCALE FISHERIES TO INCLUDE MKR MRR ONES
# Noel Swain, DFO
# June 12, 2024
# scope: ----
# 1) split up finescale sport fisheries in CAMP2024MSF by infilling info
# from original and adding date and new fishery id using functions from cetl.
#
# 2) add in new finescale sport fisheries to the worksheet "cmfMSFRelesaseRates" in the workbook
# "finescale_msf_template_split_fisheries" on sharepoint.

# libraries: ----
#remotes::install_git('https://gitlab.com/chinook-technical-committee/programs/r-packages/cetl')
library(cetl)
library(DBI)
options(warn = 2)
library(dplyr)

# 1) updating CAMP2024MSF:----
# camp config files and camp connection:
camp_config_filename <- r"(camp_2024msf.config)"
camp_conn <- ctcUtil::open_camp_connection(camp_config_filename)

# TO DELETE ANY RECORDS: ----
# cetl::delete_fishery(camp_conn, 32053)

#Spring = Months 1-4, Summer = Months 5-9, Fall = Months 10-12
# 30 days: April, June, September
# 31 days: January, March, May, July, August, October, and December

# CA JDF S:----
# FALL:
cetl::duplicate_fishery(camp_conn,
                        3220,
                        32201,
                        name = "CA JDF S FALL",
                        description = "CANADIAN JUAN DE FUCA SPORT - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3220,
                        32202,
                        name = "CA JDF S SPRING",
                        description = "CANADIAN JUAN DE FUCA SPORT - SPRING",
                        reg_type_id = 3,
                        #default_ukr = .8,
                        #default_mrr = .2,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month
# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3220,
                        32203,
                        name = "CA JDF S SUMMER",
                        description = "CANADIAN JUAN DE FUCA SPORT - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month

# JOHN ST S: ----
# FALL:
cetl::duplicate_fishery(camp_conn,
                        3208,
                        32081,
                        name = "JOHN ST S FALL",
                        description = "JOHNSTONE STRAIT SPORT - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3208,
                        32082,
                        name = "JOHN ST S SPRING",
                        description = "JOHNSTONE STRAIT SPORT - SPRING",
                        reg_type_id = 3,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month

# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3208,
                        32083,
                        name = "JOHN ST S SUMMER",
                        description = "JOHNSTONE STRAIT SPORT - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month

# NGS S: ----
# FALL:
cetl::duplicate_fishery(camp_conn,
                        3209,
                        32091,
                        name = "NGS S FALL",
                        description = "NORTH GEORGIA STRAIT SPORT - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3209,
                        32092,
                        name = "NGS S SPRING",
                        description = "NORTH GEORGIA STRAIT SPORT - SPRING",
                        reg_type_id = 3,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month
# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3209,
                        32093,
                        name = "NGS S SUMMER",
                        description = "NORTH GEORGIA STRAIT SPORT - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month
#\\\\\\\\\\\\
# NOTE: for WCVI north/south AABM and ISBM, need to manually update dates based on original fishery temporal ranges:
# •	Split 3215 into the three seasons while maintaining their original date ranges (e.g., if a date range spans only one season, say Oct. 16 – Dec. 1, it’s fall, but if it spans multiple, say Jan 1 – May 10, then it would be Spring: Jan 1 – April 30 and Summer: May 1- May 10.
# •	Split into north and south: regions 21, 23, 24, M401 are South and 25-27 are North.
# •	Any regions without specified date ranges get split into the three seasons with the same date ranges as applied to other fisheries.
# •	Delete any redundant rows
# •	Make 3216 region 28 south and 27 north and south and should get partitioned out as per usual by three seasons
# •	Copy over all regions with specified date ranges from 3215 to 3216 and apply the date ranges missing from 3215 (e.g., Fall = Oct. 16 – Dec. 1 in 3215 then fall = Oct 1-15 in 3216).

# #***NOTE***# COMMENTED OUT WCVI SO THAT MANUAL UPDATES TO CAMP ARE NOT OVERWRITTEN - IF RUN, YOU WILL NEED TO FOLLOW STEPS ABOVE TO PARTITION OUT REGIONS/TIME PERIODS!
# # WCVI AABM S:----
# # NORTH:
# # FALL:
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32151,
#                         name = "NWCVI AABM S FALL",
#                         description = "NORTH WCVI AABM SPORT - FALL")
# # SPRING:
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32152,
#                         name = "NWCVI AABM S SPRING",
#                         description = "NORTH WCVI AABM SPORT - SPRING")
# # SUMMER:
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32153,
#                         name = "NWCVI AABM S SUMMER",
#                         description = "NORTH WCVI AABM SPORT - SUMMER")
# # SOUTH
# # FALL
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32154,
#                         name = "SWCVI AABM S FALL",
#                         description = "SOUTH WCVI AABM SPORT - FALL")
# # SPRING:
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32155,
#                         name = "SWCVI AABM S SPRING",
#                         description = "SOUTH WCVI AABM SPORT - SPRING")
# # SUMMER:
# cetl::duplicate_fishery(camp_conn,
#                         3215,
#                         32156,
#                         name = "SWCVI AABM S SUMMER",
#                         description = "SOUTH WCVI AABM SPORT - SUMMER")
#
# # WCVI ISBM Sport: ----
# # just call all ISBM summer, but need to extend to include Oct.
# # NORTH:
# # SUMMER:
# cetl::duplicate_fishery(camp_conn,
#                         32153,
#                         32161,
#                         name = "NWCVI ISBM S SUMMER",
#                         description = "NORTH WCVI ISBM SPORT - SUMMER")
# # SOUTH
# # SUMMER:
# cetl::duplicate_fishery(camp_conn,
#                         32156,
#                         32162,
#                         name = "SWCVI ISBM S SUMMER",
#                         description = "SOUTH WCVI ISBM SPORT - SUMMER")
#/////////////
# SGS S:----
# FALL
cetl::duplicate_fishery(camp_conn,
                        3212,
                        32121,
                        name = "SGS S FALL",
                        description = "SOUTH GEORGIA STRAIT SPORT - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3212,
                        32122,
                        name = "SGS S SPRING",
                        description = "SOUTH GEORGIA STRAIT SPORT - SPRING",
                        reg_type_id = 3,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month
# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3212,
                        32123,
                        name = "SGS S SUMMER",
                        description = "SOUTH GEORGIA STRAIT SPORT - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month

# # CENTRAL S: ----
# # NOT SPLITTING FOR NOW (so commenting out)!
# # FALL
# cetl::duplicate_fishery(camp_conn,
#                         3205,
#                         32051,
#                         name = "CENTRAL S FALL",
#                         description = "CENTRAL SPORT - FALL",
#                         reg_type_id = 3,
#                         start_month = 10,
#                         start_day = 1,
#                         end_month = 12,
#                         end_day = 31)
# # SPRING:
# cetl::duplicate_fishery(camp_conn,
#                         3205,
#                         32052,
#                         name = "CENTRAL S SPRING",
#                         description = "CENTRAL SPORT - SPRING",
#                         reg_type_id = 3,
#                         start_month = 1,
#                         start_day = 1,
#                         end_month = 4,
#                         end_day = 30) # 30 day month
# # SUMMER:
# cetl::duplicate_fishery(camp_conn,
#                         3205,
#                         32053,
#                         name = "CENTRAL S SUMMER",
#                         description = "CENTRAL SPORT - SUMMER",
#                         reg_type_id = 3,
#                         start_month = 5,
#                         start_day = 1,
#                         end_month = 9,
#                         end_day = 30) # 30 day month

# NORTH (NBC) AABM S: ----
# FALL
cetl::duplicate_fishery(camp_conn,
                        3201,
                        32011,
                        name = "NORTH AABM S FALL",
                        description = "NORTH AABM SPORT (QCI) - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3201,
                        32012,
                        name = "NORTH AABM S SPRING",
                        description = "NORTH AABM SPORT (QCI) - SPRING",
                        reg_type_id = 3,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month
# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3201,
                        32013,
                        name = "NORTH AABM S SUMMER",
                        description = "NORTH AABM SPORT (QCI) - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month

# NORTH ISBM S: ----
# FALL
cetl::duplicate_fishery(camp_conn,
                        3202,
                        32021,
                        name = "NORTH ISBM S FALL",
                        description = "NORTH ISBM SPORT - FALL",
                        reg_type_id = 3,
                        start_month = 10,
                        start_day = 1,
                        end_month = 12,
                        end_day = 31)
# SPRING:
cetl::duplicate_fishery(camp_conn,
                        3202,
                        32022,
                        name = "NORTH ISBM S SPRING",
                        description = "NORTH ISBM SPORT - SPRING",
                        reg_type_id = 3,
                        start_month = 1,
                        start_day = 1,
                        end_month = 4,
                        end_day = 30) # 30 day month
# SUMMER:
cetl::duplicate_fishery(camp_conn,
                        3202,
                        32023,
                        name = "NORTH ISBM S SUMMER",
                        description = "NORTH ISBM SPORT - SUMMER",
                        reg_type_id = 3,
                        start_month = 5,
                        start_day = 1,
                        end_month = 9,
                        end_day = 30) # 30 day month

# DELETE OLD FISHERIES...
cetl::delete_fishery(camp_conn, 3201)
cetl::delete_fishery(camp_conn, 3202)
cetl::delete_fishery(camp_conn, 3208)
cetl::delete_fishery(camp_conn, 3209)
cetl::delete_fishery(camp_conn, 3212)
cetl::delete_fishery(camp_conn, 3215)
cetl::delete_fishery(camp_conn, 3216)
cetl::delete_fishery(camp_conn, 3220)
dbDisconnect(camp_conn)


