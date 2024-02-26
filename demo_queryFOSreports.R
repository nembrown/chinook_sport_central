
# comments ----------------------------------------------------------------
#author: Michael Folkes

#this shows how to import the sql files the represent all of the tyee reports found on FOS, these sql text files come with the package, and more can be added.
#user needs an FOS account
#after importing the sql, one of them (Skeena Tyee Indices for a Year and Species_v2) is modified to select species and date range and it's then run.

#not all the canned sql works. These were exported by Fifi and I haven't taken the time to troubleshoot them.

# packages ----------------------------------------------------------------

require(FOSer)



# setup -------------------------------------------------------------------


#this config file includes at least three variables, each being a path
#this file is user specific, as such it is in the .gitignore and won't be followed by git
settings <- yaml::read_yaml("user.settings.config")

# data:import ----------------------------------------------------------

#source of username and pw for FOS:
userid <- yaml::read_yaml(settings$userid.filepath)
#the userid text file would have variables 'username' and 'password' that are your fos account credentials:
#list(username= "user", password= "mypassword")



#show what fos reports are available to run:
names(sql.list)


# data:run query multipanel catch summary ---------------------------------

data.multipanel.catch <- queryFOS(connect_file_config= settings$config.filepath, username = userid$username, password = userid$password, sql = sql.list$Skeena_Tyee_Multipanel_Catch_Summary_Export_vars)

str(data.multipanel.catch)
data.multipanel.catch$DATE <- as.Date(data.multipanel.catch$DATE, "%d/%b/%Y")
range(data.multipanel.catch$DATE)


sql.args <- list(
  speciescode=list(search="@speciescode@", replace=124),
  startdate=list(search="@startdate@", replace="2023/01/01"),
  enddate=list(search="@enddate@", replace="2999/12/31"),
  season_id=list(search="@season_id@", replace=2621), #2621 is 2021 tyee
  year=list(search="@year@", replace=2022),
  fsub_id=list(search="@fsub_id@", replace=585)
)


data.multipanel.catch <- queryFOS(connect_file_config= settings$config.filepath, username = userid$username, password = userid$password, sql = sql.list$Skeena_Tyee_Multipanel_Catch_Summary_Export_vars, sql.args = sql.args)

str(data.multipanel.catch)
data.multipanel.catch$DATE <- as.Date(data.multipanel.catch$DATE, "%d/%b/%Y")
range(data.multipanel.catch$DATE)




# data:run daily tyee index query --------------------------------------------------------------
cat(sql.list$Skeena_Tyee_Indices_for_a_Year_and_Species)
data.sql.index <- queryFOS(connect_file_config= settings$config.filepath, username = userid$username, password = userid$password, sql = sql.list$Skeena_Tyee_Indices_for_a_Year_and_Species)

str(data.sql.index)
colnames(data.sql.index) <- tolower(colnames(data.sql.index))
data.sql.index$date <- as.Date(data.sql.index$fishing_date)
range(data.sql.index$date)
tail(data.sql.index)


# data:run daily tyee index query, with modified dates --------------------------------------------------------------

#change species to chinook
#change date range to span may 1, 2021 to Oct 31, 2022  (handy as the report cannot output multiple years)

#sql.args is a list that allows for search and replace of some terms in the sql. in this case, we can change species and date range for the daily tyee index report sql:

sql.args <- list(
	speciescode=list(search="@speciescode@", replace=124),
	startdate=list(search="@startdate@", replace="2020/01/01"),
	enddate=list(search="@enddate@", replace="2999/12/31"),
	season_id=list(search="@season_id@", replace=2621), #2621 is 2021 tyee
	year=list(search="@year@", replace=2022),
	fsub_id=list(search="@fsub_id@", replace=585)
)



data.sql.index <- queryFOS(connect_file_config= settings$config.filepath, username = userid$username, password = userid$password, sql = sql.list$Skeena_Tyee_Indices_for_a_Year_and_Species_vars, sql.args = sql.args)
str(data.sql.index)
colnames(data.sql.index) <- tolower(colnames(data.sql.index))
data.sql.index$date <- as.Date(data.sql.index$fishing_date)
range(data.sql.index$date)
tail(data.sql.index)


#if calling the tyee index sql without the sql.args argument, the default is to query all dates (1955-2999)
data.sql.index <- queryFOS(connect_file_config= settings$config.filepath, username = userid$username, password = userid$password, sql = sql.list$Skeena_Tyee_Indices_for_a_Year_and_Species_vars)
colnames(data.sql.index) <- tolower(colnames(data.sql.index))
data.sql.index$date <- as.Date(data.sql.index$fishing_date)
range(data.sql.index$date)






