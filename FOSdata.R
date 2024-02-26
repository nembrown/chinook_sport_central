#Sys.setenv(OCI_LIB64="C:/Oracle/12.2.0_Instant_x64")
#Sys.setenv(OCI_INC="C:/Oracle/12.2.0_Instant_x64/sdk/include")
#Sys.setenv(ORACLE_HOME="C:/Oracle/12.2.0_Instant_x64")


#install.packages("ROracle", type="source")

remotes::install_local("C:/Users/BROWNNEM/Dropbox/PC/Downloads/ROracle_1.3-2.tar.gz", repos = NULL, type = "source")

remotes::install_git("https://gitlab.com/michaelfolkes/FOSer", build_vignettes = TRUE)
require(FOSer)

?FOSer::writeScript()

writeScript("demo_aa")
writeScript("demo_queryFOSreports")
