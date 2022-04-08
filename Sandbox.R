library(skimr)
library(tidyr)
library(dplyr)
library(RJSONIO)
library(stringr)
library(lubridate)
library(sp)
library(doBy)
library(ona)
library(rgdal)

packages <- c("osfr", "downloader", "jsonlite", "rgdal", "sp", "raster", "leaflet", "DT", "htmlwidgets",
              "caret", "caretEnsemble", "mgcv", "MASS", "randomForest", "xgboost", "nnet",
              "klaR", "dplyr", "doParallel", "dismo", "arm")

# Install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))





source("/Users/iita_rm/Dropbox/IITA/CIALCA/Surveying/R/ona.R")
#https://blog.ona.io/general/2016/04/15/Ona-R-Integration.html

########################################
###Downloading Enumerator Registration##
########################################

Reg_EN<-onaDownload("Register_EN", "iita_nrm", "rmanners", "FE6219jm@@")
Reg_EN<-Reg_EN[Reg_EN$projectCode=="RS",]

Reg_EN<-Reg_EN %>%
  filter(Reg_EN$today >= as.Date("2022-03-01"))####Filtering just for 22B

########################################
###Downloading Household Registration###
########################################
  
###Removing multiple submissions of the same household, only holding onto the most recent submission for a household
Reg_HH_unduplicated<-subset(Reg_HH, Reg_HH$HHID %in% names(which(table(Reg_HH$HHID) == 1)))

####There are a number of cases here where multiple enumerators have registered the same household. Need to check this. 
Reg_HH_duplicates<-subset(Reg_HH, Reg_HH$HHID %in% names(which(table(Reg_HH$HHID) > 1)))
Reg_HH_duplicates<-Reg_HH_duplicates %>% group_by(Reg_HH_duplicates$HHID) %>% slice(which.min(Reg_HH_duplicates$start))

Reg_HH_no_dup<-rbind(Reg_HH_unduplicated, Reg_HH_duplicates[,c(1:59)])

#########Mapping Household locations
Training_loc<-read.csv("/Users/iita_rm/Dropbox/IITA/EiA/Use Cases/Rwanda 2.0/R/Data_Submission_Dashboard/EiA_Rwanda_Dashboard/Data/Training Locations.csv")

w <- leaflet() %>%
  setView(lng = mean(Reg_HH_no_dup$X_geopoint_longitude, na.rm=T), lat = mean(Reg_HH_no_dup$X_geopoint_latitude, na.rm=T), zoom = 8) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(Reg_HH_no_dup$X_geopoint_longitude, Reg_HH_no_dup$X_geopoint_latitude, color="red",clusterOptions = markerClusterOptions())%>% 
  addCircleMarkers(Training_loc$X_geopoint_longitude, Training_loc$X_geopoint_latitude, color = "blue", clusterOptions = markerClusterOptions())
#saveWidget(w, 'RW_GS_sample_locs.html', selfcontained = T) ## save widget
w ## plot widget 

###Should probably filter out those submissions from the training.
#https://stackoverflow.com/questions/44471480/how-can-i-filter-out-coordinates-lat-lon-in-a-data-table
#anti_join(Reg_HH_no_dup$X_geopoint_latitude, Training_loc$X_geopoint_latitude) -- this should work, but I'm not sure why it isn't. The submissions from Kibuye should be deleted, but aren't.



########################################
###Downloading Assign Field Trial Plot##
########################################
Assign_all<-onaDownload("Assign_FDTLPO", "iita_nrm", "rmanners", "FE6219jm@@")
Assign_all<-Assign_all[Assign_all$projectCode=="RS",]
Assign_all<-Assign_all %>%
  filter(Assign_all$today >= as.Date("2022-03-01"))####Filtering just for 22B

Assign_all$expCode

Assign_all<-Assign_all %>% filter(!grepl('L', expCode))
Assign_all<-Assign_all %>% filter(!grepl('Lime|lime', FD_name_new))


########Assign Field
####################

Assign_Field<-Assign_all[,c(1:34)]

Field_ID<-Assign_Field$FDID2

###Quality Control
Assign_Field$No_HH_ID<-NA
Assign_Field$No_HH_ID<-ifelse(is.na(Assign_Field$HHID), 1,0)####If there are no HH_IDs then this is given a 1, else 0. 

Assign_Field$No_Field_Name<-NA
Assign_Field$No_Field_Name<-ifelse(is.na(Assign_Field$FD_name_new), 1,0)

AF_Missing_ID<-Assign_Field[Assign_Field$No_Field_Name==1,]####For flagging those fields where IDs haven't been scanned and no field name has been provided.

Assign_Field<-Assign_Field[Assign_Field$No_Field_Name==0,]

#Assign_Field$Naming_Convention<-NA #Need to think how to do this...

########################################
######Downloading Describe Field ######
########################################
Describe_Field<-onaDownload("Describe_FD", "iita_nrm", "rmanners", "FE6219jm@@")
Describe_Field<-Describe_Field[Describe_Field$projectCode=="RS",]
Describe_Field<-Describe_Field %>%
  filter(Describe_Field$today >= as.Date("2022-03-01"))####Filtering just for 22B



########################################
######Downloading Soil Samples ######
########################################
Collect_Soil_Sam<-onaDownload("Collect_SS", "iita_nrm", "rmanners", "FE6219jm@@")
Collect_Soil_Sam<-Collect_Soil_Sam[Collect_Soil_Sam$projectCode=="RS",]
Collect_Soil_Sam<-Collect_Soil_Sam %>%
  filter(Collect_Soil_Sam$today >= as.Date("2022-03-01"))####Filtering just for 22B




