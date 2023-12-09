# XX extract daily sst.R

# script to extract daily sst for Sept/Mar (1997-2023) cc data sets for northern lats EDA
#
# dkb 22 Nov 2023

# 
# require(ncdf4)      # fyi, if you have trouble connecting to the server, make sure this pkg is up to date.
# require(ncdf)
# 
# library(plyr)
# library(raster); library(rgdal)   
# library(maps); library(mapdata); library(maptools); library(mapproj)
# library(move);library(sp)
# library(ggplot2); library(scales); library(zoo); library(reshape)
# 
# require(lattice);  library(sp)
# 
# library(rasterVis)
# library(rgeos)
# 
# library(rgdal) # Loads SP package by default
# library(classInt)
# library(RColorBrewer)
# library(fields)


## Set dir ----------------------------------------------------------------------
ncpath = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"

# wd <- "~/Dropbox/RESEARCH/PROJECTS/NPAC_Turtles/data/data_tracks_for_analysis"
# path = wd

# # functions source location
# r.fun.dir <- paste('~/Dropbox/RESEARCH/CODES/R_codes/',"functions/",sep="");        

## Source Scripts -----
### briefly change dir to not break other scripts ----
# wd <- getwd()
# setwd('./code/')
source('../code/00_northern_lats_helper_functions.R')

source('../code/01_prep_turtle_data.R')

# # reset wd ----
# setwd('../')

# functions: xtracto local source ............
source('~/Dropbox/RESEARCH/CODES/R_codes/xtractomatic/Xtracto_Local_NPAC.R')   


## specifcy function: FYI, this is a modified function from orig XTRACTO LOCAL
getDateRangeX <- function(inpts, ncIn){
    parsedt <- vector(mode='character', length(ncIn))
    for (j in 1:length(ncIn)){
        nc <- ncIn[j]
        parsename <- unlist(strsplit(nc,'_'))
        parsedt[j] <- substr(parsename[abs(length(parsename))],1,10)
    }
    startD <- format(as.POSIXct(min(inpts$dt), tz='UTC'), '%Y-%m-%d')
    print(paste('Ptsfile start', startD))
    endD <- format(as.POSIXct(max(inpts$dt), tz='UTC'), '%Y-%m-%d')
    print(paste('Ptsfile end', endD))
    startfile <- which(abs(as.Date(parsedt)-as.Date(startD)) == min(abs(as.Date(parsedt)-as.Date(startD))))[1]-1
    endfile <- which(abs(as.Date(endD)-as.Date(parsedt)) == min(abs(as.Date(endD)-as.Date(parsedt))))[1]
    dtIn <- ncIn[startfile:endfile]
}

## PART 1: SET UP CC DATA SETS ----------------------------------------------------------

raw_df <- raw_data_cohort_1_w_names
daily_df <- daily_avg_data_cohort_1

# make historic df consistent with cohort df(s)
hist_df <-  historic_tags %>% wrangleHistDF()

# combine cohort 1 & historic dfs
df_all <- rbind(daily_df, hist_df)

# rename tagpts for consistency
obsdata <- df_all %>%
    filter(lon >=-180 & lon <= 0) %>%
    filter(month == 9 | month == 3 | month == 8) 

sst.lag = FALSE
if(sst.lag){
library(lubridate)
obsdata_sept_lag <- obsdata %>%
    filter(month == 9) %>%
    mutate(orig_date = date,
               date = ymd(as.Date(orig_date)) %m+% months(-1)) 

# rename for consistency:
obsdata <- obsdata_sept_lag

}


## for northern lats 165W to 140W long
obsdata <- obsdata %>%
    filter(lon > -165 & lon < -140)

## PART 2: XTRACTO LOCAL ----------------------------------------------------------

## 3) set up xtracto ...................................................
# ncpath <- '/Volumes/Seagate/npac_ncdf'

list.ncs <- list.files(ncpath, full.names=T)

obsdata$X <- seq(1,nrow(obsdata))
# obsdata$lon <- obsdata$lon-360
obsdata$dt <- as.POSIXct(paste(obsdata$date,'12:00:00'),tz='UTC')

obsdata$dtime<-as.POSIXct(obsdata$date,origin="1970-01-01",tz='UTC')
head(obsdata$dtime)
range(obsdata$dtime)

# check out obsdata lon range
range(obsdata$lon)  # [1] -159.9992  179.9979


## let's do this....


### 1) get Daily SST and SST RMS ----
filenames<-grep("sst_dhw_5km",list.ncs)  
sstfiles<-list.ncs[filenames]
varname<-"CRW_SST";  mnradius<- 0.05; sdradius<- 1; alt<-1
sstfilesIn <- getDateRangeX(inpts=obsdata,ncIn=sstfiles) 
sstmean <- rbindlist(lapply(sstfilesIn, FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius),fill=TRUE)
obsdata$sst_dhw_5km_new_mean<-rep(NA, length(obsdata[,1])); obsdata$sst_dhw_5km_new_sd<-rep(NA, length(obsdata[,1]))
# obsdata$HiSST[sstmean$X]<-sstmean$SST_mean
obsdata$sst_dhw_5km_new_mean[sstmean$X]<-sstmean$CRW_SST_mean
obsdata$sst_dhw_5km_new_sd[sstmean$X]<-sstmean$CRW_SST_sd

head(obsdata); tail(obsdata)
range(obsdata$sst_dhw_5km_new_mean, na.rm=T)

rm(sstmean); rm(sstfilesIn)


## 2) now get CHLA ----
### SEAWIFS
filenames<-grep("erdSW2018chla8day",list.ncs)
# filenames<-grep("erdSW2018chlamday",list.ncs)
chlfiles<-list.ncs[filenames]
varname<-"chlorophyll";  mnradius<- 1; sdradius<- 1; alt<-1
chlfilesIn <- getDateRangeX(inpts=obsdata,ncIn=chlfiles)
seawifsmean <- rbindlist(lapply(chlfilesIn,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius,alt=alt),fill=TRUE)

obsdata$seawifs_new_mean<-rep(NA, length(obsdata[,1]))
obsdata$seawifs_new_mean[seawifsmean$X]<-seawifsmean$chlorophyll_mean

head(obsdata)
range(obsdata$seawifs_new_mean, na.rm=T)

rm(seawifsmean); rm(chlfilesIn)

# 
### MODIS
#http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMHchla8day.nc?chlorophyll[(2013-10-12T00:00:00Z):1:(2013-10-12T00:00:00Z)][(0.0):1:(0.0)][(-90.0):1:(90.0)][(0.0):1:(360.0)]
filenames<-grep("erdMH1chla8day",list.ncs)
# filenames<-grep("erdMH1chlamday",list.ncs)
chlfiles<-list.ncs[filenames]
varname<-"chlorophyll";  mnradius<- 1; sdradius<- 1; alt<-1
chlfilesIn <- getDateRangeX(inpts=obsdata,ncIn=chlfiles)
modismean <- rbindlist(lapply(chlfilesIn,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius,alt=alt),fill=TRUE)

obsdata$modis_new_mean<-rep(NA, length(obsdata[,1]))
obsdata$modis_new_mean[modismean$X]<-modismean$chlorophyll_mean

head(obsdata)
range(obsdata$modis_new_mean, na.rm=T)

rm(modismean); rm(chlfilesIn)


### VIIRS
#http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMHchla8day.nc?chlorophyll[(2013-10-12T00:00:00Z):1:(2013-10-12T00:00:00Z)][(0.0):1:(0.0)][(-90.0):1:(90.0)][(0.0):1:(360.0)]
filenames<-grep("nesdisVHNSQchlaWeekly",list.ncs)
# filenames<-grep("nesdisVHNSQchlaMonthly",list.ncs)
chlfiles<-list.ncs[filenames]
varname<-"chlor_a";  mnradius<- 0.25; sdradius<- 1; alt<-1
chlfilesIn <- getDateRangeX(inpts=obsdata,ncIn=chlfiles)
viirsmean <- rbindlist(lapply(chlfilesIn,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=mnradius,alt=alt),fill=TRUE)

obsdata$viirs_new_mean<-rep(NA, length(obsdata[,1]))
obsdata$viirs_new_mean[viirsmean$X]<-viirsmean$chlor_a_mean

head(obsdata)
range(obsdata$viirs_new_mean, na.rm=T)

rm(viirsmean); rm(chlfilesIn)

### i) blendo products to deal with gappiness -------------------------------------

# # first, replace 'NaN's with 'NA'
# for (x in 1:dim(obsdata)[1]){
#     if(obsdata$HiCHL[x] == "NaN"){
#         obsdata$HiCHL[x] <- "NA"
#     }
# }

# THEN ... .blend products
# obsdata$chl <- as.numeric(obsdata$HiCHL)
obsdata$chl_mean <- as.numeric(obsdata$modis_new_mean)

for (x in 1:dim(obsdata)[1]){

    if (is.na(obsdata$chl_mean[x])) {obsdata$chl_mean[x] <- obsdata$seawifs_new_mean[x]}    #chl
    # if (is.na(presabs$sst[x])) {presabs$sst <- presabs$path_new_mean[x]}    #sst

}

for (x in 1:dim(obsdata)[1]){
    
    if (is.na(obsdata$chl_mean[x])) {obsdata$chl_mean[x] <- obsdata$viirs_new_mean[x]}    #chl
    # if (is.na(presabs$sst[x])) {presabs$sst <- presabs$path_new_mean[x]}    #sst
    
}

# recheck % gappiness
(length(which(is.na(obsdata$chl_mean)))/length(obsdata[,1]))*100  ## 23.9% missing - bledno chl

na_by_cols <- lapply(obsdata, function(x) sum(is.na(x)))

(na_by_cols$chl_mean / nrow(obsdata)) * 100

#### ii) Log Transform Chl & EKE -------------------------------------------------------
# # #chl
# obsdata$logChl <- log(obsdata$chl+0.0001)
# hist(obsdata$logChl)
# 
# #eke
# eke_mean<-1/2*(obsdata$SSHu^2+obsdata$SSHv^2)
# head(eke_mean)
# hist(eke_mean)
# 
# l.eke_mean <- log(eke_mean+0.0001)
# obsdata$log_eke <- l.eke_mean
# hist(obsdata$log_eke)	




## Save RDS File ----
saveRDS(obsdata, file = "./data/processed/xtracted_cc_sept_mar_aug_1997_2023.rds")

# saveRDS(obsdata, file = "../data/processed/xtracted_cc_sept_sst_lag_aug_1997_2023.rds") # only for sept turtle position - aug sst lag
