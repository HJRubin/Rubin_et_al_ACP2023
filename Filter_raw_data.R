#This code filters nitrogen and sulfur deposition observations
#from the NADP, EMEP, EANET, CAPMoN, and AIRMoN.
#See Rubin et al., (2023) published in ACP for details.

setwd("~/R/Wet Deposition")
library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(stringr)
library(stringi)
library(lubridate)
library(measurements)
library(readxl)

#First download all datasets and get them in the same format.
#Then check units.
#Then merge together into a single dataframe. Export as csv.

#NTN
ntn = read.csv('NTN-All-a-s-dep.csv')
# ntn = read.csv('NTN-All-2020-dep.csv')
ntn_sites = read.csv('NTNsites.csv')
colnames(ntn_sites)[which(names(ntn_sites) == "siteid")] <- "siteID"

ntn_merge = merge(ntn, ntn_sites)
ntn_merge_2010 = ntn_merge[ntn_merge$yr == "2010",]
# write.csv(ntn_merge_2010, "ntn_merge_2010.csv")
ntn_filt = ntn_merge_2010 %>%
  select(siteID, latitude, longitude, NH4, NO3, ppt, SO4)

write.csv(ntn_filt, 'ntn_03092022.csv')

#EMEP
emep = read.csv("EMEP_annual.csv")
emep_sites = read.csv('EMEP_sites.csv')
emep_merge = merge(emep, emep_sites)
emep_filt = emep_merge %>%
  select(Code, Latitude, Longitude, NH4, NO3, SO4, mm)

#EANET
eanet = read_excel('Wet2010Annual_eanet.csv')
eanet_filt = eanet %>%
  select(Code, Latitude, Longitude, NH4...mmol.m.2y.1., NO3...mmol.m.2y.1., SO42...mmol.m.2y.1., Precip_mm)

china = read.csv("China_dep.csv")

capmon = read.csv('CAPMoN-AllSites-2010_1.csv')
capmon_sites = read.csv("CAPMoN_sites.csv")
capmon_sites = capmon_sites %>% select(SiteName_NomDuSite, ID, Province_State, Country_Pays, Organization, Network, Latitude, Longitude)
capmon_sites_u = unique(capmon_sites)

capmon_sep = capmon %>%
  separate(SiteID, 
           c("SiteName", "ID"), 
           sep=cumsum(c(9,3)))

capmon_join = merge(capmon_sep, capmon_sites_u, by = c("ID"))

capmon_join$SO4_mgm2 = capmon_join$SO4_mgL * capmon_join$SampleVolume_mL
capmon_join$NO3_mgm2 = capmon_join$NO3_mgL * capmon_join$SampleVolume_mL
capmon_join$NH4_mgm2 = capmon_join$NH4_mgL * capmon_join$SampleVolume_mL

capmon_filt = capmon_join %>% select(Latitude, Longitude, NO3_mgm2, NH4_mgm2, SO4_mgm2, SampleVolume_mL)

capmon_rem = aggregate(capmon_filt$SO4_mgm2, by = list(capmon_filt$Latitude, capmon_filt$Longitude), FUN=mean)
capmon_rem1 = aggregate(capmon_filt$NO3_mgm2, by = list(capmon_filt$Latitude, capmon_filt$Longitude), FUN=mean)
capmon_rem2 = aggregate(capmon_filt$NH4_mgm2, by = list(capmon_filt$Latitude, capmon_filt$Longitude), FUN=mean)
capmon_rem3 = aggregate(capmon_filt$Precip, by = list(capmon_filt$Latitude, capmon_filt$Longitude), FUN=mean)

colnames(capmon_rem)[which(names(capmon_rem) == "Group.1")] <- "Latitude"
colnames(capmon_rem)[which(names(capmon_rem) == "Group.2")] <- "Longitude"
colnames(capmon_rem)[which(names(capmon_rem) == "x")] <- "SO4_mgm2"
capmon_rem$NO3_mgm2 = capmon_rem1$x
capmon_rem$NH4_mgm2 = capmon_rem2$x
capmon_rem$Precip = capmon_rem3$x

colnames(eanet_filt)[which(names(eanet_filt) == "Code")] <- "siteID"
colnames(eanet_filt)[which(names(eanet_filt) == "Precip_mm")] <- "Precip"
colnames(ntn_filt)[which(names(ntn_filt) == "latitude")] <- "Latitude"
colnames(ntn_filt)[which(names(ntn_filt) == "longitude")] <- "Longitude"
colnames(ntn_filt)[which(names(ntn_filt) == "ppt")] <- "Precip"
colnames(eanet_filt)[which(names(eanet_filt) == "NH4...mmol.m.2y.1.")] <- "NH4"
colnames(eanet_filt)[which(names(eanet_filt) == "NO3...mmol.m.2y.1.")] <- "NO3"
colnames(eanet_filt)[which(names(eanet_filt) == "SO42...mmol.m.2y.1.")] <- "SO4"
colnames(eanet_filt)[which(names(eanet_filt) == "Precip_mm")] <- "Precip"
colnames(eanet_filt)[which(names(eanet_filt) == "Code")] <- "siteID"
colnames(emep_filt)[which(names(emep_filt) == "Code")] <- "siteID"
colnames(emep_filt)[which(names(emep_filt) == "mm")] <- "Precip"
colnames(china)[which(names(china) == "LAT")] <- "Latitude"
colnames(china)[which(names(china) == "LON")] <- "Longitude"
colnames(china)[which(names(china) == "Code")] <- "siteID"
colnames(capmon_filt)[which(names(capmon_filt) == "SampleVolume_mL")] <- "Precip"

eanet_filt$SO4_mgm2 = eanet_filt$SO4 * 96.07
eanet_filt$NH4_mgm2 = eanet_filt$NH4 * 18
eanet_filt$NO3_mgm2 = eanet_filt$NO3 * 62

eanet_lat = eanet_filt$Latitude
eanet_lon = eanet_filt$Longitude
eanet_lat = gsub('?', 'd', eanet_lat)
eanet_lon = gsub('?', 'd', eanet_lon)

eanet_lat = sp::char2dms(eanet_lat)
eanet_lat_1 = sp::as.numeric.DMS(eanet_lat)
eanet_lon = sp::char2dms(eanet_lon)
eanet_lon_1 = sp::as.numeric.DMS(eanet_lon)

eanet_filt_1 = eanet_filt %>% select(NO3_mgm2, NH4_mgm2, SO4_mgm2, Precip)
eanet_filt_1$Latitude = eanet_lat_1
eanet_filt_1$Longitude = eanet_lon_1


#EMEP
emep_filt$NO3_mgm2 = emep_filt$NO3 * 1000
emep_filt$SO4_mgm2 = emep_filt$SO4 * 1000
emep_filt$NH4_mgm2 = as.numeric(emep_filt$NH4) * 1000


emep_lat = emep_filt$Latitude
emep_lon = emep_filt$Longitude
emep_lat = gsub('?', 'd', emep_lat)
emep_lon = gsub('?', 'd', emep_lon)

emep_lat = sp::char2dms(emep_lat)
emep_lat_1 = sp::as.numeric.DMS(emep_lat)
emep_lon = sp::char2dms(emep_lon)
emep_lon_1 = sp::as.numeric.DMS(emep_lon)

emep_filt_1 = emep_filt %>% select(NO3_mgm2, NH4_mgm2, SO4_mgm2, Precip)
emep_filt_1$Latitude = emep_lat_1
emep_filt_1$Longitude = emep_lon_1


#Amon
amon = read.csv("AMoN-ALL-W.csv")
sites = read.csv("amon_sites.csv")
colnames(amon)[which(names(amon) == "siteID")] <- "siteId"
amon_merge = merge(amon, sites)
amon_filt = amon_merge[amon_merge$dep..mg. > 0,]
amon_filt$depN = amon_filt$dep..mg. * 18/14
write.csv(amon_filt, "amon_filt.csv")  
  
#Conversion
# china$NO3_mgm2 = china$NO3 * china$Precip
# china$NH4_mgm2 = china$NH4 * china$Precip
# china$SO4_mgm2 = china$SO4 * china$Precip
china$NO3_mgm2 = china$NO3 
china$NH4_mgm2 = china$NH4 
china$SO4_mgm2 = china$SO4 
china_filt = china %>% select(Latitude, Longitude, NO3_mgm2, NH4_mgm2, SO4_mgm2, Precip)

# colnames(china_filt)[which(names(china_filt) == "NO3_mgm2")] <- "NO3"
# colnames(china_filt)[which(names(china_filt) == "NH4_mgm2")] <- "NH4"
# colnames(china_filt)[which(names(china_filt) == "SO4_mgm2")] <- "SO4"


ntn_filt$NO3_mgm2 = ntn_filt$NO3 * 100
ntn_filt$NH4_mgm2 = ntn_filt$NH4 * 100
ntn_filt$SO4_mgm2 = ntn_filt$SO4 * 100
ntn_filt$Precip = ntn_filt$Precip * 10

ntn_filt_1 = ntn_filt %>% select(Latitude, Longitude, NO3_mgm2, NH4_mgm2, SO4_mgm2, Precip)

indaaf1 = read.csv('INDAAF1.csv')

a = rbind(ntn_filt_1, china_filt, emep_filt_1, eanet_filt_1, capmon_rem, indaaf1)

b = rbind(ntn_filt_1, emep_filt_1, eanet_filt_1, capmon_rem, indaaf1)

write.csv(a, "FINAL_wetdep_102521.csv")
write.csv(b, "FINAL_wetdep__precip_100821.csv")


###MAP###
wet = read.csv('FINAL_wetdep_102521.csv')
wet$totalwet = wet$NO3_mgm2 + wet$NH4_mgm2
dry = read.csv('FINAL/combined_dry_dep_2010.csv')
dry[is.na(dry)] <- 0
dry$total = dry$TNH3_ug.m.3 + dry$TNH4_ug.m.3 + dry$TOTAL_NO2_ug.m.3 + dry$TOTAL_NO3_ug.m.3
dry$totalmg = dry$total/10
colnames(dry)[which(names(dry) == "LATITUDE")] <- "Latitude"
colnames(dry)[which(names(dry) == "LONGITUDE")] <- "Longitude"


both = merge(wet, dry, by = c("Latitude", "Longitude"), all = T)
both[is.na(both)] <- 0

both$TOTAL = both$totalwet + both$total.y
both$TOTALmg = both$totalwet + both$totalmg

write.csv(both, 'wetanddrydep_11152022.csv')

