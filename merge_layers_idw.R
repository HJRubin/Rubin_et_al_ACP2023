#This script takes input from a database and joins layers that are
#too large for ArcGIS to handle.
#See Rubin et al., (2023) published in ACP for details.

library(sf)
library(sp)
library(dplyr)
library(tidyverse)
library(rgdal)
library(RSQLite)

#First, read in observation and model layers.
#Merge appropriate layers. 
#Calculate IDW. 
#Write as raster. 

fgdb = "C:/Users/User/OneDrive - University of Tennessee/Documents/ArcGIS/Projects/MMF_Feb_2023/MMF_Feb_2023.gdb"

fc <- sf::st_read(fgdb, layer = "wetno3_htap_points")
obs <- sf::st_read(fgdb, layer = "observations")
near_tab <- sf::st_read(fgdb, layer = "wetno3_neartable1deg")

obs$id = 1:815

no3_join = merge(near_tab, obs, by.x ="NEAR_FID", by.y = "id",  all.x = TRUE)
no3_join_df = as.data.frame(no3_join)
fc_df = as.data.frame(fc)

no3_join_all= merge(fc_df, no3_join_df, by.x ="pointid", by.y = "IN_FID", all = TRUE)
no3_join_all_geo = no3_join_all %>% st_sf(sf_column_name = 'Shape')

# max(no3_join_all$NEAR_DIST, na.rm = TRUE)
no3_join_all_geo$weight = (1 - (no3_join_all_geo$NEAR_DIST / 111194.4))^2 #555974.3 ##277987.2
no3_join_all_geo$weight1 = 1 - no3_join_all_geo$weight
no3_join_all_geo$obs_no3 = no3_join_all_geo$grid_code #* 1000000 * 31540000

no3_join_all_geo$Lat = as.numeric(no3_join_all_geo$Lat)
no3_join_all_geo$NO3_mgm2 = as.numeric(no3_join_all_geo$NO3_mgm2)
no3_join_all_geo$Precip = as.numeric(no3_join_all_geo$Precip)
no3_join_all_geo$NEAR_RANK = as.numeric(no3_join_all_geo$NEAR_RANK)
no3_join_all_geo$NEAR_FID = as.numeric(no3_join_all_geo$NEAR_FID)
no3_join_all_geo$num = as.numeric(no3_join_all_geo$num)
no3_join_all_geo$pointid = as.numeric(no3_join_all_geo$pointid)


no3_join_all_geo$idw = (no3_join_all_geo$weight * no3_join_all_geo$NO3_mgm2) + (no3_join_all_geo$weight1 * no3_join_all_geo$obs_no3)


no3_join_all_geo %>% 
  st_write("no3_1deg_geo_030723.gpkg")

# library(ncdf4)
# library(raster)
# 
# filename = ("C:/Users/User/Documents/HTAP/Other Dep/htap2_BASE_mmm_wetcomno3_Surface_2010_monthly.nc_2010mean.nc")
# 
# r <- brick(filename, varname="wetcomno3")

# nc <- nc_open(filename)

# # Get extent info
# ncvar_get(nc, "lat")
# ncvar_get(nc, "lon")

# extent(r) <- c(-180, 179.75, -90, 90)
# 
# writeRaster(r, "C:/Users/User/Documents/HTAP/Other Dep/Good/htap2_BASE_mmm_wetcomno3_Surface_2010_mean.nc", format="CDF", varname="wetcomno3", xname="lon", yname="lat", overwrite=TRUE)
