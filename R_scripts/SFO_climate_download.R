library(sf)
library(ggplot2)
library(raster)
library(climatol)
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)
library(daymetr)

# read in the shapefile with pastures of the SFO
pasts = shapefile("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")
# these two allotments are actually in Challis but due to a weird projection, a little got cut
# drop them
pasts = pasts[pasts$ALLOT_NAME != 'Allison Creek',]
pasts = pasts[pasts$ALLOT_NAME != 'Hat Creek',]

# what we want for this is a single poly for each allotment, not pasture so merge based on allot
allots = raster::aggregate(pasts,by='ALLOT_NAME',FUN=sum)

# calculate the centroid of each allotment, I only wanted to use a single point
# for climate data since the allotments really aren't that big
allots_cent <- st_centroid(st_as_sf(allots))

# need to convert to an sf object 
allots_cent_sf = st_as_sf(allots_cent)
allots_cent_locs = cbind(allots_cent_sf, st_coordinates(allots_cent_sf))
# create a dataframe with coordinates as the X, Y columns for each centroid point
allots_cent_df = as.data.frame(allots_cent_locs)
keeps = c("ALLOT_NAME","X","Y")
allots_cent_df = allots_cent_df[,keeps]

# due to the naming convention of some allotments, there were / in the name, go through
# and get rid of those and switch them to .
for(i in 1:85){
  loc = allots_cent_df[i,1]
  new_loc = gsub('/','.',loc)
  allots_cent_df[i,1] = new_loc
}

all.climate = data.frame(year = as.integer(),PPT_mm = as.numeric(),Tmax_C = as.numeric(),Tmin_C = as.numeric(),Elevation_m = as.numeric(),allot_name = as.character())
### this loop creates average weather data across the whole year
for(i in 1:85){
  first = allots_cent_df[i,]
  
  site_data <- download_daymet(site = first$ALLOT_NAME,
                               lat = first$Y,
                               lon = first$X,
                               start = 2000,
                               end = 2020,
                               internal = TRUE,
                               simplify = TRUE)
  site <- site_data[1,1:5]
  # for WL, only care about tmax, tmin, and ppt
  site_sub <- site_data[(site_data$measurement=="prcp..mm.day." |
                           site_data$measurement=="tmax..deg.c." |
                           site_data$measurement=="tmin..deg.c."),]
  
  # join yday and year to then create a month column
  site_sub$year.yday <- paste(site_sub$year, site_sub$yday, sep="-")
  
  site_sub$month <- month(as.POSIXlt(site_sub$year.yday, format="%Y-%j"), 
                          label=TRUE)
  site_sub <- pivot_wider(site_sub, id_cols=c(year, yday, month),
                          names_from=measurement, values_from=value)
  
  # aggregate by year
  tmp.mod1 <- site_sub %>% group_by(year) %>%
    summarize(PPT_mm = sum(prcp..mm.day.), Tmax_C=mean(tmax..deg.c.),
              Tmin_C=mean(tmin..deg.c.))
  tmp.mod1$elevation_m = site$altitude
  tmp.mod1$allot_name = first$ALLOT_NAME
  
  all.climate = rbind(all.climate,tmp.mod1)
}

write.csv(all.climate,"C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_climate.csv")

# need to create a loop to get weather data for phenology relevant measures
all.climate = data.frame(spring.Tmax_C = as.numeric(),
                         spring.Tmin_C = as.numeric(),
                         spring.PPT_mm = as.numeric(),
                         summer.Tmax_C = as.numeric(),
                         summer.Tmin_C = as.numeric(),
                         summer.PPT_mm = as.numeric(),
                         fall.Tmax_C = as.numeric(),
                         fall.Tmin_C = as.numeric(),
                         fall.PPT_mm = as.numeric(),
                         early.season.Tmax_C = as.numeric(),
                         early.season.Tmin_C = as.numeric(),
                         early.season.PPT_mm = as.numeric(),
                         mid.season.Tmax_C = as.numeric(),
                         mid.season.Tmin_C = as.numeric(),
                         mid.season.PPT_mm = as.numeric(),
                         late.season.Tmax_C = as.numeric(),
                         late.season.Tmin_C = as.numeric(),
                         late.seaon.PPT_mm = as.numeric(),
                         annual.Tmax_C = as.numeric(),
                         annual.Tmin_C = as.numeric(),
                         annual.PPT_mm = as.numeric(),
                         elevation_m = as.numeric(),
                         allot_name = as.character())
for(i in 1:85){
  first = allots_cent_df[i,]
  
  site_data <- download_daymet(site = first$ALLOT_NAME,
                               lat = first$Y,
                               lon = first$X,
                               start = 2000,
                               end = 2020,
                               internal = TRUE,
                               simplify = TRUE)
  site <- site_data[1,1:5]
  # for WL, only care about tmax, tmin, and ppt
  site_sub <- site_data[(site_data$measurement=="prcp..mm.day." |
                           site_data$measurement=="tmax..deg.c." |
                           site_data$measurement=="tmin..deg.c."),]
  
  # join yday and year to then create a month column
  site_sub$year.yday <- paste(site_sub$year, site_sub$yday, sep="-")
  
  site_sub$month <- month(as.POSIXlt(site_sub$year.yday, format="%Y-%j"), 
                          label=TRUE)
  site_sub <- pivot_wider(site_sub, id_cols=c(year, yday, month),
                          names_from=measurement, values_from=value)
  site_sub$date = as.Date(site_sub$yday - 1,origin = paste0(site_sub$year,'-01-01'))
  site_sub$Tavg_C = (site_sub$tmax..deg.c. + site_sub$tmin..deg.c.)/2
  # aggregate by year
  tmp.mod1 = site_sub %>% group_by(year) %>%
    reframe(spring.Tmax_C = mean(tmax..deg.c.[yday >= 80 & yday < 172]),
            spring.Tmin_C = mean(tmin..deg.c.[yday >= 80 & yday < 172]),
           spring.PPT_mm = sum(prcp..mm.day.[yday >= 80 & yday < 172]),
           summer.Tmax_C = mean(tmax..deg.c.[yday >= 172 & yday < 264]),
           summer.Tmin_C = mean(tmin..deg.c.[yday >= 172 & yday < 264]),
           summer.PPT_mm = sum(prcp..mm.day.[yday >= 172 & yday < 264]),
           fall.Tmax_C = mean(tmax..deg.c.[yday >= 264 & yday < 355]),
           fall.Tmin_C = mean(tmin..deg.c.[yday >= 264 & yday < 355]),
           fall.PPT_mm = sum(prcp..mm.day.[yday >= 264 & yday < 355]),
           early.season.Tmax_C = mean(tmax..deg.c.[yday >= 0 & yday < 121]),
           early.season.Tmin_C = mean(tmin..deg.c.[yday >= 0 & yday < 121]),
           early.season.PPT_mm = mean(prcp..mm.day.[yday >= 0 & yday < 121]),
           mid.season.Tmax_C = mean(tmax..deg.c.[yday >= 121 & yday < 264]),
           mid.season.Tmin_C = mean(tmin..deg.c.[yday >= 121 & yday < 264]),
           mid.season.PPT_mm = mean(prcp..mm.day.[yday >= 121 & yday < 264]),
           late.season.Tmax_C = mean(tmax..deg.c.[yday >= 264]),
           late.season.Tmin_C = mean(tmin..deg.c.[yday >= 264]),
           late.seaon.PPT_mm = mean(prcp..mm.day.[yday >= 264]),
           annual.Tmax_C = mean(tmax..deg.c.),
           annual.Tmin_C = mean(tmin..deg.c.),
           annual.PPT_mm = mean(prcp..mm.day.))  
  
  tmp.mod1$elevation_m = site$altitude
  tmp.mod1$allot_name = first$ALLOT_NAME
  
  all.climate = rbind(all.climate,tmp.mod1)
}
write.csv(all.climate,"C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno_climate.csv")
