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
pasts = shapefile("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")
# these two allotments are actually in Challis but due to a weird projection, a little got cut
# drop them
pasts = pasts[pasts$ALLOT_NAME != 'Allison Creek',]
pasts = pasts[pasts$ALLOT_NAME != 'Hat Creek',]

# what we want for this is a single poly for each allotment, not pasture so merge based on allot
allots = raster::aggregate(pasts,by='ALLOT_NAME',FUN=sum)

# calculate the centroid of each allotment, I only wanted to use a single point
# for climate data since the allotments really aren't that big
allots_cent <- st_centroid(st_as_sf(allots))

ggplot() + 
  geom_sf(data = st_as_sf(allots), fill = 'white') +
  geom_sf(data = allots_cent, color = 'red') 

# need to convert to an sf object 
allots_cent_sf = st_as_sf(allots_cent)
allots_cent_locs = cbind(allots_cent_sf, st_coordinates(allots_cent_sf))
# create a dataframe with coordinates as the X, Y columns for each centroid point
allots_cent_df = as.data.frame(allots_cent_locs)
keeps = c("ALLOT_NAME","X","Y")
allots_cent_df = allots_cent_df[,keeps]


# empty dataframe to fill with for loop
new = data.frame(V1 = c(0,0,0,0),V2 = c(0,0,0,0),V3 = c(0,0,0,0), V4 = c(0,0,0,0),
                 V5 = c(0,0,0,0),V6 = c(0,0,0,0),V7 = c(0,0,0,0), V8 =c(0,0,0,0),
                 V9 = c(0,0,0,0), V10 = c(0,0,0,0), V11 = c(0,0,0,0), V12 = c(0,0,0,0))
# due to the naming convention of some allotments, there were / in the name, go through
# and get rid of those and switch them to .
for(i in 1:85){
  loc = allots_cent_df[i,1]
  new_loc = gsub('/','.',loc)
  allots_cent_df[i,1] = new_loc
}

# June - Oct represent months that could be dry so I wanted to test them out
# in the loop below, for each site, every time one of those months has P < 2T it adds 1
# to the month count for the respective month
count.sept = 0
count.june = 0
count.oct = 0
count.july = 0
count.august = 0
for(i in 1:85){
first = allots_cent_df[i,]

site_data <- download_daymet(site = first$ALLOT_NAME,
                             lat = first$Y,
                             lon = first$X,
                             start = 1991,
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

# aggregate by each month of each year
tmp.mod1_Van <- site_sub %>% group_by(year, month) %>%
  summarize(PPT_mm=sum(prcp..mm.day.), Tmax_C=mean(tmax..deg.c.),
            Tmin_C=mean(tmin..deg.c.), Tabs_C=min(tmin..deg.c.))

#to means for each month across years
site_climate.normal <- tmp.mod1_Van %>%
  group_by(month) %>% summarize(PPT_mm=mean(PPT_mm), Tmax_C=mean(Tmax_C),
                                Tmin_C=mean(Tmin_C), Tabs_C=min(Tabs_C))

#remove month in left column
site_climate.normal <- site_climate.normal[,-1]

site_climate.normal <- as.data.frame(t(site_climate.normal),
                                     stringsAsFactors = FALSE)
# add the columns from the site data to the overall area dataframe
common <- intersect(names(new), names(site_climate.normal))
new[common] = lapply(common, \(x) new[[x]] + site_climate.normal[[x]])

# this is checking each month P < 2T as described above
sept = site_climate.normal[1:3,9]
d.sept = ifelse(sept[1]<2*(mean(c(sept[2],sept[3]))),1,0)
count.sept = count.sept + d.sept

june = site_climate.normal[1:3,6]
d.june = ifelse(june[1]<2*(mean(c(june[2],june[3]))),1,0)
count.june = count.june + d.june

july = site_climate.normal[1:3,7]
d.july = ifelse(july[1]<2*(mean(c(july[2],july[3]))),1,0)
count.july = count.july + d.july

august = site_climate.normal[1:3,8]
d.august = ifelse(august[1]<2*(mean(c(august[2],august[3]))),1,0)
count.august = count.august + d.august

oct = site_climate.normal[1:3,10]
d.oct = ifelse(oct[1]<2*(mean(c(oct[2],oct[3]))),1,0)
count.oct = count.oct + d.oct
}

# to get average of climate data, divide by number of allotments
new = new/85

row.names(new) = row.names(site_climate.normal) 
site
# create the WL diagram, est and alt should be changed in the future
diagwl(new, est="Vancouver, Canada",cols=NULL,
       alt=site$altitude, per="1990-2020", mlab="en")


## results were 0 for june, 33 for september, 85 for July, 85 for august, 0 for oct

