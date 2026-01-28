#library(remotes)
#install.packages("https://cran.r-project.org/src/contrib/Archive/scPDSI/scPDSI_0.1.3.tar.gz", 
#                 repo=NULL, type="source")

library(sf)
library(ggplot2)
library(raster)
library(climatol)
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)
#library(SPEI)
library(scPDSI)
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

# due to the naming convention of some allotments, there were / in the name, go through
# and get rid of those and switch them to .
for(i in 1:85){
  loc = allots_cent_df[i,1]
  new_loc = gsub('/','.',loc)
  allots_cent_df[i,1] = new_loc
}

# empty dataframe to add site data to
all_sites_annual = data.frame(year = as.numeric(),
                              ppt = as.numeric(),
                              pet = as.numeric(),
                              ratio = as.numeric(),
                              site = as.character())
for(i in 1:85){
  first = allots_cent_df[i,]
  site_data <- download_daymet(lat = first$Y,
                               lon = first$X,
                               start = 1990,
                               end = 2020,
                               internal = TRUE)
  simple_data = site_data$data
  # for use in calculating PET, convert from W m-2 to MJ m-1 d-1
  simple_data$srad..MJ.m2.d = (simple_data$srad..W.m.2. * simple_data$dayl..s.)/1000000
  simple_data$dayl..h. = simple_data$dayl..s./60/60
  # calculate PET using the Hargreaves Samani method
  simple_data$pet = (0.0023*0.408*simple_data$srad..MJ.m2.d*(mean(c(simple_data$tmax..deg.c.,simple_data$tmin..deg.c.)) + 17.8)*
                       sqrt(simple_data$tmax..deg.c.- simple_data$tmin..deg.c.))
  
  
  simple_data$date = as.Date(simple_data$yday - 1, origin = paste0(simple_data$year,"-01-01"))
  simple_data$month <- month(as.POSIXlt(simple_data$date, format="%Y-%m-%d"))
  # Summarize the monthly data for each month of each year
  summarized_data<- simple_data %>% group_by(year, month) %>% summarize(PPT_mm=sum(prcp..mm.day.),
                                                              PET_mm=sum(pet))
  # now calculate the ppt and pet for each water year
  # Ex: 1991 water year = October 1, 1990 - September 30, 1991
  annual_ppt = data.frame(year = 1991:2020,ppt=0,pet=0)
  for(j in 1991:2020){
    year.0 = summarized_data %>% filter(year == (j-1) & month >= 10)
    year.1 = summarized_data %>% filter(year == j & month < 10)
    year = rbind(year.0,year.1)
    annual_ppt[annual_ppt$year == j,2] = sum(year$PPT_mm)
    annual_ppt[annual_ppt$year == j,3] = sum(year$PET_mm)
  }
  
  annual_ppt$ratio = annual_ppt$ppt/annual_ppt$pet
  annual_ppt$site = first$ALLOT_NAME
  
  # add 30 years of water data to the empty dataframe
  all_sites_annual = rbind(all_sites_annual,annual_ppt)
}
  mean(all_sites_annual$ppt)
  mean(all_sites_annual$pet)
  mean(all_sites_annual$ratio)
  
  # now compute the average for each year across all sites and add it as a repeated row
  # for easy display
  all_sites_annual[2551:2580,1] = 1991:2020
  all_sites_annual[2551:2580,2] = rep(mean(all_sites_annual$ppt,na.rm=T),times=30)
  all_sites_annual[2551:2580,3] = rep(mean(all_sites_annual$pet,na.rm=T),times=30)
  all_sites_annual[2551:2580,4] = rep(mean(all_sites_annual$ratio,na.rm=T),times=30)
  all_sites_annual[2551:2580,5] = rep('average',times=30)
  
  # add a column with labels that you would like for each line
  all_sites_annual$yearColor = ifelse(all_sites_annual$site == 'average',
                                      '30 Year Regional Average',
                                      'Single Site')
  # plot cumulative annual ppt and pet for each site highlighting the average
  # for all allotments
  ggplot(all_sites_annual,aes(x=year,y=ppt,group=site,alpha=yearColor))+
    geom_line()+
    scale_alpha_manual(values = c(1,0.15))
  ggplot(all_sites_annual,aes(x=year,y=pet,group=site,alpha=yearColor))+
    geom_line()+
    scale_alpha_manual(values = c(1,0.15))
  
  
  
  # unless something changes, all this is just scratch code and should be deleted prior to publication
  
  
  
  
  yearly.avg = all_sites_annual[all_sites_annual$yearColor != '30 Year Regional Average',] %>%
    group_by(year) %>%
    summarize(ppt = mean(ppt),pet = mean(pet),ratio = mean(ratio))
  yearly.avg$site = 'year.average'
  yearly.avg$yearColor = 'Annual Regional Average'
  
  df = rbind(all_sites_annual,yearly.avg)
  ggplot(df,aes(x=year,y=ppt,group=site,alpha=yearColor,color=yearColor))+
    geom_line()+
    scale_alpha_manual(values = c(1,1,0.15))+
    scale_color_manual(values = c('black','red','black'))
  ggplot(df,aes(x=year,y=pet,group=site,alpha=yearColor,color=yearColor))+
    geom_line()+
    scale_alpha_manual(values = c(1,1,0.15))+
    scale_color_manual(values = c('black','red','black'))
  
  site.avgs = all_sites_annual[all_sites_annual$site !='average',] %>% group_by(site) %>%
    summarize(site.avg.ppt = mean(ppt))
  sites = site.avgs$site
  all_sites_annual$local.ratio = 0
  for(i in sites){
    local.avg = as.numeric(site.avgs[site.avgs$site == i,2])
    local.years = all_sites_annual[all_sites_annual$site == i,]
    all_sites_annual[all_sites_annual$site == i,7] = local.years$ppt/local.avg
  }
  yearly.ratio = all_sites_annual[all_sites_annual$yearColor != '30 Year Regional Average',] %>%
    group_by(year) %>%
    summarize(local.ratio = mean(local.ratio))
  yearly.ratio$site = 'year.average'
  yearly.ratio$yearColor = 'Annual Regional Average'
  yearly.ratio$pet = 0
  yearly.ratio$ppt = 0
  yearly.ratio$ratio = 0
  df2 = rbind(all_sites_annual,yearly.ratio)
  df2 = df2 %>% filter(yearColor != '30 Year Regional Average')
  
  ggplot(df2,aes(x=year,y=local.ratio,group=site,alpha=yearColor,color=yearColor))+
    geom_line()+
    scale_alpha_manual(values = c(1,0.15))+
    scale_color_manual(values = c('red','black'))+
    labs(x = 'Year',y='Percent of Average',alpha=NULL,color=NULL)
  
  options(PDSI.coe.K1.1 = 1.6)
  options(PDSI.coe.K1.3 = 0.4)
  options(PDSI.coe.K2 = 16.84)
  options(PDSI.p = 0.755)
  options(PDSI.q = 1/1.63)
  x = pdsi(summarized_data$PPT_mm,summarized_data$PET_mm,
       start = 1990, end = 2020)
  summarized_data$pdsi = x$X
  
  plot(x, index = "WPLM")
  
  y = pdsi(summarized_data$PPT_mm,summarized_data$PET_mm,
           start = 1990, end = 2020,sc=F)
  plot(y, index = "WPLM")
  summarized_data$pdsi = x$WPLM
  ggplot(simple_data,aes(x = date,y=pet)) +
    geom_line()
  
  
  monthly.avs = summarized_data %>% group_by(month) %>% summarize(PPT_mm=mean(PPT_mm))
  #summarized_data$month = as.character(summarized_data$month)
  summarized_data$month.year = paste0(summarized_data$year,'-',summarized_data$month)
  summarized_data$avg =  rep(monthly.avs$PPT_mm, times = 31)
  summarized_data = ungroup(summarized_data)
  ggplot(summarized_data) +
    geom_line(aes(x=month.year,y = PPT_mm,group= 1)) +
    geom_line(aes(x=month.year,y = avg, group = 2),color='red')
  
  summarized_data$pdsi
  