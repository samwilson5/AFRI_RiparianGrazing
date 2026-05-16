library(sf)
library(stringr)
library(dplyr)
library(phenofit)
library(zoo)
library(ggplot2)

# read in pasture data
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])

# create unique identifier for each allotment/pasture combo
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)

combo_List = pastsDF$combo_NO

# read in NDVI & QA data for Landsat 7 (2000 - 2012), downloaded from GEE
# asignment of weights is based on docs from phenofit and USGS
for(i in combo_List){
  ndvi = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT_NDVI/",i,'.csv'))
  qa = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT_QA/",i,'.csv'))
  full = ndvi %>% left_join(qa,join_by('DATE_ACQUIRED' == 'DATE_ACQUIRED'))
  full = full %>%
    mutate(Weight = case_when(QA_PIXEL == 1 ~ 0,
                              QA_PIXEL == 5440 ~ 1,
                              QA_PIXEL == 5442 ~ 0.5,
                              QA_PIXEL == 5504 ~ 1,
                              QA_PIXEL == 5506 ~ 0.5,
                              QA_PIXEL == 5696 ~ 0.5,
                              QA_PIXEL == 5698 ~ 0.5,
                              QA_PIXEL == 5760 ~ 0.5,
                              QA_PIXEL == 5896 ~ 0.2,
                              QA_PIXEL == 7440 ~ 0.2,
                              QA_PIXEL == 7442 ~ 0.2,
                              QA_PIXEL == 7568 ~ 0.2,
                              QA_PIXEL == 7696 ~ 0.2,
                              QA_PIXEL == 7698 ~ 0.2,
                              QA_PIXEL == 7824 ~ 0.2,
                              QA_PIXEL == 7960 ~ 0.2,
                              QA_PIXEL == 8088 ~ 0.2,
                              QA_PIXEL == 13600 ~ 0.2,
                              QA_PIXEL == 13602 ~ 0.2,
                              QA_PIXEL == 13664 ~ 0.2,
                              QA_PIXEL == 13856 ~ 0.2,
                              QA_PIXEL == 13858 ~ 0.2))
  assign(paste0(i,'_csv_pre2013'),full)
}



# read in NDVI & QA data from Landsat 8 (2013 - 2020), downloaded from GEE
# asignment of weights is based on docs from phenofit and USGS
for(i in combo_List){
  ndvi = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT8_NDVI/",i,'.csv'))
  qa = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT8_QA/",i,'.csv'))
  full = ndvi %>% left_join(qa,join_by('DATE_ACQUIRED' == 'DATE_ACQUIRED'))
  full = full %>%
    mutate(Weight = case_when(QA_PIXEL == 1 ~ 0,
                              QA_PIXEL == 21762 ~ 0.5,
                              QA_PIXEL == 21824 ~ 1,
                              QA_PIXEL == 21826 ~ 0.5,
                              QA_PIXEL == 21888 ~ 1,
                              QA_PIXEL == 21890 ~ 0.5,
                              QA_PIXEL == 21952 ~ 1,
                              QA_PIXEL == 22018 ~ 0.5,
                              QA_PIXEL == 22080 ~ 0.5,
                              QA_PIXEL == 22144 ~ 0.5,
                              QA_PIXEL == 22280 ~ 0.2,
                              QA_PIXEL == 23826 ~ 0.2,
                              QA_PIXEL == 23888 ~ 0.2,
                              QA_PIXEL == 23952 ~ 0.2,
                              QA_PIXEL == 24082 ~ 0.2,
                              QA_PIXEL == 24088 ~ 0.2,
                              QA_PIXEL == 24144 ~ 0.2,
                              QA_PIXEL == 24216 ~ 0.2,
                              QA_PIXEL == 24344 ~ 0.2,
                              QA_PIXEL == 24472 ~ 0.2,
                              QA_PIXEL == 29986 ~ 0.2,
                              QA_PIXEL == 30048 ~ 0.2,
                              QA_PIXEL == 30242 ~ 0.2,
                              QA_PIXEL == 30304 ~ 0.2,
                              QA_PIXEL == 54534 ~ 0.5,
                              QA_PIXEL == 54596 ~ 1,
                              QA_PIXEL == 54790 ~ 0.5,
                              QA_PIXEL == 54852 ~ 0.5,
                              QA_PIXEL == 55052 ~ 0.2,
                              QA_PIXEL == 56598 ~ 0.2,
                              QA_PIXEL == 56660 ~ 0.2,
                              QA_PIXEL == 56854 ~ 0.2,
                              QA_PIXEL == 56916 ~ 0.2,
                              QA_PIXEL == 62758 ~ 0.2,
                              QA_PIXEL == 62820 ~ 0.2,
                              QA_PIXEL == 63014 ~ 0.2,
                              QA_PIXEL == 63076 ~ 0.2))
  assign(paste0(i,'_csv_post2013'),full)
}

# create empty data frame and then fill it with data for each pasture combined from both landsat datasets
all.landsat = data.frame(DATE_ACQUIRED = as.character(),NDVI = as.numeric(),QA_PIXEL = as.numeric(),combo_NO = as.character(),Weight = as.numeric())
for(i in combo_List){
  pre = get(paste0(i,'_csv_pre2013'))
  pre$DATE_ACQUIRED = as.Date(pre$DATE_ACQUIRED,format = '%Y-%m-%d')
  pre = pre %>% filter(DATE_ACQUIRED <= '2013-03-01')
  pre$DATE_ACQUIRED = as.character(pre$DATE_ACQUIRED)
  post = get(paste0(i,'_csv_post2013'))
  new = rbind(pre,post)
  new$combo_NO = i
  all.landsat = rbind(all.landsat,new)
}

rm(list = ls()[grep("*2013", ls())])


# Now run phenofit on the data for each site ##


pheno_ready = all.landsat[,c('DATE_ACQUIRED','NDVI','Weight','combo_NO')]
# convert date column to appropriate type
pheno_ready$DATE_ACQUIRED = as.Date(pheno_ready$DATE_ACQUIRED, format = '%Y-%m-%d')
# negative NDVI values are impossible, convert them to 0
pheno_ready$NDVI = ifelse(pheno_ready$NDVI < 0, 0, pheno_ready$NDVI)

pheno_ready = pheno_ready %>% rename(
  t = DATE_ACQUIRED,
  y = NDVI,
  w = Weight
)

# set paremters for phenofit (largely based on recomendations from phenofit package)
nptperyear     <- 22 # for a 16-day return interval
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM 
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu") # all 5 methods for fine fitting

# loop through each pasture and perform the phenofit procedure
for(i in combo_List){
  filtered.pheno_ready = pheno_ready %>% filter(combo_NO == i)
  # there as some pastures that had too little of riparian areas to use landsat, skip them
  if(all(is.na(filtered.pheno_ready$y))){skip = 'skip'}
  else{
  # even though we didn't filter for quality, a few NA's exist, approximate them
  filtered.pheno_ready$y = na.approx(filtered.pheno_ready$y,na.rm=F)
  # if we approximated the NDVI (y), give a low weight (w)
  filtered.pheno_ready$w = ifelse(is.na(filtered.pheno_ready$w),0.2,filtered.pheno_ready$w)
  # t = time, y = ndvi, w = weight
  filtered.pheno_ready = filtered.pheno_ready[,c('t','y','w')]
  # built in data check for phenofit
  INPUT <- check_input(filtered.pheno_ready$t, filtered.pheno_ready$y, filtered.pheno_ready$w,
                       nptperyear = nptperyear,
                       maxgap = nptperyear / 4, wmin = 0)
  # rough fitting, weighted whitiker determines best input value
  brks <- season_mov(INPUT,
                     list(FUN = "smooth_wWHIT", wFUN = wFUN,
                          maxExtendMonth = 3,
                          wmin = wmin, r_min = 0.1))
  # use all five methods for the fine fitting
  fit <- curvefits(INPUT, brks,
                   list(
                     methods = methods_fine, 
                     wFUN = wFUN,
                     iters = 2,
                     wmin = wmin,
                     nextend = 2,
                     maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                     minPercValid = minPercValid))
  l_param <- get_param(fit)
  dfit <- get_fitting(fit)
  # use a 20% threshold to determine start and end of growing season
  TRS <- 0.5
  l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE)
  # create empty dataframe to store fitted curve values (average of all 5 methods)
  fitted.curve = data.frame(date = as.Date(character()),NDVI = as.numeric())
  for(j in 2000:2020){
    step.one = fit[[paste0(j,'_1')]]
    model.one = step.one$model$AG$zs$iter2
    model.two = step.one$model$Zhang$zs$iter2
    model.three = step.one$model$Beck$zs$iter2
    model.four = step.one$model$Elmore$zs$iter2
    model.five = step.one$model$Gu$zs$iter2
    pre.mean = data.frame(curve1 = model.one,curve2 = model.two,curve3 = model.three,curve4 = model.four,curve5 = model.five)
    post.mean = data.frame(date = step.one$tout,NDVI = rowMeans(pre.mean))
    post.mean$date = as.Date(post.mean$date - 1, origin = '2000-01-01')
    fitted.curve = rbind(fitted.curve,post.mean)
  }
  fitted.curve$combo_NO = i
  # store the fitted curves in dataframes for each pasture
  assign(paste0('fitted.curve_',i),fitted.curve)
  
  pheno <- l_pheno$doy %>% melt_list('meth')
  plot_season(INPUT, brks, ylab = "NDVI")
  pheno$combo_NO = i
  # store the sos,pos, and eos data in dataframes for each pasture
  assign(paste0('pheno_',i),pheno)}
  
}

# there is a lot of data, so remove some to just free up memory
rm(fitted.curve)
rm(pheno)
rm(pheno_ready)

# put fitted curves and pheno data into a single dataframe with unique identifier as a column
all.fits = grep('^fitted.curve', ls(), value = TRUE) |>
  mget() |>
  bind_rows()

all.pheno = grep('^pheno', ls(), value = TRUE) |>
  mget() |>
  bind_rows()

# make sure the lengths are equivalent to number of pastures with non-NA riparian data (243)
#length(unique(all.fits$combo_NO)) #243
#length(unique(all.pheno$combo_NO)) #243


write.csv(all.fits,"Data/phenofit_output/all_fits.csv")
write.csv(all.pheno,"Data/phenofit_output/all_pheno.csv")


###########################################################################################################################
## now we're actually going to do some analysis of the phenofit output data
library(sf)
library(dplyr)
library(stringr)
library(units)
library(lme4)
library(lubridate)
library(emmeans)
library(ggplot2)

# read in the pasture data
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")

# calculate area in acres and create a unique identifier for each allotment/pasture combo
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$area = drop_units(st_area(pastsSF))
pastsDF$area = pastsDF$area * 0.000247105
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)


combo_List = pastsDF$combo_NO

# read in 'sample size' data, this tells us how many pixels were classified as riparian for each pasture 
# using out object oriented classification in GEE
ls_sampleSize = data.frame(combo_NO = as.character(), size_pixels = as.numeric())
for(i in combo_List){
  temp = read.csv(paste0("Data/SFO_riparian_sampleSize/",i,'.csv'))
  temp$combo_NO = i
  temp = temp %>% rename(size_pixels = SR_B1)
  ls_sampleSize = rbind(ls_sampleSize,temp)
}

# we removed any pastures with less than 10 pixels of riparian (< .01 square kilometers)
ls_sampleSize = ls_sampleSize %>% filter(size_pixels >= 10)
sites_to_keep = c(ls_sampleSize$combo_NO)

# read in fitted curve, pheno dates, and treatment data
fitted.curve.all = read.csv("Data/phenofit_output/all_fits.csv")
pheno.all = read.csv("Data/phenofit_output/all_pheno.csv")
treatments = read.csv('Data/SFO_allTreatments.csv')

treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)

########### there are some inconsistencies between treatment csv and the pasts df that need to be corrected so that we use all 
## available data
# Baldy Seding -> Baldy Seeding
# Middle (Turner) -> Middle
# Allotment Big Spring -> Big Springs
# Pasture Big Springs -> Big Spring
# SE Flat -> Flat
# WSA (Shears Creek) -> Shears Creek
# Bird Creek Unit -> Bird Creek
# Bob Moore Unit -> Bob Moore Creek
# Deriar Unit -> Deriar Creek
# Fenced Pasture -> Fenced
# Bolton -> Bolton D
# Gary Creek -> Gary Creek (E)
# Geertson Creek -> Geertson Creek (B)
# Kirtley Creek -> Kirtley Creek (C)
# Geertson Creek Allotment Seeding -> Seeding (A)
# Ryegrass BLM -> Ryegrass
# South Shearing Pen -> Shearing Pen
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Baldy Seding','Baldy Seeding')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Middle \\(Turner\\)','Middle')
treatments$ALLOT_NAME = str_replace(treatments$ALLOT_NAME,'Big Spring','Big Springs')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Big Springs','Big Spring')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'SE Flat','Flat')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'WSA \\(Shears Creek\\)','Shears Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bird Creek Unit','Bird Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bob Moore Unit','Bob Moore Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Deriar Unit','Deriar Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Fenced Pasture','Fenced')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bolton','Bolton D')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Gary Creek','Gary Creek (E)')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Geertson Creek','Geertson Creek (B)')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Kirtley Creek','Kirtley Creek (C)')
treatments[treatments$ALLOT_NAME == 'Geertson Creek' & treatments$PAST_NAME == 'Seeding',2] = 'Seeding (A)'
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Ryegrass BLM','Ryegrass')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'South Shearing Pen','Shearing Pen')

# join treatments to the pasture data and fix a spelling mistake
treatments = treatments %>% left_join(pastsDF,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))
treatments = treatments %>% dplyr::select(Treatment,Start,combo_NO,area)
treatments$Treatment = ifelse(treatments$Treatment == 'Early ','Early',treatments$Treatment)

pheno.sites = unique(pheno.all$combo_NO)

# since there are 5 curve fitting procedures used, theres five of each year, so we need to just break that and get a single year
splitup = function(x){
  z = as.numeric(str_split(x,'_',simplify = T)[1,1])
  return(z)
}

# loop through each pastures info and get the average for the 5 fine curve fitting procedures
summarized.pheno.all = data.frame(year = as.numeric(),mean.pos = as.numeric(),mean.sos = as.numeric(),mean.eos = as.numeric(),combo_NO = as.character())
for(i in pheno.sites){
  pheno.singlesite = pheno.all %>% filter(combo_NO == i)
  summarized.singlesite = pheno.singlesite %>% 
    group_by(flag) %>%
    summarise(mean.pos = round(mean(DER.pos)),
              mean.sos = round(mean(TRS5.sos)),
              mean.eos = round(mean(TRS5.eos)))
  summarized.singlesite$flag = sapply(summarized.singlesite$flag,splitup,USE.NAMES = F)
  summarized.singlesite = summarized.singlesite %>% rename(year = flag)
  summarized.singlesite = summarized.singlesite[complete.cases(summarized.singlesite),]
  for(j in 2000:2020){
    if(nrow(summarized.singlesite %>% filter(year == j)) == 0){
      summarized.singlesite = summarized.singlesite %>% 
        add_row(year = j,mean.pos=NA,mean.sos=NA,mean.eos=NA)
    }
  }
  summarized.singlesite$combo_NO = i
  
  summarized.pheno.all = rbind(summarized.pheno.all,summarized.singlesite)
}


# this site had too many poor values for the fitted curve process to work, so it had to be dropped
summarized.pheno.all = summarized.pheno.all %>% filter(combo_NO != '06310_02')

# join treatment to pheno data
summarized.pheno.all = summarized.pheno.all  %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
summarized.pheno.all = summarized.pheno.all[summarized.pheno.all$year >= summarized.pheno.all$Start,]

# create a dataframe with just averages across our timeframe for each metric
average.summarized.pheno.all = summarized.pheno.all %>%
  group_by(combo_NO) %>%
  summarise(
    pos = mean(mean.pos,na.rm = T),
    sos = mean(mean.sos,na.rm = T),
    eos = mean(mean.eos,na.rm = T),
    area = mean(area,na.rm = T),
    Treatment = first(Treatment)
  ) 


### so we have cleaned up the data pretty well but we need to bring in climate data
## see SFO_climate_download.R for how these csv's were created

#read in phenologically relevant climate variables
all.climate = read.csv("Data/all_pheno_climate.csv")
all.climate = all.climate %>% rename(ALLOT_NAME = allot_name)

# we could only go as fine as allotments so join climate data to allotments, repeating for each pasture
all.climate = merge(all.climate,pastsDF,by='ALLOT_NAME')
all.climate = all.climate %>% dplyr::select(year,annual.PPT_mm,annual.Tmax_C,annual.Tmin_C,elevation_m,combo_NO)
all.climate = all.climate %>% rename(PPT_mm = annual.PPT_mm,Tmax_C = annual.Tmax_C,Tmin_C = annual.Tmin_C)
temp = summarized.pheno.all %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
temp = temp[complete.cases(temp),]
# length(unique(temp$combo_NO)) ## 125 sites available for this analysis

# calculate annual average temperature
all.climate$Tavg_C = (all.climate$Tmax_C+all.climate$Tmin_C)/2

# calculate average annual measures for each pasture, this actually never gets used
all.climate.annual = all.climate %>% group_by(combo_NO) %>%
  summarise(PPT_mm = mean(PPT_mm),
            Tmax_C = mean(Tmax_C),
            Tmin_C = mean(Tmin_C),
            Tavg_C = mean(Tavg_C),
            elevation_m = first(elevation_m))

temp2 = average.summarized.pheno.all %>% left_join(all.climate.annual,join_by(combo_NO == combo_NO))
temp2 = temp2[complete.cases(temp2),]
#length(unique(temp2$combo_NO)) ## 125 sites available 

# combine pheno and climate data, remove incomplete years
summarized.pheno.climate = summarized.pheno.all %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
summarized.pheno.climate = summarized.pheno.climate[complete.cases(summarized.pheno.climate),]

# combine average annual values for climate and pheno, remove incomplete years
average.summarized.climate = average.summarized.pheno.all %>% left_join(all.climate.annual,join_by(combo_NO == combo_NO))
average.summarized.climate = average.summarized.climate[complete.cases(average.summarized.climate),]

######################################################################
# we now need to get our daily fitted ndvi curves to extract integrals and peak info
fitted.curve.all = fitted.curve.all %>%
  dplyr::select(date,NDVI,combo_NO) %>%
  mutate(year = year(date)) %>%
  mutate(doy = yday(date))

fitted.curve.all = fitted.curve.all %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
fitted.curve.all = fitted.curve.all[fitted.curve.all$year >= fitted.curve.all$Start,]

# average start/end of season for all years and all sites, for the gs integral
avg.sos = round(mean(average.summarized.climate$sos)) #106
avg.eos = round(mean(average.summarized.climate$eos)) #312
poses = summarized.pheno.climate %>% dplyr::select(combo_NO,mean.pos,year)

fitted.curve.all = fitted.curve.all %>% left_join(poses,join_by(combo_NO == combo_NO,year==year))

# function to find the growing season integral
getInt = function(x,start,end){
  limit = x[x$doy >= start & x$doy <= end,]
  total = sum(limit$NDVI,na.rm = T)
  return(total)
}

fitted.curve.all$NDVI = ifelse(fitted.curve.all$NDVI > 1,fitted.curve.all$NDVI/100,fitted.curve.all$NDVI)

# calculate the NDVI variables that we are interested in
fitted.response.variables = fitted.curve.all %>%
  group_by(combo_NO,year) %>%
  reframe(
    gs_integral = sum(NDVI[doy >= avg.sos & doy <= avg.eos]),
    annual_integral = sum(NDVI),
    peak_NDVI = NDVI[doy == mean.pos],
    area = first(area),
    Treatment = first(Treatment)
  )

fitted.response.variables = fitted.response.variables[complete.cases(fitted.response.variables),]
# join climate data to our NDVI measures
fitted.response.climate = fitted.response.variables %>%
  left_join(all.climate,join_by(combo_NO==combo_NO,year==year))

# remove sites that were too small
fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

# read in actual use
actual = read.csv('Data/Salmon_actualUse.csv')

# some pastures have multiple leases for each year, sum up the aums
actual.summarised = actual %>%
  group_by(Allotment,Pasture,Year) %>%
  summarise(actual.AUM = sum(AUM))

actual.summarised = actual.summarised %>%
  filter(Pasture != 'Summary')
# Middle (Turner) needs to be Middle
# SE Flat needs to be Flat
# WSA (Shears Creek) -> Shears Creek
# Bird Creek Unit -> Bird Creek
# Bob Moore Unit -> Bob Moore Creek
# Deriar Unit -> Deriar Creek
# Bolton (D) -> D Bolton which will need to be combined with the existing D Bolton
# Gary Creek -> Gary Creek (E) and combine with existing Gary Creek (E)
# There is also a Gary Creek (E) with two spaces to be fixed
# Kirtley Creek -> Kirtley Creek (C)
# Bear Creek/McNutt -> Bear Creek
# Ryegrass BLM -> Ryegrass

actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Middle \\(Turner\\)','Middle')
actual.summarised$Allotment = str_replace(actual.summarised$Allotment,'Big Spring','Big Springs')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Big Springs','Big Spring')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'SE Flat','Flat')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'WSA \\(Shears Creek\\)','Shears Creek')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Bird Creek Unit','Bird Creek')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Bob Moore Unit','Bob Moore Creek')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Deriar Unit','Deriar Creek')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Bolton \\(D\\)','D Bolton')
actual.summarised[actual.summarised$Pasture == 'Gary Creek',2] = 'Gary Creek (E)'
actual.summarised[actual.summarised$Pasture == 'Gary Creek  (E)',2] = 'Gary Creek (E)'
actual.summarised[actual.summarised$Pasture == 'Kirtley Creek',2] = 'Kirtley Creek (C)'
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Ryegrass BLM','Ryegrass')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'South Shearing Pen','Shearing Pen')
actual.summarised$Pasture = str_replace(actual.summarised$Pasture,'Bear Creek\\/McNutt','Bear Creek')

# we need to assign combo number to this df based on allotment/pasture name
actual.use = pastsDF %>% left_join(actual.summarised,join_by(ALLOT_NAME==Allotment,PAST_NAME==Pasture))

# however, we cant just use AUM, we need to use AUM/acre
actual.use = actual.use %>% dplyr::select(combo_NO,Year,actual.AUM)
actual.use = as.data.frame(actual.use)

# join actual use to the current working data
fitted.response.climate = fitted.response.climate %>% left_join(actual.use,join_by(combo_NO == combo_NO,year==Year))
# calculate aum per acre
fitted.response.climate$aum.per.acre = fitted.response.climate$actual.AUM/fitted.response.climate$area
fitted.response.climate = fitted.response.climate[complete.cases(fitted.response.climate),]

# too few samples to keep these treatments
fitted.response.climate = fitted.response.climate %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')

fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -gs_integral), y = gs_integral)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

mean(fitted.response.climate$gs_integral) # 28.9
sd(fitted.response.climate$gs_integral) # 6.6
mean(fitted.response.climate$annual_integral) # 41.4
sd(fitted.response.climate$annual_integral) # 10.6
mean(fitted.response.climate$peak_NDVI) # 0.22
sd(fitted.response.climate$peak_NDVI) # 0.05
length(unique(fitted.response.climate$combo_NO)) # 106
#### analysis of growing season integral first ###

# base model needs to include year, site, and aum/acre
m2.lmer = lmer(gs_integral ~ aum.per.acre + year + (1|combo_NO),REML=F,data=fitted.response.climate)

# first look at annual average temperature
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is the variable we most care about here
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.35

# lets look at the model with just our significant factors
m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + year + aum.per.acre + (1|combo_NO),data=fitted.response.climate)

# now add in treatment, just to double check that it is still insignificant
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.38

# assess the interaction between treatment and temperature
m2.lmer = update(m1.lmer, .~.+Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') #0.36

# assess the interaction between treatment and precip
m2.lmer = update(m1.lmer, .~.+Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') #0.48

# no significant improvements for adding treatment in anyway, analysis of growing season integral is over

# so lets look at the interaction
#m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + year + (1|combo_NO),data=fitted.response.climate)
#confint(m1.lmer) # all interaction terms cross 0
#car::Anova(m1.lmer,type='II') # everything except just treatment is significant
#emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
#pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences
#plot(emmeans(m1.lmer, ~ Treatment*Tavg_C))
### overall, theres not a lot that can be said about only a significant result from the interaction....
#(RG4 <- ref_grid(m1.lmer))
#emmip(m1.lmer, Treatment~Tavg_C,style='factor',at = list(Tavg_C = 2:9))
#plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots
#plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
#     adj = -0.3, pch = 20, col = "gray40")
#summary(m1.lmer)

###### second variable -> annual integral (annual_integral) ########################

# a double check that we are only using sites with large enough riparian areas
fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -annual_integral), y = annual_integral)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

# base model should include year, site, and aum/acre
m2.lmer = lmer(annual_integral ~ year + aum.per.acre + (1|combo_NO), REML = F, data = fitted.response.climate)

# first temperature
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001


# next precipitation
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is our variable of interest
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.66

# create a model with our significant variables
m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + year + aum.per.acre + (1|combo_NO),REML = F,data = fitted.response.climate)
#confint(m1.lmer) # 
#car::Anova(m1.lmer,type='II')

# add in treatment just to make sure there is no significance still
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.67

# assess a possible interaction between temperature and treatment
m2.lmer = update(m1.lmer, .~.+ Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.55

# assess a possible interaction between temperature and precipitation
m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.31

# based on our analysis, there appears to be no significant impact of treatment (or an interaction)
# on annual integral

#m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + year + (1|combo_NO),REML = F,data = fitted.response.climate) 
#confint(m1.lmer) #only summer.late.rotation doesn't include 0
#car::Anova(m1.lmer,type='II') 
#emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
#pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences
#plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
#     adj = -0.3, pch = 20, col = "gray40")
#summary(m1.lmer)

###### third variable -> peak NDVI (peak_NDVI) ########################

# a double check that we are only using sites with large enough riparian areas
fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -peak_NDVI), y = peak_NDVI)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2


# base model must include year, site, and aum/acre
m2.lmer = lmer(peak_NDVI ~ year + aum.per.acre + (1|combo_NO), REML = F, data = fitted.response.climate)

# first temperature
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.17

# next precipitation
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is our main variable of interest here
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.34

# create a model with our significant variables
m1.lmer = lmer(peak_NDVI ~ PPT_mm + aum.per.acre + year + (1|combo_NO),REML = F,data = fitted.response.climate)
#confint(m1.lmer)
#car::Anova(m1.lmer,type='II')

# add treatment to the model just to make sure it is not significant
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.37

# check for interaction between treatment and precipitation
m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.38

# based on our analysis there is no significant impact of treatment (or interaciton)
# on peak NDVI values

#plot(m1.lmer)
#qqnorm(resid(m1.lmer))

#### take aways##################
# no impact of treatment on any of our 'productivity' variables

########################################################################################################
# now we are moving from productivity metrics to phenology metrics ##

# read in climate data
all.climate2 = read.csv("Data/all_pheno_climate.csv")

all.climate2 = all.climate2 %>% rename(ALLOT_NAME = allot_name)
# merge climate data with pastures
all.climate.pheno = merge(all.climate2,pastsDF,by='ALLOT_NAME')
all.climate.pheno = all.climate.pheno %>% rename(late.season.PPT_mm = late.seaon.PPT_mm)
all.climate.pheno = all.climate.pheno %>% dplyr::select(year,spring.Tmax_C,spring.Tmin_C,spring.PPT_mm,summer.Tmax_C,summer.Tmin_C,summer.PPT_mm,fall.Tmax_C,fall.Tmin_C,fall.PPT_mm,early.season.Tmax_C,early.season.Tmin_C,early.season.PPT_mm,mid.season.Tmax_C,mid.season.Tmin_C,mid.season.PPT_mm,late.season.Tmax_C,late.season.Tmin_C,late.season.PPT_mm,annual.Tmax_C,annual.Tmin_C,annual.PPT_mm,elevation_m,combo_NO)
# join climate variables to pheno measures
pheno.variables = summarized.pheno.all %>% left_join(all.climate.pheno,join_by(year==year,combo_NO==combo_NO))
grazing = fitted.response.climate %>% dplyr::select(combo_NO,year,aum.per.acre)
# add use data to pheno measures
pheno.variables = pheno.variables %>% left_join(grazing,join_by(combo_NO == combo_NO,year==year))
pheno.variables = pheno.variables[complete.cases(pheno.variables),]

# only keep sites that have large enough riparian areas
pheno.variables = pheno.variables[pheno.variables$combo_NO %in% sites_to_keep,]
# not enough samples
pheno.variables = pheno.variables %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')

# calculate temperature averages for our three seasons
pheno.variables$spring.Tavg_C = (pheno.variables$spring.Tmax_C + pheno.variables$spring.Tmin_C)/2
pheno.variables$summer.Tavg_C = (pheno.variables$summer.Tmax_C + pheno.variables$summer.Tmin_C)/2
pheno.variables$fall.Tavg_C = (pheno.variables$fall.Tmax_C + pheno.variables$fall.Tmin_C)/2
pheno.variables = pheno.variables %>% filter(mean.sos >= 60)

#################################### start with peak of season (mean.pos) #######################
pheno.variables = pheno.variables[pheno.variables$combo_NO %in% sites_to_keep,]
mean(pheno.variables$mean.sos) # 128
mean(pheno.variables$mean.pos) # 189
mean(pheno.variables$mean.eos) # 282

# base model must include year, site, and aum/acre
m2.lmer = lmer(mean.pos ~ year + aum.per.acre + (1|combo_NO), REML = F, data = pheno.variables)

# first spring temperature
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# next summer temperature
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# now spring precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# now summer precipi
m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm) 
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is our variable of interest
mtest = lmer(mean.pos ~ Treatment + aum.per.acre + year + (1|combo_NO),REML = F, data = pheno.variables)
anova(m2.lmer,mtest)# p = 0.13

# create a model with our significant variables
m1.lmer = lmer(mean.pos ~ aum.per.acre + year + spring.Tavg_C + spring.PPT_mm + summer.Tavg_C + summer.PPT_mm + (1|combo_NO), REML = F, data = pheno.variables)

# add treatment just to make sure there is nothing significant
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.0023
confint(m2.lmer) # summer is entirely negative
car::Anova(m2.lmer,type='II') #treatment is allegedly important
emmeans(m2.lmer, ~ Treatment) # summer does not overlap
pairs(emmeans(m2.lmer, ~ Treatment)) # early and summer significantly different, continuous and summer significantly different 
plot(emmeans(m2.lmer, ~ Treatment),xlab='Estimated Mean Peak of Season DOY',color='grey10') + 
  coord_flip()+
  theme_bw()+
  theme(axis.text.y = element_text(colour = "black", size = 18, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 22, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))+
  annotate('text',x=210,y = 'Continuous',label='bold(a)',parse=T,size=10)+
  annotate('text',x =199, y = 'Early',label='bold(a)',parse=T,size=10)+
  annotate('text',x=199,y='Late',label='bold(ab)',parse=T,size=10)+
  annotate('text',x=188,y='Summer',label='bold(b)',parse=T,size=10)

summary(m2.lmer)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# combo_NO (Intercept) 136.3    11.67   
# Residual             594.6    24.38   
# Number of obs: 1636, groups:  combo_NO, 106
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                   723.89259  271.76788   2.664
# year                           -0.25014    0.13523  -1.850
# spring.Tavg_C                  -3.35158    0.53363  -6.281
# spring.PPT_mm                   0.05725    0.01300   4.404
# summer.Tavg_C                  -1.33256    0.79989  -1.666
# summer.PPT_mm                   0.12758    0.02953   4.320
# TreatmentEarly                 -1.21272    4.34731  -0.279
# TreatmentEarly.Late.Roatation  -0.89598   10.36954  -0.086
# TreatmentExclosure              0.10005    8.53947   0.012
# TreatmentLate                  -8.39900    5.90628  -1.422
# TreatmentRest.Rotation          0.60832   14.10297   0.043
# TreatmentSummer               -14.04337    4.50689  -3.116
# TreatmentSummer.Late.Rotation  -6.86698   10.50365  -0.654

# add treatment just to make sure there is nothing significant
m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.0023
########################################################################################
# next with start of season (mean.sos)

# base model must include year, site, and aum/acre
m2.lmer = lmer(mean.sos ~ aum.per.acre + year + (1|combo_NO), REML = F, data = pheno.variables)

# first spring temperature
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# going to skip summer since start of season is before summer

# now spring precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

#treatment is our variable of interest
mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.65

# create a model with our significant variables
m1.lmer = lmer(mean.sos ~ year + spring.Tavg_C + spring.PPT_mm + aum.per.acre + (1|combo_NO) , REML = F, data = pheno.variables)
# add treatment just to make sure there is no significance
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.1

# check for interaction with spring temperature
m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.75

# check for interaction with spring precip
m2.lmer = update(m1.lmer, .~.+ Treatment*spring.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.53

# based on our analysis we found no signficant impact of treatment (or interaction)
# on start of season date

##########################################
# next with end of season (mean.eos)

# base model must include year, site, and aum/acre
m2.lmer = lmer(mean.eos ~ year + aum.per.acre + (1|combo_NO), REML = F, data = pheno.variables)

# first spring temperature
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# summer temperature
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.24

# fall temperature
m3.lmer = update(m2.lmer, .~.+ fall.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# now spring precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.63

# summer precip
m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.79

# fall precip
m3.lmer = update(m2.lmer, .~.+ fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.01

# treatment is our variable of interest
mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.88

# create a model with our significant variables
m1.lmer = lmer(mean.eos ~ year + spring.Tavg_C + fall.Tavg_C + fall.PPT_mm + aum.per.acre + (1|combo_NO), REML = F, data = pheno.variables)

# lets add treatment just to make sure there is no significance
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.21

# look for interaction with spring temperature
m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.75

# look for interaction with fall temperature
m2.lmer = update(m1.lmer, .~.+ Treatment*fall.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.56

# look for interaction with summer precip
m2.lmer = update(m1.lmer, .~.+ Treatment*summer.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.63

# based on our analysis it would seem there is no impact of treatment (or interaction)
# on end of season date

##########################################################################
# length of season

# calculate length of season
pheno.variables$mean.los = pheno.variables$mean.eos - pheno.variables$mean.sos
pheno.variables = pheno.variables[pheno.variables$mean.los < 300,]

# base model must include year, site, and aum/acre
m2.lmer = lmer(mean.los ~ year + aum.per.acre + (1|combo_NO), REML = F, data = pheno.variables)

# first spring temp
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# summer temperature
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #p < 0.001

# fall temperature
m3.lmer = update(m2.lmer, .~.+ fall.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.14

# now spring precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.49

# summer precip
m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.45

# fall precip
m3.lmer = update(m2.lmer, .~.+ fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is our variable of interest
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.75

# create a model with our significant variables
m1.lmer = lmer(mean.los ~ year + spring.Tavg_C + summer.Tavg_C + fall.PPT_mm + (1|combo_NO), data = pheno.variables)

# add treatment just to make sure
mtest = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,mtest)# p = 0.62

# check for interaction with spring temp
mtest = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,mtest)# p = 0.85

# check for interaction with summer temp
mtest = update(m1.lmer, .~.+ Treatment*summer.Tavg_C)
anova(m1.lmer,mtest)# p = 0.27

# check for interaction with fall precip
mtest = update(m1.lmer, .~.+ Treatment*fall.PPT_mm)
anova(m1.lmer,mtest)# p = 0.88

# check for interaction with spring temp
mtest = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,mtest)# p = 0.85
# based on our analysis, treatment (or interaction) has no significant impact on length of season

####################################################################
# moving to trend analysis of NDVI data with the bfast package
library(bfast)
library(lubridate)
library(xts)
library(dplyr)
library(sf)

# read in the NDVI fitted curves for all pastures
fitted.curves = read.csv("Data/phenofit_output/all_fits.csv")

# reframe the fitted curve data
fitted.curves = fitted.curves %>%
  dplyr::select(date,NDVI,combo_NO) %>%
  mutate(year = year(date)) %>%
  mutate(doy = yday(date))

sites = unique(fitted.curves$combo_NO)

test_site = fitted.curves %>% filter(combo_NO == sites[1])

test_ts = bfastts(as.vector(test_site$NDVI),test_site$date,type = 'irregular')
test_ts

test_ts = na.approx(test_ts)

bfm2 <- bfast(test_ts, h = 365/length(test_ts), decomp = 'stl',
              season = "harmonic", breaks = 'LWZ', max.iter = 2)
plot(bfm2)
bfm2$Magnitude
bfm2$Time

bfm2$output[[1]]$Tt




#################################################
# bfast operates best with an 8-day return interval (not daily) 
# instead of using the original NDVI values and interpolating NA's
# we are going to use the fine fitted curve data but only on the original dates of the landsat imagery
# so first, read in the landsat data to get the dates

# read in pasture data
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])

# create a unique identifier for each allotment/pasture combo
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)

combo_List = pastsDF$combo_NO

# get all of the landsat 7 imagery
for(i in combo_List){
  ndvi = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT_NDVI/",i,'.csv'))
  qa = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT_QA/",i,'.csv'))
  full = ndvi %>% left_join(qa,join_by('DATE_ACQUIRED' == 'DATE_ACQUIRED'))
  full = full %>%
    mutate(Weight = case_when(QA_PIXEL == 1 ~ 0,
                              QA_PIXEL == 5440 ~ 1,
                              QA_PIXEL == 5442 ~ 0.5,
                              QA_PIXEL == 5504 ~ 1,
                              QA_PIXEL == 5506 ~ 0.5,
                              QA_PIXEL == 5696 ~ 0.5,
                              QA_PIXEL == 5698 ~ 0.5,
                              QA_PIXEL == 5760 ~ 0.5,
                              QA_PIXEL == 5896 ~ 0.2,
                              QA_PIXEL == 7440 ~ 0.2,
                              QA_PIXEL == 7442 ~ 0.2,
                              QA_PIXEL == 7568 ~ 0.2,
                              QA_PIXEL == 7696 ~ 0.2,
                              QA_PIXEL == 7698 ~ 0.2,
                              QA_PIXEL == 7824 ~ 0.2,
                              QA_PIXEL == 7960 ~ 0.2,
                              QA_PIXEL == 8088 ~ 0.2,
                              QA_PIXEL == 13600 ~ 0.2,
                              QA_PIXEL == 13602 ~ 0.2,
                              QA_PIXEL == 13664 ~ 0.2,
                              QA_PIXEL == 13856 ~ 0.2,
                              QA_PIXEL == 13858 ~ 0.2))
  assign(paste0(i,'_csv_pre2013'),full)
}



# read in the landsat 8 data
for(i in combo_List){
  ndvi = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT8_NDVI/",i,'.csv'))
  qa = read.csv(paste0("Data/GEE_LandsatData/SFO_LANDSAT8_QA/",i,'.csv'))
  full = ndvi %>% left_join(qa,join_by('DATE_ACQUIRED' == 'DATE_ACQUIRED'))
  full = full %>%
    mutate(Weight = case_when(QA_PIXEL == 1 ~ 0,
                              QA_PIXEL == 21762 ~ 0.5,
                              QA_PIXEL == 21824 ~ 1,
                              QA_PIXEL == 21826 ~ 0.5,
                              QA_PIXEL == 21888 ~ 1,
                              QA_PIXEL == 21890 ~ 0.5,
                              QA_PIXEL == 21952 ~ 1,
                              QA_PIXEL == 22018 ~ 0.5,
                              QA_PIXEL == 22080 ~ 0.5,
                              QA_PIXEL == 22144 ~ 0.5,
                              QA_PIXEL == 22280 ~ 0.2,
                              QA_PIXEL == 23826 ~ 0.2,
                              QA_PIXEL == 23888 ~ 0.2,
                              QA_PIXEL == 23952 ~ 0.2,
                              QA_PIXEL == 24082 ~ 0.2,
                              QA_PIXEL == 24088 ~ 0.2,
                              QA_PIXEL == 24144 ~ 0.2,
                              QA_PIXEL == 24216 ~ 0.2,
                              QA_PIXEL == 24344 ~ 0.2,
                              QA_PIXEL == 24472 ~ 0.2,
                              QA_PIXEL == 29986 ~ 0.2,
                              QA_PIXEL == 30048 ~ 0.2,
                              QA_PIXEL == 30242 ~ 0.2,
                              QA_PIXEL == 30304 ~ 0.2,
                              QA_PIXEL == 54534 ~ 0.5,
                              QA_PIXEL == 54596 ~ 1,
                              QA_PIXEL == 54790 ~ 0.5,
                              QA_PIXEL == 54852 ~ 0.5,
                              QA_PIXEL == 55052 ~ 0.2,
                              QA_PIXEL == 56598 ~ 0.2,
                              QA_PIXEL == 56660 ~ 0.2,
                              QA_PIXEL == 56854 ~ 0.2,
                              QA_PIXEL == 56916 ~ 0.2,
                              QA_PIXEL == 62758 ~ 0.2,
                              QA_PIXEL == 62820 ~ 0.2,
                              QA_PIXEL == 63014 ~ 0.2,
                              QA_PIXEL == 63076 ~ 0.2))
  assign(paste0(i,'_csv_post2013'),full)
}

# create an empty dataframe then loop through the landsat data and compile it into a singleframe
all.landsat = data.frame(DATE_ACQUIRED = as.character(),NDVI = as.numeric(),QA_PIXEL = as.numeric(),combo_NO = as.character(),Weight = as.numeric())
for(i in combo_List){
  pre = get(paste0(i,'_csv_pre2013'))
  pre$DATE_ACQUIRED = as.Date(pre$DATE_ACQUIRED,format = '%Y-%m-%d')
  pre = pre %>% filter(DATE_ACQUIRED <= '2013-03-01')
  pre$DATE_ACQUIRED = as.character(pre$DATE_ACQUIRED)
  post = get(paste0(i,'_csv_post2013'))
  new = rbind(pre,post)
  new$combo_NO = i
  all.landsat = rbind(all.landsat,new)
}
rm(list = ls()[grep("*2013", ls())])

dates_use = unique(all.landsat$DATE_ACQUIRED)
# read in the NDVI fine fitted curves
fitted.curves = read.csv("Data/phenofit_output/all_fits.csv")

fitted.curves = fitted.curves %>%
  dplyr::select(date,NDVI,combo_NO) %>%
  mutate(year = year(date)) %>%
  mutate(doy = yday(date))

fitted.curves$NDVI = ifelse(fitted.curves$NDVI > 1,NA,fitted.curves$NDVI)

fitted.curves = fitted.curves %>% filter(combo_NO != '06310_02') %>%
  filter(combo_NO != '06301_03') %>%
  filter(combo_NO != '06307_05') %>%
  filter(combo_NO != '14409_01') 

sites = unique(fitted.curves$combo_NO)

# create an empty dataframe to store the trend data
trendSlope.df = data.frame(combo_NO = as.character(),slope = as.numeric())
for(i in sites){
  test_site = fitted.curves %>% filter(combo_NO == i)
  test_site = test_site %>% filter(date %in% dates_use,)
  # creating a time series object
  ndvi_ts = ts(test_site$NDVI, frequency = 22, start = c(2000,1), end = c(2020,23))
  # there shouldn't be any NA's but just to be sure
  ndvi_ts = na.approx(ndvi_ts)
  # run the bfast decomposition (0.05 -> min break is one year, stl decomposition, one growing season ('harmonic'), LWZ sensitive to breaks)
  bfm2 <- bfast(ndvi_ts, h = 0.05, decomp = 'stl',
                season = "harmonic", breaks = 'LWZ', max.iter = 5)
  # extract the slope of the trend line
  trendSlope.all = (bfm2$output[[1]]$Tt[463] - bfm2$output[[1]]$Tt[1])/(time(bfm2$output[[1]]$Tt)[463] - time(bfm2$output[[1]]$Tt)[1])
  temp.df = data.frame(combo_NO = i,slope = trendSlope.all,start = bfm2$output[[1]]$Tt[1], end = bfm2$output[[1]]$Tt[463])
  trendSlope.df = rbind(trendSlope.df,temp.df)
}

write.csv(trendSlope.df,'Data/bfast_trendSlope.csv')

###################################################################
######### analyze the slope of the trend data
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)

# read in the slope data
slopes = read.csv('Data/bfast_trendSlope.csv')
slopes = slopes[complete.cases(slopes),]

# read in the treatment data
treatments = read.csv('Data/SFO_allTreatments.csv')
treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)
# read in pasture data
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)

# fix some naming inconsitencies
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Baldy Seding','Baldy Seeding')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Middle \\(Turner\\)','Middle')
treatments$ALLOT_NAME = str_replace(treatments$ALLOT_NAME,'Big Spring','Big Springs')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Big Springs','Big Spring')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'SE Flat','Flat')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'WSA \\(Shears Creek\\)','Shears Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bird Creek Unit','Bird Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bob Moore Unit','Bob Moore Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Deriar Unit','Deriar Creek')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Fenced Pasture','Fenced')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Bolton','Bolton D')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Gary Creek','Gary Creek (E)')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Geertson Creek','Geertson Creek (B)')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Kirtley Creek','Kirtley Creek (C)')
treatments[treatments$ALLOT_NAME == 'Geertson Creek' & treatments$PAST_NAME == 'Seeding',2] = 'Seeding (A)'
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'Ryegrass BLM','Ryegrass')
treatments$PAST_NAME = str_replace(treatments$PAST_NAME,'South Shearing Pen','Shearing Pen')

# join pastures and treatment data
treatments = treatments %>% left_join(pastsDF,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))

treatments = treatments %>% dplyr::select(Treatment,Start,combo_NO)

# fix a spelling mistake in the treatment data
treatments$Treatment = ifelse(treatments$Treatment == 'Early ','Early',treatments$Treatment)

# join slope data
slopes = slopes %>% left_join(treatments,join_by(combo_NO == combo_NO))
slopes = slopes[complete.cases(slopes),]


# not enough samples for these treatments
slopes = slopes[slopes$Treatment!= 'Rest.Rotation',]
slopes = slopes[slopes$Treatment!= 'Late',]
slopes = slopes[slopes$Treatment!= 'Late.Rotation',]
slopes = slopes[slopes$Treatment!= 'Summer.Late.Rotation',]
slopes = slopes[slopes$Treatment!= 'Early.Late.Roatation',]

mean(slopes$slope) #0.002
nrow(slopes[slopes$slope <= 0,]) #11
nrow(slopes[slopes$slope > 0,]) #101
11/101 # .11
# anova of slopes vs treatment
summary(aov(slopes$slope ~ slopes$Treatment)) #p = 0.23
TukeyHSD(aov(slopes$slope ~ slopes$Treatment)) # no significant differences 
ggplot(slopes, aes(x = Treatment, y = slope))  + 
  stat_boxplot(geom ='errorbar',width=0.2) + 
  geom_boxplot(outlier.shape = 2) +
  geom_jitter(width = 0.1, alpha = 0.6)


ggplot(slopes, aes(x=2000,xend=2020,y=start,yend=end,group=combo_NO))+
  geom_segment(linewidth=.5,alpha=0.7,color='grey30')+
  labs(y='NDVI',x='Year')+
  geom_segment(aes(x=2000,xend=2020,y=mean(start),yend=mean(end)),
               color='red',
               linewidth=1.5)+
  theme_bw()+
  theme(axis.text.y = element_text(colour = "black", size = 18, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 22, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))
