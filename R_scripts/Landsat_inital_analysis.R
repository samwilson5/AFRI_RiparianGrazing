library(ggplot2)
library(dplyr)
library(zoo)
### method for unsmoothed peak NDVI
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/Landsat_centerRidgeA.csv")
#landsat = landsat[complete.cases(landsat),]
landsat$DATE_ACQUIRED = as.Date(landsat$DATE_ACQUIRED, format = '%b %d, %Y')
landsat = landsat[-473,]
landsat$Year = as.numeric(format(landsat$DATE_ACQUIRED,'%Y'))
landsat$doy = as.numeric(format(landsat$DATE_ACQUIRED,'%j'))
landsat$NDVI = na.approx(landsat$NDVI,na.rm=F)
landsat$NDVI = ifelse(is.na(landsat$NDVI) == T,0,landsat$NDVI)
set.seed(123)
z = runif(472,min=0,max=2)/10000
landsat$NDVI = landsat$NDVI + z
year.maxes = landsat %>% 
  group_by(Year) %>%
  summarise(peak = max(NDVI),
            when_peak = landsat[which(landsat$NDVI == max(NDVI)),4],
            indices = which(landsat$NDVI == max(NDVI)))
year.maxes$peak = year.maxes$peak - z[year.maxes$indices]

year.maxes %>%
  ggplot( aes(x=Year, y=peak)) +
  geom_line() +
  geom_point()
write.csv(year.maxes,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_maxes.csv")

###################################
## method for integral based on raw landsat data
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/Landsat_centerRidgeA.csv")
#landsat = landsat[complete.cases(landsat),]
landsat = landsat[-473,]
landsat$DATE_ACQUIRED = as.Date(landsat$DATE_ACQUIRED, format = '%b %d, %Y')
landsat$doy = as.numeric(format(landsat$DATE_ACQUIRED,'%j'))
landsat$Year = as.numeric(format(landsat$DATE_ACQUIRED,'%Y'))
landsat$NDVI = na.approx(landsat$NDVI,na.rm=F)
landsat$NDVI = ifelse(is.na(landsat$NDVI) == T,0,landsat$NDVI)
## filter based on values <0.2
ndvi.filtered = landsat %>% 
  group_by(Year)
ndvi.filtered$NDVI = ifelse(ndvi.filtered$NDVI < 0, 0, ndvi.filtered$NDVI)

annualIntegral = data.frame(Year = 2000:2020,Inte = 0)
for(i in 2000:2020){
  y = ndvi.filtered[ndvi.filtered$Year == i,]
  temp = cumsum(y$NDVI[-nrow(y)]*diff(y$doy))
  annualIntegral[which(annualIntegral$Year == i),2] = temp[length(temp)]
}

annualIntegral %>%
  ggplot( aes(x=Year, y=Inte)) +
  geom_line() +
  geom_point()

write.csv(annualIntegral,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_large.csv")



## method for integral based on smoothed curves see phenofit, will need to write 
## methods for this soon
##################################################################
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/Landsat_centerRidgeA.csv")
landsat = landsat[-473,]
landsat = landsat %>% 
  rename(date = DATE_ACQUIRED,
         y = NDVI)
landsat$t = as.Date(landsat$date, format = '%b %d, %Y')
landsat$w = 1.0
landsat$w = ifelse(is.na(landsat$y)==T,0.2,landsat$w)
landsat$y = na.approx(landsat$y,na.rm=F)
landsat$w = ifelse(is.na(landsat$y)==T,0,landsat$w)

d = landsat

lambda         <- 5
nptperyear     <- 22
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

INPUT <- check_input(d$t, d$y, d$w,
                     QC_flag = d$QC_flag,
                     nptperyear = nptperyear,
                     maxgap = nptperyear / 4, wmin = 0
)

brks <- season_mov(INPUT,
                   list(FUN = "smooth_wWHIT", wFUN = wFUN,
                        maxExtendMonth = 3,
                        wmin = wmin, r_min = 0.1
                   ))

## 2.4 Curve fitting
fit <- curvefits(INPUT, brks,
                 list(
                   methods ='AG',
                   wFUN = wFUN,
                   iters = 2,
                   wmin = wmin,
                   # constrain = FALSE,
                   nextend = 2,
                   maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                   minPercValid = minPercValid
                 ))

## curve fit data from 2000
#fit$'2000_1'$model$Gu$zs$iter2
## corresponding days
#as.Date(fit$'2000_1'$tout,origin='2000-01-01')

for(i in 1:21){
  model = fit[[i]]$model$AG$zs$iter2
  series = as.Date(fit[[i]]$tout,origin='2000-01-01')
  w = data.frame(modeled = model,date = series)
  if(i == 1){smooth.ls = w}
  else{smooth.ls = rbind(smooth.ls,w)}
}

smooth.ls$Year = as.numeric(format(smooth.ls$date,'%Y'))
smooth.ls$doy = as.numeric(format(smooth.ls$date,'%j'))

ndvi.filtered = smooth.ls %>% 
  group_by(Year)

ndvi.filtered$modeled = ifelse(ndvi.filtered$modeled < 0, 0, ndvi.filtered$modeled)
annualIntegral = data.frame(Year = 2000:2020,Inte = 0)
for(i in 2000:2020){
  y = ndvi.filtered[ndvi.filtered$Year == i,]
  temp = cumsum(y$modeled[-nrow(y)]*diff(y$doy))
  annualIntegral[which(annualIntegral$Year == i),2] = temp[length(temp)]
}

annualIntegral %>%
  ggplot( aes(x=Year, y=Inte)) +
  geom_line() +
  geom_point()


write.csv(annualIntegral,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_large.csv")

smoothed.max = smooth.ls %>% 
  group_by(Year) %>% 
  summarise(peak = max(modeled),when_peak = smooth.ls[which(smooth.ls$modeled == max(modeled)),4])
write.csv(smoothed.max,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_maxes.csv")







########################################################################
## copute small integral
## method for integral based on raw landsat data
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Landsat_centerRidgeA.csv")
#landsat = landsat[complete.cases(landsat),]
landsat = landsat[-473,]
landsat$DATE_ACQUIRED = as.Date(landsat$DATE_ACQUIRED, format = '%b %d, %Y')
landsat$doy = as.numeric(format(landsat$DATE_ACQUIRED,'%j'))
landsat$Year = as.numeric(format(landsat$DATE_ACQUIRED,'%Y'))
landsat$NDVI = na.approx(landsat$NDVI,na.rm=F)
landsat$NDVI = ifelse(is.na(landsat$NDVI) == T,0,landsat$NDVI)
## filter based on values <0.2
ndvi.filtered = landsat %>% 
  group_by(Year) %>% 
  filter(doy >= 104 & doy <= 318)
ndvi.filtered$NDVI = ifelse(ndvi.filtered$NDVI < 0, 0, ndvi.filtered$NDVI)

annualIntegral = data.frame(Year = 2000:2020,Inte = 0)
for(i in 2000:2020){
  y = ndvi.filtered[ndvi.filtered$Year == i,]
  temp = cumsum(y$NDVI[-nrow(y)]*diff(y$doy))
  annualIntegral[which(annualIntegral$Year == i),2] = temp[length(temp)]
}

annualIntegral %>%
  ggplot( aes(x=Year, y=Inte)) +
  geom_line() +
  geom_point()

write.csv(annualIntegral,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_small.csv")



## method for small integral based on smoothed curves see phenofit, will need to write 
## methods for this soon
##################################################################
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Landsat_centerRidgeA.csv")
landsat = landsat[-473,]
landsat = landsat %>% 
  rename(date = DATE_ACQUIRED,
         y = NDVI)
landsat$t = as.Date(landsat$date, format = '%b %d, %Y')
landsat$w = 1.0
landsat$w = ifelse(is.na(landsat$y)==T,0.2,landsat$w)
landsat$y = na.approx(landsat$y,na.rm=F)
landsat$w = ifelse(is.na(landsat$y)==T,0,landsat$w)

d = landsat

lambda         <- 5
nptperyear     <- 22
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

INPUT <- check_input(d$t, d$y, d$w,
                     QC_flag = d$QC_flag,
                     nptperyear = nptperyear,
                     maxgap = nptperyear / 4, wmin = 0
)

brks <- season_mov(INPUT,
                   list(FUN = "smooth_wWHIT", wFUN = wFUN,
                        maxExtendMonth = 3,
                        wmin = wmin, r_min = 0.1
                   ))

## 2.4 Curve fitting
fit <- curvefits(INPUT, brks,
                 list(
                   methods ='AG',
                   wFUN = wFUN,
                   iters = 2,
                   wmin = wmin,
                   # constrain = FALSE,
                   nextend = 2,
                   maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                   minPercValid = minPercValid
                 ))

## curve fit data from 2000
#fit$'2000_1'$model$Gu$zs$iter2
## corresponding days
#as.Date(fit$'2000_1'$tout,origin='2000-01-01')

for(i in 1:21){
  model = fit[[i]]$model$AG$zs$iter2
  series = as.Date(fit[[i]]$tout,origin='2000-01-01')
  w = data.frame(modeled = model,date = series)
  if(i == 1){smooth.ls = w}
  else{smooth.ls = rbind(smooth.ls,w)}
}

smooth.ls$Year = as.numeric(format(smooth.ls$date,'%Y'))
smooth.ls$doy = as.numeric(format(smooth.ls$date,'%j'))

ndvi.filtered = smooth.ls %>% 
  group_by(Year) %>% 
  filter(doy >= 104 & doy <= 318) 

ndvi.filtered$modeled = ifelse(ndvi.filtered$modeled < 0, 0, ndvi.filtered$modeled)
annualIntegral = data.frame(Year = 2000:2020,Inte = 0)
for(i in 2000:2020){
  y = ndvi.filtered[ndvi.filtered$Year == i,]
  temp = cumsum(y$modeled[-nrow(y)]*diff(y$doy))
  annualIntegral[which(annualIntegral$Year == i),2] = temp[length(temp)]
}

annualIntegral %>%
  ggplot( aes(x=Year, y=Inte)) +
  geom_line() +
  geom_point()


write.csv(annualIntegral,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_small.csv")









library(plotly)
fig <- plot_ly(complete.land, x = ~date, y = ~NDVI, name = 'trace_0',type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = smoothed01, name = 'trace_1', mode = 'lines')
fig






#############################################
library(phenofit)
library(dplyr)
library(ggplot2)

landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Landsat_centerRidgeA.csv")
modis = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Modis_centerRidgeA.csv")

qc_summary(1033)
bitwAnd(1033)
bitwShiftL(1,10)
bitwAnd(36872,1024)

paste(as.integer(rev(intToBits(1033))[17:32]), collapse="")
intToBits(1033)

dec2bin <- function(x) paste(as.integer(rev(intToBits(x))[17:32]), collapse = "")
y = dec2bin(1033)


ranges = c(1,2,3,3,4,6,7,8,9,10,11,11,12,12,13,13,14,14,15,15,16,16)
breaks = seq(1,22,2)

substring(y,ranges[3],ranges[3+1])

for(i in breaks){
  bit = substring(y,ranges[i],ranges[i+1])
  
}