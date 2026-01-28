library(dplyr)
modis.df = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_MODIS_pheno.csv")
landsat.df = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno.csv")
#landsat.df2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno2.csv")
# TRS2 has a 20% pheno threshold
# DER is start, peak and end of season

modis.group = modis.df %>% 
  group_by(flag) %>%
  summarise(
    thresh.sos = mean(TRS2.sos),
    thresh.eos = mean(TRS2.eos),
    der.sos = mean(DER.sos),
    der.pos = mean(DER.pos),
    der.eos = mean(DER.eos),
    greenup = mean(Greenup,na.rm = T),
    maturity = mean(Maturity,na.rm = T),
    senescence = mean(Senescence,na.rm = T),
    dormancy = mean(Dormancy,na.rm = T)
  )

landsat.group = landsat.df %>% 
  group_by(flag) %>%
  summarise(
    thresh.sos = mean(TRS2.sos),
    thresh.eos = mean(TRS2.eos),
    der.sos = mean(DER.sos),
    der.pos = mean(DER.pos),
    der.eos = mean(DER.eos),
    greenup = mean(Greenup,na.rm = T),
    maturity = mean(Maturity,na.rm = T),
    senescence = mean(Senescence,na.rm = T),
    dormancy = mean(Dormancy,na.rm = T)
  )
landsat.group$year = c(2000:2020)
modis.group$year = c(2000:2020)
landsat.group$thresh.length = landsat.group$thresh.eos - landsat.group$thresh.sos
modis.group$thresh.length = modis.group$thresh.eos - modis.group$thresh.sos
ggplot() +
  geom_line(aes(y = modis.group$thresh.sos,x = modis.group$year, colour = 'Modis'),group=1) +
  geom_line(aes(y = modis.group$thresh.eos,x = modis.group$year, colour = 'Modis'),group = 1) +
  geom_line(aes(y = landsat.group$thresh.sos,x = landsat.group$year, colour = 'Landsat'),group=1) +
  geom_line(aes(y = landsat.group$thresh.eos,x = landsat.group$year, colour = 'Landsat'),group = 1)+
  labs(colour = '',x='Year',y='DOY for Start/End of Season',title='Day of Year Growing Season Center Ridge A')

ggplot() +
  geom_line(aes(y = modis.group$thresh.length,x = modis.group$year, colour = 'Modis'),group=1) +
  geom_line(aes(y = landsat.group$thresh.length,x = landsat.group$year, colour = 'Landsat'),group=1) +
  labs(colour = '',x='Year',y='Length of Growing Season',title='Length of Growing Season Center Ridge A')

ggplot() +
  geom_line(aes(y = landsat.group$thresh.length,x = landsat.group$year, colour = 'Landsat'),group=1) +
  geom_point(aes(y = landsat.group$thresh.length,x = landsat.group$year))+
  #geom_line(aes(y = landsat.group$thresh.eos,x = landsat.group$year, colour = 'Landsat'),group = 1)+
  geom_smooth(aes(y = landsat.group$thresh.length,x = landsat.group$year))+
  ylim(0,270)
#ggplot() +
#  geom_line(aes(y = modis.group$greenup,x = modis.group$year, colour = 'Modis'),group=1) +
#  geom_line(aes(y = modis.group$senescence,x = modis.group$year, colour = 'Modis'),group = 1) +
#  geom_line(aes(y = landsat.group$greenup,x = landsat.group$year, colour = 'Landsat'),group=1) +
#  geom_line(aes(y = landsat.group$senescence,x = landsat.group$year, colour = 'Landsat'),group = 1)+
#  labs(colour = '',x='Year',y='DOY for Green-up/Senescence',title='Day of Year Green-up/Senescence Center Ridge A')

#ggplot() +
#  geom_line(aes(y = modis.group$der.sos,x = modis.group$flag, coulour = 'sos'),group=1,color = 'blue') +
#  geom_line(aes(y = modis.group$der.pos,x = modis.group$flag, coulour = 'eos'),group = 1,color = 'blue') +
#  geom_line(aes(y = modis.group$der.eos,x = modis.group$flag, coulour = 'eos'),group = 1,color = 'blue') + 
#  geom_line(aes(y = landsat.group$der.sos,x = landsat.group$flag, coulour = 'sos'),group=1) +
#  geom_line(aes(y = landsat.group$der.pos,x = landsat.group$flag, coulour = 'eos'),group = 1) + 
#  geom_line(aes(y = landsat.group$der.eos,x = landsat.group$flag, coulour = 'eos'),group = 1)

## based on these, I think still need to do a confirmation compared to upland to make sure that its not contaminating
## replace the small int with actual growing season values
smooth.int.small = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_small.csv")
smooth.max = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_maxes.csv")
unsmooth.int.small = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_small.csv")
unsmooth.max = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_maxes.csv")

# it seems like smoothing might only be beneficial for integral, but perhaps overlay with climate data first

#### this integral is focused on data above .2, instead i think we need to use actual values for growing season
ggplot() +
  geom_line(aes(y = smooth.int.small$Inte,x = smooth.int.small$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.int.small$Inte,x = unsmooth.int.small$Year, colour = 'unsmooth'),group = 1)+
  labs(colour = '',x='Year',y='Small NDVI-I',title='Annual Small NDVI Integral Center Ridge A')+
  ylim(0,120)

ggplot() +
  geom_line(aes(y = smooth.max$peak,x = smooth.max$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.max$peak,x = unsmooth.max$Year, colour = 'unsmooth'),group = 1)+
  labs(colour = '',x='Year',y='Max NDVI',title='Annual Max NDVI Center Ridge A')+
  ylim(0,0.80)

ggplot() +
  geom_line(aes(y = smooth.max$when_peak,x = smooth.max$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.max$when_peak,x = unsmooth.max$Year, colour = 'unsmooth'),group = 1)+
  labs(colour = '',x='Year',y='DOY for Max NDVI',title='Day of Year for Max NDVI Center Ridge A')+
  ylim(0,230)


# it seems like smoothing might only be beneficial for integral, but perhaps overlay with climate data first

# these are integrals for all values > 0, what would be called the large integral
smooth.int.large = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_large.csv")
unsmooth.int.large = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_large.csv")


ggplot() +
  geom_line(aes(y = smooth.int.large$Inte,x = smooth.int.large$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.int.large$Inte,x = unsmooth.int.large$Year, colour = 'unsmooth'),group = 1)+
  labs(colour = '',x='Year',y='NDVI-I',title='Annual NDVI Integral Center Ridge A')+
  ylim(0,120)

#########################################################
###########################################################################
########################################################

landsat.df = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno.csv")
landsat.df2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno2.csv")
# TRS2 has a 20% pheno threshold
# DER is start, peak and end of season
landsat.group = landsat.df %>% 
  group_by(flag) %>%
  summarise(
    thresh.sos = mean(TRS2.sos),
    thresh.eos = mean(TRS2.eos),
    der.sos = mean(DER.sos),
    der.pos = mean(DER.pos),
    der.eos = mean(DER.eos),
    greenup = mean(Greenup,na.rm = T),
    maturity = mean(Maturity,na.rm = T),
    senescence = mean(Senescence,na.rm = T),
    dormancy = mean(Dormancy,na.rm = T)
  )
landsat.group2 = landsat.df2 %>% 
  group_by(flag) %>%
  summarise(
    thresh.sos = mean(TRS2.sos),
    thresh.eos = mean(TRS2.eos),
    der.sos = mean(DER.sos),
    der.pos = mean(DER.pos),
    der.eos = mean(DER.eos),
    greenup = mean(Greenup,na.rm = T),
    maturity = mean(Maturity,na.rm = T),
    senescence = mean(Senescence,na.rm = T),
    dormancy = mean(Dormancy,na.rm = T)
  )
landsat.group$year = c(2000:2020)
landsat.group2$year = c(2000:2020)
landsat.group$thresh.length = landsat.group$thresh.eos - landsat.group$thresh.sos
landsat.group2$thresh.length = landsat.group2$thresh.eos - landsat.group2$thresh.sos
ggplot() +
  geom_line(aes(y = landsat.group2$thresh.sos,x = landsat.group2$year, colour = 'Landsat 2'),group=1) +
  geom_line(aes(y = landsat.group2$thresh.eos,x = landsat.group2$year, colour = 'Landsat 2'),group = 1) +
  geom_line(aes(y = landsat.group$thresh.sos,x = landsat.group$year, colour = 'Landsat'),group=1) +
  geom_line(aes(y = landsat.group$thresh.eos,x = landsat.group$year, colour = 'Landsat'),group = 1)+
  labs(colour = '',x='Year',y='DOY for Start/End of Season',title='Day of Year Growing Season Center Ridge A')

ggplot() +
  geom_line(aes(y = landsat.group2$thresh.length,x = landsat.group2$year, colour = 'Landsat 2'),group=1) +
  geom_line(aes(y = landsat.group$thresh.length,x = landsat.group$year, colour = 'Landsat'),group=1) +
  labs(colour = '',x='Year',y='Length of Growing Season',title='Length of Growing Season Center Ridge A')

ggplot() +
  geom_line(aes(y = landsat.group2$thresh.length,x = landsat.group2$year, colour = 'Landsat 2'),group=1)+
  geom_line(aes(y = landsat.group$thresh.length,x = landsat.group$year, colour = 'Landsat'),group=1)+
  ylim(0,270)

## based on these, I think still need to do a confirmation compared to upland to make sure that its not contaminating
## replace the small int with actual growing season values
smooth.int.small = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_small.csv")
smooth.max = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_maxes.csv")
unsmooth.int.small = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_small.csv")
unsmooth.max = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_maxes.csv")
smooth.int.small2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_small2.csv")
smooth.max2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_maxes2.csv")
unsmooth.int.small2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_small2.csv")
unsmooth.max2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_maxes2.csv")
# it seems like smoothing might only be beneficial for integral, but perhaps overlay with climate data first

#### this integral is focused on data above .2, instead i think we need to use actual values for growing season
ggplot() +
  geom_line(aes(y = smooth.int.small$Inte,x = smooth.int.small$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.int.small$Inte,x = unsmooth.int.small$Year, colour = 'unsmooth'),group = 1)+
  geom_line(aes(y = smooth.int.small2$Inte,x = smooth.int.small2$Year, colour = 'smooth 2'),group=1) +
  geom_line(aes(y = unsmooth.int.small2$Inte,x = unsmooth.int.small2$Year, colour = 'unsmooth 2'),group = 1)+
  labs(colour = '',x='Year',y='Small NDVI-I',title='Annual Small NDVI Integral Center Ridge A')+
  ylim(0,110)

ggplot() +
  geom_line(aes(y = smooth.max$peak,x = smooth.max$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.max$peak,x = unsmooth.max$Year, colour = 'unsmooth'),group = 1)+
  geom_line(aes(y = smooth.max2$peak,x = smooth.max2$Year, colour = 'smooth 2'),group=1) +
  geom_line(aes(y = unsmooth.max2$peak,x = unsmooth.max2$Year, colour = 'unsmooth 2'),group = 1)+
  labs(colour = '',x='Year',y='Max NDVI',title='Annual Max NDVI Center Ridge A')+
  ylim(0,0.80)

ggplot() +
  geom_line(aes(y = smooth.max$when_peak,x = smooth.max$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.max$when_peak,x = unsmooth.max$Year, colour = 'unsmooth'),group = 1)+
  geom_line(aes(y = smooth.max2$when_peak,x = smooth.max2$Year, colour = 'smooth 2'),group=1) +
  geom_line(aes(y = unsmooth.max2$when_peak,x = unsmooth.max2$Year, colour = 'unsmooth 2'),group = 1)+
  labs(colour = '',x='Year',y='DOY for Max NDVI',title='Day of Year for Max NDVI Center Ridge A')+
  ylim(0,230)


# it seems like smoothing might only be beneficial for integral, but perhaps overlay with climate data first

# these are integrals for all values > 0, what would be called the large integral
smooth.int.large = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_large.csv")
unsmooth.int.large = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_large.csv")
smooth.int.large2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_smoothed_integral_large2.csv")
unsmooth.int.large2 = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_unsmoothed_integral_large2.csv")


ggplot() +
  geom_line(aes(y = smooth.int.large$Inte,x = smooth.int.large$Year, colour = 'smooth'),group=1) +
  geom_line(aes(y = unsmooth.int.large$Inte,x = unsmooth.int.large$Year, colour = 'unsmooth'),group = 1)+
  geom_line(aes(y = smooth.int.large2$Inte,x = smooth.int.large2$Year, colour = 'smooth 2'),group=1) +
  geom_line(aes(y = unsmooth.int.large2$Inte,x = unsmooth.int.large2$Year, colour = 'unsmooth 2'),group = 1)+
  labs(colour = '',x='Year',y='NDVI-I',title='Annual NDVI Integral Center Ridge A')+
  ylim(0,120)


