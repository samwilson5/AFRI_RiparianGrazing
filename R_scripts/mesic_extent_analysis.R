library(sf)
library(stringr)
library(phenofit)
library(zoo)
library(lme4)
library(ggplot2)
library(units)
library(emmeans)
library(dplyr)

# read in pasture shapefile
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")

# create dataframe and calculate area in acres
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$area = drop_units(st_area(pastsSF))
pastsDF$area = pastsDF$area * 0.000247105
# create unique column for each allotment/pasture combo
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)


combo_List = pastsDF$combo_NO

# loop through each pasture and extract the yearly mesic extent, measured and downloaded from GEE
ls_mesic = data.frame(combo_NO = as.character(), year = as.numeric(), ndvi_pixels = as.numeric(),avg_area = as.numeric(), diff_from_avg = as.numeric())
for(i in combo_List){
  temp = read.csv(paste0("Data/SFO_mesicExtent/",i,'.csv'))
  temp$combo_NO = i
  temp$year = str_split(temp$Year_period,'_',simplify=T)[,1]
  temp$year = as.numeric(temp$year)
  temp$ndvi_pixels = temp$NDVI
  #temp = temp %>% filter(ndvi_pixels > 0)
  if(nrow(temp) == 0){print('skip')}
  else{
  temp$avg_area = mean(temp$ndvi_pixels)
  temp$diff_from_avg = temp$ndvi_pixels/temp$avg_area
  temp = temp %>% dplyr::select(combo_NO,year,ndvi_pixels, avg_area,diff_from_avg)
  if(temp$avg_area[1] == 0){print('skip')}
  else{ls_mesic = rbind(ls_mesic,temp)}}
}
ls_mesic$diff_from_avg = ifelse(is.na(ls_mesic$diff_from_avg),1,ls_mesic$diff_from_avg)
# read in climate data ############ probably update the location
all.climate = read.csv("Data/all_pheno_climate.csv")
all.climate = all.climate %>% rename(ALLOT_NAME = allot_name)

# join the climate data to the pasture, rename some things and calculate an average temperature
all.climate = merge(all.climate,pastsDF,by='ALLOT_NAME')
all.climate = all.climate %>% dplyr::select(year,annual.PPT_mm,annual.Tmax_C,annual.Tmin_C,elevation_m,combo_NO,area)
all.climate = all.climate %>% rename(PPT_mm = annual.PPT_mm,Tmax_C = annual.Tmax_C,Tmin_C = annual.Tmin_C)
all.climate$Tavg_C = (all.climate$Tmax_C+all.climate$Tmin_C)/2

# join the climate data to the mesic extent data 
ls_mesic_climate = ls_mesic %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
# remove non-complete cases
ls_mesic_climate = ls_mesic_climate[complete.cases(ls_mesic_climate),]
#length(unique(ls_mesic_climate$combo_NO)) 

# read in treatment data
treatments = read.csv('Data/SFO_allTreatments.csv')

treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)

# from the treatment data to the pasture data there were some inconsistencies in naming
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


treatments = treatments %>% left_join(pastsDF,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))
treatments = treatments %>% dplyr::select(Treatment,Start,combo_NO)
# fix a spelling error
treatments$Treatment = ifelse(treatments$Treatment == 'Early ','Early',treatments$Treatment)

# join treatment data to our mesic extent data
ls_mesic_climate_treat = ls_mesic_climate  %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
# filter for after our known start of treatment
ls_mesic_climate_treat = ls_mesic_climate_treat[ls_mesic_climate_treat$year >= ls_mesic_climate_treat$Start,]

#ls_mesic_climate_treat = ls_mesic_climate_treat[complete.cases(ls_mesic_climate_treat),]
#length(unique(ls_mesic_climate_treat$combo_NO)) #126

# resolving spelling errors
ls_mesic_climate_treat$Treatment = ifelse(ls_mesic_climate_treat$Treatment == 'Early.Late.Roatation','Early.Late.Rotation',ls_mesic_climate_treat$Treatment)
ls_mesic_climate_treat$Treatment = ifelse(ls_mesic_climate_treat$Treatment == 'Early ','Early',ls_mesic_climate_treat$Treatment)

# read in actual use
actual = read.csv('Data/Salmon_actualUse.csv')

# some pastures had multiple leases per year so sum up aums per year per pasture
actual.summarised = actual %>%
  group_by(Allotment,Pasture,Year) %>%
  summarise(actual.AUM = sum(AUM))


actual.summarised = actual.summarised %>%
  filter(Pasture != 'Summary')

# fixing inconsistencies in naming of pastures
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

#actual.summarised = actual.summarised %>%
#  group_by(Allotment,Pasture,Year) %>%
#  summarise(actual.AUM = sum(actual.AUM))

# we need to assign combo number to this df based on allotment/pasture name
actual.use = pastsDF %>% left_join(actual.summarised,join_by(ALLOT_NAME==Allotment,PAST_NAME==Pasture))



actual.use = actual.use %>% dplyr::select(combo_NO,Year,actual.AUM)
actual.use = as.data.frame(actual.use)

# join on the actual use to the mesic extent data
ls_mesic_climate_treat = ls_mesic_climate_treat %>% left_join(actual.use,join_by(combo_NO == combo_NO,year==Year))
# calculate aum/acre
ls_mesic_climate_treat$aum.per.acre = ls_mesic_climate_treat$actual.AUM/ls_mesic_climate_treat$area
# only keep complete cases
ls_mesic_climate_treat = ls_mesic_climate_treat[complete.cases(ls_mesic_climate_treat),]



# not enough samples for these treatments
ls_mesic_climate_treat = ls_mesic_climate_treat %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')


length(unique(ls_mesic_climate_treat$combo_NO)) # 117

# box plot of mesic extent vs treatment
p2 <- ggplot(ls_mesic_climate_treat, aes(x = reorder(Treatment, -diff_from_avg), y = diff_from_avg)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Treatment", y = "Difference from Average")
p2


mean(ls_mesic_climate_treat$diff_from_avg,na.rm=T) #1.12
median(ls_mesic_climate_treat$diff_from_avg)#1.07
sd(ls_mesic_climate_treat$diff_from_avg) #0.45

mean((unique(ls_mesic_climate_treat$avg_area)/4046.86)/unique(ls_mesic_climate_treat$area))
# mixed effects modelling ################################

# base model must include year, site, and aum/acre
m2.lmer = lmer(diff_from_avg ~ year + aum.per.acre + (1|combo_NO),REML=F,data=ls_mesic_climate_treat)

# first add in annual average temp
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.96
#summary(m3.lmer)

# next annual precipitation
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## p < 0.001

# our main variable of interest is treatment
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## p = 0.07

# model with our significant variables
m1.lmer = lmer(diff_from_avg  ~ PPT_mm + year + aum.per.acre + (1|combo_NO),REML=T,data=ls_mesic_climate_treat)


mtest = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,mtest,test='Chi') #p < 0.001
summary(mtest)
car::Anova(mtest,type='II') # treatment is very significant
confint(mtest,method='profile') #late and summer are both fully negative
emmeans(mtest,~Treatment)
plot(emmeans(mtest,~Treatment),xlab='Estimated Mean Percent of Average Mesic Extent',
     ylab = 'Timing of Grazing',color='grey10')+ 
  theme(text = element_text(size = 20))+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y = element_text(colour = "black", size = 18, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 22, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 22, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))+
  annotate('text',x=1.26,y = 'Continuous',label='bold(ab)',parse=T,size=10)+
  annotate('text',x =1.24, y = 'Early',label='bold(a)',parse=T,size=10)+
  annotate('text',x=1.18,y='Late',label='bold(ab)',parse=T,size=10)+
  annotate('text',x=1.16,y='Summer',label='bold(b)',parse=T,size=10)
pairs(emmeans(mtest,~Treatment)) # early and summer are significantly different

# diagnostics
plot(m1.lmer, Treatment ~ resid(.), abline = 0 )

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")
qqnorm(resid(m1.lmer))
qqline(residuals(m1.lmer),col=2)

plot(fitted(m1.lmer),residuals(m1.lmer))

summary(m1.lmer)
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -8.4790 -0.3044 -0.0082  0.2444  8.8780 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# combo_NO (Intercept) 49502    222.49  
# year     (Intercept)  3738     61.14  
# Residual             24415    156.25  
# Number of obs: 1935, groups:  combo_NO, 124; year, 17
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                     435.41     112.63   3.866
# Tavg_C                          -35.22      16.41  -2.146
# TreatmentEarly                 -214.11      70.49  -3.037
# TreatmentEarly.Late.Roatation  -176.98     173.09  -1.022
# TreatmentExclosure             -154.47     129.48  -1.193
# TreatmentLate                   -45.19      93.33  -0.484
# TreatmentRest.Rotation         -144.03     237.14  -0.607
# TreatmentSummer                -142.57      73.84  -1.931
# TreatmentSummer.Late.Rotation   237.40     172.73   1.374