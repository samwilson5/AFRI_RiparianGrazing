library(dplyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(stringr)
library(sf)
library(units)

# read in pasture data
pastsSF = st_read("Data/SFO_pastures/Salmon_Pastures.shp")

# calculate area in acres
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$area = drop_units(st_area(pastsSF))
pastsDF$area = pastsDF$area * 0.000247105
pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)


combo_List = pastsDF$combo_NO

# calculating number of pixels for riparian areas in each pasture
ls_sampleSize = data.frame(combo_NO = as.character(), size_pixels = as.numeric())
for(i in combo_List){
  temp = read.csv(paste0("Data/SFO_riparian_sampleSize/",i,'.csv'))
  temp$combo_NO = i
  temp = temp %>% rename(size_pixels = SR_B1)
  ls_sampleSize = rbind(ls_sampleSize,temp)
}

# create a list with pastures with large enough areas (>0.1 square kilometers)
ls_sampleSize = ls_sampleSize %>% filter(size_pixels >= 10)
sites_to_keep = c(ls_sampleSize$combo_NO) 


# years of NAIP imagery
years = c('2004','2006','2009','2011','2013','2015','2017','2019')

# load in woody cover data from NAIP imagery, calculated in GEE
woody = data.frame(ALLOT_NO = as.character(),PAST_NO = as.character(),woody = as.numeric(),year = as.numeric())
for(i in years){
  x = read.csv(paste0('Data/NAIP_woodyCover/',i,'_woody.csv'))
  x = x %>% dplyr::select(ALLOT_NO,PAST_NO,woody)
  x$ALLOT_NO = as.character(x$ALLOT_NO)
  x$PAST_NO = as.character(x$PAST_NO)
  x$year = as.numeric(i)
  woody = rbind(woody,x)
}

# split apart the stored values for allotment and pasture, then create a unique identifier
woody$ALLOT_NO = str_pad(woody$ALLOT_NO, 5, side='left', pad='0')
woody$PAST_NO = str_pad(woody$PAST_NO, 2, side='left', pad='0')
woody$combo_NO = paste0(woody$ALLOT_NO,'_',woody$PAST_NO)

woody = woody %>% dplyr::select(woody,year,combo_NO)

# join woody data to pasture data
analysis_data = woody %>% left_join(pastsDF,join_by(combo_NO == combo_NO))

analysis_data = analysis_data %>% dplyr::select(woody,year,combo_NO,ALLOT_NAME,PAST_NAME,area)

# load in treatment data
treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SFO_allTreatments.csv')
treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)

# correct naming inconsistencies
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

# join treatment to woody data 
analysis_data = analysis_data %>% left_join(treatments,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))

# correct spelling error in treatment data
analysis_data$Treatment = ifelse(analysis_data$Treatment == 'Early ','Early',analysis_data$Treatment)

# load in actual use
actual = read.csv('Data/Salmon_actualUse.csv')

actual.summarised = actual %>%
  group_by(Allotment,Pasture,Year) %>%
  summarise(actual.AUM = sum(AUM))

actual.summarised = actual.summarised %>%
  filter(Pasture != 'Summary')

# correct naming inconsistencies
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

actual.summarised = actual.summarised %>%
  group_by(Allotment,Pasture,Year) %>%
  summarise(actual.AUM = sum(actual.AUM))

actual.use = pastsDF %>% left_join(actual.summarised,join_by(ALLOT_NAME==Allotment,PAST_NAME==Pasture))

actual.use = actual.use %>% dplyr::select(combo_NO,Year,actual.AUM)

analysis_data = analysis_data %>% left_join(actual.use,join_by(combo_NO == combo_NO,year==Year))
# calculate aum/acre
analysis_data$aum.per.acre = analysis_data$actual.AUM/analysis_data$area

# load in climate data
all.climate = read.csv("Data/all_pheno_climate.csv")
all.climate = all.climate %>% rename(ALLOT_NAME = allot_name)

all.climate = merge(all.climate,pastsDF,by='ALLOT_NAME')
all.climate = all.climate %>% dplyr::select(year,annual.PPT_mm,annual.Tmax_C,annual.Tmin_C,elevation_m,combo_NO)
all.climate = all.climate %>% rename(PPT_mm = annual.PPT_mm,Tmax_C = annual.Tmax_C,Tmin_C = annual.Tmin_C)

# calculate average temperature
all.climate$Tavg_C = (all.climate$Tmax_C+all.climate$Tmin_C)/2

analysis_data = analysis_data %>% left_join(all.climate,join_by(combo_NO==combo_NO,year==year))

# filter start date and complete cases
analysis_data = analysis_data[analysis_data$year >= analysis_data$Start,]
analysis_data = analysis_data[complete.cases(analysis_data),]
analysis_data = analysis_data[analysis_data$combo_NO %in% sites_to_keep,]

# not enough samples to keep these treatments
analysis_data = analysis_data %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')

p2 <- ggplot(analysis_data, aes(x = reorder(Treatment, -woody), y = woody)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

mean(analysis_data$woody) #72%
sd(analysis_data$woody) #18%
median(analysis_data$woody)
length(unique(analysis_data$combo_NO)) # 103
################################################### mixed effects modelling ##################

# base model must include year, site, and aum/acre
m2.lmer = lmer(woody ~ aum.per.acre + year + (1|combo_NO),REML=F,data=analysis_data)

# start with annual precip
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = .12

# average temperature
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # p < 0.001

# treatment is our variable of interest
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') #0.31

# create a model with our significant variables
m1.lmer = lmer(woody ~ year + aum.per.acre + Tavg_C + (1|combo_NO),REML=F,data=analysis_data)

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.49

m2.lmer = update(m1.lmer, .~.+ Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') # p = 0.63
plot(emmeans(m2.lmer, ~ Treatment))
summary(m2.lmer)
pairs(emmeans(m2.lmer, ~Treatment))

# based on our analysis it would appear there is no impact of treatment (or interaction)