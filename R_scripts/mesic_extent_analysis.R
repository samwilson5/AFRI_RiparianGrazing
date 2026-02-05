library(sf)
library(stringr)
library(dplyr)
library(phenofit)
library(zoo)
library(lme4)
library(ggplot2)
library(units)
library(emmeans)
pastsSF = st_read("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")

pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$area = drop_units(st_area(pastsSF))

pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)


combo_List = pastsDF$combo_NO

ls_mesic = data.frame(combo_NO = as.character(), year = as.numeric(), ndvi_pixels = as.numeric(),avg_area = as.numeric(), diff_from_avg = as.numeric())
for(i in combo_List){
  temp = read.csv(paste0("Data/SFO_Mesic_Extent_SeasonMEAN3/",i,'.csv'))
  temp$combo_NO = i
  temp$year = str_split(temp$Year_period,'_',simplify=T)[,1]
  temp$year = as.numeric(temp$year)
  temp$ndvi_pixels = temp$NDVI
  temp$avg_area = mean(temp$ndvi_pixels)
  temp$diff_from_avg = temp$ndvi_pixels/temp$avg_area
  temp = temp %>% select(combo_NO,year,ndvi_pixels, avg_area,diff_from_avg)
  ls_mesic = rbind(ls_mesic,temp)
}

all.climate = read.csv("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno_climate.csv")
all.climate = all.climate %>% rename(ALLOT_NAME = allot_name)

all.climate = merge(all.climate,pastsDF,by='ALLOT_NAME')
all.climate = all.climate %>% dplyr::select(year,annual.PPT_mm,annual.Tmax_C,annual.Tmin_C,elevation_m,combo_NO,area)
all.climate = all.climate %>% rename(PPT_mm = annual.PPT_mm,Tmax_C = annual.Tmax_C,Tmin_C = annual.Tmin_C)
all.climate$Tavg_C = (all.climate$Tmax_C+all.climate$Tmin_C)/2

ls_mesic_climate = ls_mesic %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
ls_mesic_climate = ls_mesic_climate[complete.cases(ls_mesic_climate),]
length(unique(ls_mesic_climate$combo_NO)) ## 116 sites available for this analysis

treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SFO_allTreatments.csv')

treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)

treatments = treatments %>% left_join(pastsDF,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))
treatments = treatments %>% dplyr::select(Treatment,Start,combo_NO)
treatments$Treatment = ifelse(treatments$Treatment == 'Early ','Early',treatments$Treatment)

ls_mesic_climate_treat = ls_mesic_climate  %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
ls_mesic_climate_treat = ls_mesic_climate_treat[ls_mesic_climate_treat$year >= ls_mesic_climate_treat$Start,]

ls_mesic_climate_treat = ls_mesic_climate_treat[complete.cases(ls_mesic_climate_treat),]
length(unique(ls_mesic_climate_treat$combo_NO)) #117

ls_mesic_climate_treat$Treatment = ifelse(ls_mesic_climate_treat$Treatment == 'Early.Late.Roatation','Early.Late.Rotation',ls_mesic_climate_treat$Treatment)
#ls_mesic_climate_treat$mesic_area = ls_mesic_climate_treat$ndvi_pixels * 900
#ls_mesic_climate_treat$comparison = ls_mesic_climate_treat$mesic_area/ls_mesic_climate_treat$area
### first variable for mixed effects -> growing season integral (gs_integral)
ls_mesic_climate_treat = ls_mesic_climate_treat %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')
ls_mesic_climate_treat = ls_mesic_climate_treat %>%
  filter(combo_NO != '06236_05',
         combo_NO != '06204_07',
         combo_NO != '06203_05')

p1 <- ggplot(ls_mesic_climate_treat, aes(x = year, y = ndvi_pixels)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Frequency\n(Prepositions)")
p1

p2 <- ggplot(ls_mesic_climate_treat, aes(x = reorder(Treatment, -ndvi_pixels), y = ndvi_pixels)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

p3 <- ggplot(ls_mesic_climate_treat, aes(ndvi_pixels)) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Prepositions)")
p3

ggplot(ls_mesic_climate_treat[ls_mesic_climate_treat$ndvi_pixels > 0,], aes(log(ndvi_pixels))) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Prepositions)")

ggplot(ls_mesic_climate_treat, aes(year, ndvi_pixels)) +
  geom_point() +
  facet_wrap(~ Treatment, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition", y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 10000))

ggplot(ls_mesic_climate_treat, aes(x = year, y = ndvi_pixels, group = combo_NO)) +
  geom_line(alpha = 0.3) +
  labs(
    x = "Year",
    y = "y",
    title = "Trajectories of y over time by site"
  ) +
  theme_bw()

# model with only site as a random effect and no fixed effects
m0.lmer = lmer(ndvi_pixels ~ 1 + (1|combo_NO),REML=F,data = ls_mesic_climate_treat)
AIC(logLik(m0.lmer)) # 25759

# model with only year as a random effect and no fixed effects
m1.lmer = lmer(ndvi_pixels ~ 1 + (1|year), REML = T,data = ls_mesic_climate_treat)
AIC(logLik(m1.lmer)) # 3341

# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(ndvi_pixels ~ 1 + (1|year) + (1|combo_NO), REML = T, data = ls_mesic_climate_treat)
AIC(logLik(m2.lmer)) #25530
anova(m0.lmer,m2.lmer,test='Chi') # so including year and site results in lower AIC and p = <0.0001 so this model choice is definitely the move

summary(m2.lmer)
ls_mesic_climate_treat = ls_mesic_climate_treat[ls_mesic_climate_treat$ndvi_pixels > 0,]
ls_mesic_climate_treat$ndvi_pixels_log = log(ls_mesic_climate_treat$ndvi_pixels)
m2.lmer = lmer(ndvi_pixels_log ~ year + (1|combo_NO),REML=F,data=ls_mesic_climate_treat)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m

#lets just use average because i think it makes more sense
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.00000000
summary(m3.lmer)

# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## .00000000

# tavg and ppt are both good

## no elevation

m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## p = 0.007

m1.lmer = lmer(ndvi_pixels_log ~ PPT_mm + Tavg_C + year + Treatment + (1|combo_NO),REML=T,data=ls_mesic_climate_treat)
m1.lmer = lmer(ndvi_pixels ~ PPT_mm + Tavg_C + year + Treatment + (1|combo_NO),REML=T,data=ls_mesic_climate_treat)

car::Anova(m1.lmer,type='II') # treatment is very significant
confint(m1.lmer,method='profile') #confusingly all the treatments intersect 0 
emmeans(m1.lmer,~Treatment)
plot(emmeans(m1.lmer,~Treatment))
pairs(emmeans(m1.lmer,~Treatment)) # early and summer are significantly different

(RG4 <- ref_grid(m1.lmer))
emmip(RG4, Treatment~PPT_mm,style='factor')
#m2.lmer = update(m1.lmer, .~.+ elevation_m)
#anova(m1.lmer,m2.lmer,test='Chi')


plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # summer and continuous are messes

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")
qqnorm(resid(m1.lmer))
qqline(residuals(m1.lmer),col=2)

plot(fitted(m1.lmer),residuals(m1.lmer))
# we need to figure out outliers because they are quite serious
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

# cooks distance 4/n -> number of years of number of plots
# .ls.resid for boxplot outliers