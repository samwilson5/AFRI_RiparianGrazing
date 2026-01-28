library(sf)
library(stringr)
library(dplyr)
library(phenofit)
library(zoo)
library(ggplot2)
pastsSF = st_read("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])

pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)

combo_List = pastsDF$combo_NO

for(i in combo_List){
  ndvi = read.csv(paste0("C:/Users/samwi/OneDrive - University of Idaho/SFO_LANDSAT_NDVI/",i,'.csv'))
  qa = read.csv(paste0("C:/Users/samwi/OneDrive - University of Idaho/SFO_LANDSAT_QA/",i,'.csv'))
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




for(i in combo_List){
  ndvi = read.csv(paste0("C:/Users/samwi/OneDrive - University of Idaho/SFO_LANDSAT8_NDVI/",i,'.csv'))
  qa = read.csv(paste0("C:/Users/samwi/OneDrive - University of Idaho/SFO_LANDSAT8_QA/",i,'.csv'))
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


# Now run phenofit on the data for each site -> 2012 may need to be dropped
pheno_ready = all.landsat[,c('DATE_ACQUIRED','NDVI','Weight','combo_NO')]
pheno_ready$DATE_ACQUIRED = as.Date(pheno_ready$DATE_ACQUIRED, format = '%Y-%m-%d')
pheno_ready$NDVI = ifelse(pheno_ready$NDVI < 0, 0, pheno_ready$NDVI)
#pheno_ready = pheno_ready[complete.cases(pheno_ready),]
pheno_ready = pheno_ready %>% rename(
  t = DATE_ACQUIRED,
  y = NDVI,
  w = Weight
)

nptperyear     <- 22
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")


for(i in combo_List){
  filtered.pheno_ready = pheno_ready %>% filter(combo_NO == i)
  if(all(is.na(filtered.pheno_ready$y))){skip = 'skip'}
  else{
  filtered.pheno_ready$y = na.approx(filtered.pheno_ready$y,na.rm=F)
  filtered.pheno_ready$w = ifelse(is.na(filtered.pheno_ready$w),0.2,filtered.pheno_ready$w)
  filtered.pheno_ready = filtered.pheno_ready[,c('t','y','w')]
  INPUT <- check_input(filtered.pheno_ready$t, filtered.pheno_ready$y, filtered.pheno_ready$w,
                       #QC_flag = d$QC_flag,
                       nptperyear = nptperyear,
                       maxgap = nptperyear / 4, wmin = 0)
  brks <- season_mov(INPUT,
                     list(FUN = "smooth_wWHIT", wFUN = wFUN,
                          maxExtendMonth = 3,
                          wmin = wmin, r_min = 0.1))
  fit <- curvefits(INPUT, brks,
                   list(
                     methods = methods_fine, # ,"klos",, 'Gu'
                     wFUN = wFUN,
                     iters = 2,
                     wmin = wmin,
                     # constrain = FALSE,
                     nextend = 2,
                     maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                     minPercValid = minPercValid))
  l_param <- get_param(fit)
  #print(l_param$Beck)
  dfit <- get_fitting(fit)
  #print(dfit)
  
  TRS <- 0.2#c(0.1, 0.2, 0.5)
  l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) # %>% map(~melt_list(., "meth"))
  #print(l_pheno$doy$Beck)
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
    #post.mean = post.mean %>% filter(date <= paste0(j,'-12-31'))
    fitted.curve = rbind(fitted.curve,post.mean)
  }
  fitted.curve$combo_NO = i
  assign(paste0('fitted.curve_',i),fitted.curve)
  
  pheno <- l_pheno$doy %>% melt_list('meth')
  
  plot_season(INPUT, brks, ylab = "NDVI")
  
  #g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "NDVI", angle = 0)
  #grid::grid.newpage()
  #grid::grid.draw(g)
  
  #l_pheno <- get_pheno(fit[1:7], #method = "GU", 
  #                     TRS = TRS, IsPlot = TRUE, show.title = FALSE)
  
  #l_pheno <- get_pheno(fit[7], #method = "GU", 
  #                     TRS = TRS, IsPlot = TRUE, show.title = T)
  
  #fitted = data.frame(doy = fit[["2006_1"]][["model"]][["AG"]][["tout"]],
  #                    NDVI = fit[["2006_1"]][["model"]][["AG"]][["zs"]][["iter2"]])
  #unfitted = data.frame(ogNDVI = fit[["2006_1"]][["data"]][["y"]],
  #                      ofDOY = fit[["2006_1"]][["data"]][["t"]])
  #unfitted$Date = as.Date(unfitted$ofDOY, origin = '2000-01-01')
  #fitted$Date = as.Date(fitted$doy, origin = '2000-01-01')
  
  #p <- ggplot() +
  #  geom_line(aes(x=unfitted$Date,y=unfitted$ogNDVI),alpha=0.4,)+
  #  labs(x='Date',y='NDVI')+
  #  geom_point(aes(x=unfitted$Date,y=unfitted$ogNDVI))+
  #  geom_line(aes(x=fitted$Date,y=fitted$NDVI),linewidth=1.1)+
  #  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.sos"]],color='darkgreen',linewidth=1)+
  #  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.eos"]],color='red',linewidth=1)+
  #  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["DER.pos"]],color='blue',linewidth=1)+
  #  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.sos"]] + 20,
  #           y = 0.1, label = paste0('SOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.sos"]],format='%m/%d')),color = "darkgreen")+
  #  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.eos"]] + 20,
  #           y = 0.22, label = paste0('EOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.eos"]],format='%m/%d')),color = "red")+
  #  annotate("text", x = l_pheno[["date"]][["AG"]][["DER.pos"]] + 20,
  #           y = 0.65, label = paste0('POS: ',format(l_pheno[["date"]][["AG"]][["DER.pos"]],format='%m/%d')),color = "blue")
  
  
  #p
  pheno$combo_NO = i
  assign(paste0('pheno_',i),pheno)}
  
}


rm(fitted.curve)
rm(pheno)
rm(pheno_ready)

all.fits = grep('^fitted.curve', ls(), value = TRUE) |>
  mget() |>
  bind_rows()

all.pheno = grep('^pheno', ls(), value = TRUE) |>
  mget() |>
  bind_rows()


length(unique(all.fits$combo_NO)) #243
length(unique(all.pheno$combo_NO)) #243

write.csv(all.fits,"C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_fits.csv")
write.csv(all.pheno,"C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno.csv")
###########################################################################################################################
## now we're actually going to do some stats on the pheno/landsat data
## start simple and just get peak of season
library(sf)
library(dplyr)
library(stringr)
fitted.curve.all = read.csv("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_fits.csv")
pheno.all = read.csv("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno.csv")
treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SFO_allTreatments.csv')

pastsSF = st_read("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")
pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])

pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)
treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)

treatments = treatments %>% left_join(pastsDF,join_by(ALLOT_NAME == ALLOT_NAME,PAST_NAME == PAST_NAME))
treatments = treatments %>% dplyr::select(Treatment,Start,combo_NO)
treatments$Treatment = ifelse(treatments$Treatment == 'Early ','Early',treatments$Treatment)

pheno.sites = unique(pheno.all$combo_NO)

splitup = function(x){
  z = as.numeric(str_split(x,'_',simplify = T)[1,1])
  return(z)
}

summarized.pheno.all = data.frame(year = as.numeric(),mean.pos = as.numeric(),mean.sos = as.numeric(),mean.eos = as.numeric(),combo_NO = as.character())
for(i in pheno.sites){
  pheno.singlesite = pheno.all %>% filter(combo_NO == i)
  summarized.singlesite = pheno.singlesite %>% 
    group_by(flag) %>%
    summarise(mean.pos = round(mean(DER.pos)),
              mean.sos = round(mean(TRS2.sos)),
              mean.eos = round(mean(TRS2.eos)))
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
## need to fill gaps in 2000 - 2020 with NA's just to keep a good record of things

for(i in pheno.sites){
  onlyone = summarized.pheno.all %>% filter(combo_NO == i)
  if(nrow(onlyone) != 21){print(i)}
}
# this site has bad phenofit and needed to be dropped
summarized.pheno.all = summarized.pheno.all %>% filter(combo_NO != '06310_02') %>%
  filter(combo_NO != '06301_03') %>%
  filter(combo_NO != '06307_05') %>%
  filter(combo_NO != '14409_01')

summarized.pheno.all = summarized.pheno.all  %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
summarized.pheno.all = summarized.pheno.all[summarized.pheno.all$year >= summarized.pheno.all$Start,]


average.summarized.pheno.all = summarized.pheno.all %>%
  group_by(combo_NO) %>%
  summarise(
    pos = mean(mean.pos,na.rm = T),
    sos = mean(mean.sos,na.rm = T),
    eos = mean(mean.eos,na.rm = T),
    Treatment = first(Treatment)
  ) 


### so we have cleaned up the data pretty well but for a mixed effects model, I think that we are certainly going to need
### precip, temp, elvation data
## see SFO_climate_download.R for how these csv's were created

all.climate = read.csv("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno_climate.csv")
all.climate = all.climate %>% rename(ALLOT_NAME = allot_name)

all.climate = merge(all.climate,pastsDF,by='ALLOT_NAME')
all.climate = all.climate %>% dplyr::select(year,annual.PPT_mm,annual.Tmax_C,annual.Tmin_C,elevation_m,combo_NO)
all.climate = all.climate %>% rename(PPT_mm = annual.PPT_mm,Tmax_C = annual.Tmax_C,Tmin_C = annual.Tmin_C)
temp = summarized.pheno.all %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
temp = temp[complete.cases(temp),]
length(unique(temp$combo_NO)) ## 116 sites available for this analysis

all.climate$Tavg_C = (all.climate$Tmax_C+all.climate$Tmin_C)/2


all.climate.annual = all.climate %>% group_by(combo_NO) %>%
  summarise(PPT_mm = mean(PPT_mm),
            Tmax_C = mean(Tmax_C),
            Tmin_C = mean(Tmin_C),
            Tavg_C = mean(Tavg_C),
            elevation_m = first(elevation_m))

temp2 = average.summarized.pheno.all %>% left_join(all.climate.annual,join_by(combo_NO == combo_NO))
temp2 = temp2[complete.cases(temp2),]
length(unique(temp2$combo_NO)) ## 116 sites available 

## mixed effects modelling
summarized.pheno.climate = summarized.pheno.all %>% left_join(all.climate,join_by(year==year,combo_NO==combo_NO))
summarized.pheno.climate = summarized.pheno.climate[complete.cases(summarized.pheno.climate),]

average.summarized.climate = average.summarized.pheno.all %>% left_join(all.climate.annual,join_by(combo_NO == combo_NO))
average.summarized.climate = average.summarized.climate[complete.cases(average.summarized.climate),]



library(lubridate)
fitted.curve.all = fitted.curve.all %>%
  dplyr::select(date,NDVI,combo_NO) %>%
  mutate(year = year(date)) %>%
  mutate(doy = yday(date))

fitted.curve.all = fitted.curve.all %>%
  left_join(treatments,join_by(combo_NO == combo_NO))
fitted.curve.all = fitted.curve.all[fitted.curve.all$year >= fitted.curve.all$Start,]

avg.sos = round(mean(average.summarized.climate$sos)) #106
avg.eos = round(mean(average.summarized.climate$eos)) #312
poses = summarized.pheno.climate %>% dplyr::select(combo_NO,mean.pos,year)

fitted.curve.all = fitted.curve.all %>% left_join(poses,join_by(combo_NO == combo_NO,year==year))

getInt = function(x,start,end){
  limit = x[x$doy >= start & x$doy <= end,]
  total = sum(limit$NDVI,na.rm = T)
  return(total)
}
fitted.curve.all$NDVI = ifelse(fitted.curve.all$NDVI > 1,fitted.curve.all$NDVI/100,fitted.curve.all$NDVI)

fitted.response.variables = fitted.curve.all %>%
  group_by(combo_NO,year) %>%
  reframe(
    gs_integral = sum(NDVI[doy >= avg.sos & doy <= avg.eos]),
    annual_integral = sum(NDVI),
    peak_NDVI = NDVI[doy == mean.pos],
    Treatment = first(Treatment)
  )

fitted.response.variables = fitted.response.variables[complete.cases(fitted.response.variables),]
fitted.response.climate = fitted.response.variables %>%
  left_join(all.climate,join_by(combo_NO==combo_NO,year==year))

### first variable for mixed effects -> growing season integral (gs_integral)

p1 <- ggplot(fitted.response.climate, aes(x = year, y = gs_integral)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Frequency\n(Prepositions)")
p1

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -gs_integral), y = gs_integral)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

p3 <- ggplot(fitted.response.climate, aes(gs_integral)) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Prepositions)")
p3

ggplot(fitted.response.climate, aes(year, gs_integral)) +
  geom_point() +
  facet_wrap(~ Treatment, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition", y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 100))


# model with only site as a random effect and no fixed effects
m0.lmer = lmer(gs_integral ~ 1 + (1|combo_NO),REML=F,data = fitted.response.climate)
AIC(logLik(m0.lmer)) # 11262

# model with only year as a random effect and no fixed effects
m1.lmer = lmer(gs_integral ~ 1 + (1|year), REML = T,data = fitted.response.climate)
AIC(logLik(m1.lmer)) # 12020

# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(gs_integral ~ 1 + (1|year) + (1|combo_NO), REML = T, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #10323
anova(m0.lmer,m2.lmer,test='Chi') # so including year and site results in lower AIC and p = <0.0001 so this model choice is definitely the move

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m


#lets just use average because i think it makes more sense
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.01

# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## ppt certainly stays

# tavg and ppt are both good

## no elevation

m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## p = 0.57

m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + (1|year) + (1|combo_NO),data=fitted.response.climate)
car::Anova(m1.lmer,type='II')
confint(m1.lmer) # this shows Tavg including zero so lets look at treatment without zero
#m1.lmer = lmer(gs_integral ~ PPT_mm + (1|year) + (1|combo_NO),data=fitted.response.climate)

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') #0.77

m2.lmer = update(m1.lmer, .~.+Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') #0.005


m2.lmer = update(m1.lmer, .~.+Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') #0.77

# so lets look at the interaction
m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + (1|year) + (1|combo_NO),data=fitted.response.climate)
confint(m1.lmer) # only the interaction term for rest.rotation does not contain 0
car::Anova(m1.lmer,type='II') #PPT and the interaction are significant
emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences
### overall, theres not a lot that can be said about only a significant result from the interaction....

plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
summary(m1.lmer)
# random effects:
  ## combo_NO (Intercept) -> 34.02
  ## year (Intercept) -> 18.65
  ## residual -> 14.54 SD 3.813
## Tavg_C => -2.6 so higher average temperature leads to a smaller gs_integral
## PPT_mm => 0.02 => higher precip higher gs_integral
## elevation is confusting -> -0.01 higher elevation lower gs_integral
## treatments is a little strange -> negative slopes are Early.Late.Rotation, Exclosure, SUmmer, Summer.Late.Roation
## -> positive slopes are Early, Late, Rest Rotation

## interaction term is even more difficult to understand
## increase temp = lower gs_integral -> Early, Late, Rest Rotation
## increase temp = higher gs_integral -> Early.Late.Rotation, Exclosure, Summer, Summer.Late.Rotation
#TreatmentEarly                         3.518627   3.385153   1.039
#TreatmentEarly.Late.Roatation        -19.112488   9.955145  -1.920
#TreatmentExclosure                    -5.403678   6.553999  -0.824
#TreatmentLate                          6.188497   4.395195   1.408
#TreatmentRest.Rotation                32.787247  15.016575   2.183
#TreatmentSummer                       -0.632679   3.498196  -0.181
#TreatmentSummer.Late.Rotation        -10.814235   8.931838  -1.211
#Tavg_C:TreatmentEarly                 -0.524656   0.510008  -1.029
#Tavg_C:TreatmentEarly.Late.Roatation   2.004394   1.312442   1.527
#Tavg_C:TreatmentExclosure              2.170968   1.012148   2.145
#Tavg_C:TreatmentLate                  -0.990430   0.686284  -1.443
#Tavg_C:TreatmentRest.Rotation         -4.203059   1.856811  -2.264
#Tavg_C:TreatmentSummer                 0.318717   0.530420   0.601
#Tavg_C:TreatmentSummer.Late.Rotation   2.613834   1.489547   1.755

###### second variable -> annual integral (annual_integral) ########################

p1 <- ggplot(fitted.response.climate, aes(x = year, y = annual_integral)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Frequency\n(Prepositions)")
p1

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -annual_integral), y = annual_integral)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

p3 <- ggplot(fitted.response.climate, aes(annual_integral)) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Prepositions)")
p3

ggplot(fitted.response.climate, aes(year, annual_integral)) +
  geom_point() +
  facet_wrap(~ Treatment, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition", y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 100))



# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(annual_integral ~ 1 + (1|year) + (1|combo_NO), REML = T, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #11344

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m

# first tmax
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.09 -> marginally better


# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.00000 -> way better

## next treatment
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.75


m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate)
confint(m1.lmer) # tavg includes 0
car::Anova(m1.lmer,type='II')
m1.lmer = lmer(annual_integral ~ PPT_mm + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate)

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.85

m2.lmer = update(m1.lmer, .~.+ Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.05
car::Anova(m2.lmer,type='II') #PPT and interaction are significant

m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.67

m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate) 
confint(m1.lmer) #only summer.late.rotation doesn't include 0
car::Anova(m1.lmer,type='II') #PPT and the interaction are significant
emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences

plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
summary(m1.lmer)

#Random effects:
#  Groups   Name        Variance Std.Dev.
#combo_NO (Intercept) 52.88    7.272   
#year     (Intercept) 26.06    5.105   
#Residual             26.26    5.125   
#Number of obs: 1771, groups:  combo_NO, 116; year, 17

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)                           27.3475     6.1941   4.415
#Tavg_C                                 0.3414     0.8893   0.384
#PPT_mm                                 7.8308     1.5558   5.033
#TreatmentEarly                         5.2033     4.4344   1.173
#TreatmentEarly.Late.Roatation        -21.5840    13.1300  -1.644
#TreatmentExclosure                    -5.4653     8.5552  -0.639
#TreatmentLate                          5.5447     5.7457   0.965
#TreatmentRest.Rotation                29.7632    19.9406   1.493
#TreatmentSummer                       -0.2806     4.5386  -0.062
#TreatmentSummer.Late.Rotation        -16.7119    11.7563  -1.422
#Tavg_C:TreatmentEarly                 -0.7330     0.6818  -1.075
#Tavg_C:TreatmentEarly.Late.Roatation   2.5333     1.7613   1.438
#Tavg_C:TreatmentExclosure              2.3524     1.3497   1.743
#Tavg_C:TreatmentLate                  -0.8901     0.9168  -0.971
#Tavg_C:TreatmentRest.Rotation         -4.0354     2.4944  -1.618
#Tavg_C:TreatmentSummer                 0.2354     0.7088   0.332
#Tavg_C:TreatmentSummer.Late.Rotation   4.1585     2.0001   2.079

###### third variable -> peak NDVI (peak_NDVI) ########################

p1 <- ggplot(fitted.response.climate, aes(x = year, y = peak_NDVI)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Frequency\n(Prepositions)")
p1

p2 <- ggplot(fitted.response.climate, aes(x = reorder(Treatment, -peak_NDVI), y = peak_NDVI)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p2

p3 <- ggplot(fitted.response.climate, aes(peak_NDVI)) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Prepositions)")
p3

ggplot(fitted.response.climate, aes(year, peak_NDVI)) +
  geom_point() +
  facet_wrap(~ Treatment, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition", y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 0.5))



# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(peak_NDVI ~ 1 + (1|year) + (1|combo_NO), REML = T, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #11344

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m

# first tmax
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.03 -> better


# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.00000 -> way better

## next treatment
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.35


m1.lmer = lmer(peak_NDVI ~ Tavg_C + PPT_mm + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate)
confint(m1.lmer) #tavg contains 0
car::Anova(m1.lmer,type='II')
m1.lmer = lmer(peak_NDVI ~ PPT_mm + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate)

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.6

m2.lmer = update(m1.lmer, .~.+ Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.005
car::Anova(m2.lmer,type='II') #interaction and ppt are significant

m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.12

m1.lmer = lmer(peak_NDVI ~ Tavg_C + PPT_mm + Treatment*Tavg_C + (1|year) + (1|combo_NO),REML = F,data = fitted.response.climate) 
confint(m1.lmer) #only Late doesn't include 0
car::Anova(m1.lmer,type='II') #PPT and the interaction are significant
emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences

plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
summary(m1.lmer)
#Random effects:
#  Groups   Name        Variance  Std.Dev.
#combo_NO (Intercept) 0.0017223 0.04150 
#year     (Intercept) 0.0002294 0.01515 
#Residual             0.0004626 0.02151 
#Number of obs: 1771, groups:  combo_NO, 116; year, 17

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)                           0.1249041  0.0287805   4.340
#Tavg_C                                0.0019759  0.0041314   0.478
#PPT_mm                                0.0586185  0.0065216   8.988
#TreatmentEarly                        0.0120071  0.0208186   0.577
#TreatmentEarly.Late.Roatation        -0.0749830  0.0591663  -1.267
#TreatmentExclosure                   -0.0009324  0.0407285  -0.023
#TreatmentLate                         0.0593740  0.0270519   2.195
#TreatmentRest.Rotation                0.1577114  0.0887239   1.778
#TreatmentSummer                      -0.0181171  0.0213718  -0.848
#TreatmentSummer.Late.Rotation         0.0178641  0.0538324   0.332
#Tavg_C:TreatmentEarly                -0.0018865  0.0029134  -0.648
#Tavg_C:TreatmentEarly.Late.Roatation  0.0036950  0.0074077   0.499
#Tavg_C:TreatmentExclosure             0.0078460  0.0058091   1.351
#Tavg_C:TreatmentLate                 -0.0098676  0.0039233  -2.515
#Tavg_C:TreatmentRest.Rotation        -0.0202717  0.0104787  -1.935
#Tavg_C:TreatmentSummer                0.0039777  0.0030269   1.314
#Tavg_C:TreatmentSummer.Late.Rotation -0.0027592  0.0084060  -0.328

plot(m1.lmer)
# again no clear pattern

qqnorm(resid(m1.lmer))
# roughly good

model.test.2 = mutate(fitted.response.climate,
                                   prediction = fitted(m1.lmer),
                                   resid = peak_NDVI - prediction,
                                   resid2 = resid^2)

smoothplot = function(data, xvar, yvar, jitter = .2){
  ggplot(data, aes({{xvar}}, {{yvar}})) +
    geom_point(position = position_jitter(width = jitter),
               alpha = .5) +
    stat_smooth(method = 'loess', formula = y ~ x)
}
smoothplot(model.test.2, prediction, resid)
# gaussian(?) need to understand what that means

rfx = data.frame(ranef(m1.lmer))
hist(rfx$condval)
# roughly normal and centered on zero?

smoothplot(model.test.2, Tavg_C, resid2)
 # looking for no pattern or outliers

ggplot(model.test.2, aes(year, resid2)) + 
  stat_summary() + labs(y = 'Mean Squared Error')
### looking for some what consistent mean squared errors across groups
## 2015 has a crazy amount of variability compared to everywhere else..... could
# mean that it is worth dropping

#### take aways##################
# model fits are marginal, need to figure out assumptions
# for the treatment effect, the direction of the slopes were the same for each treatment across variables 
# except summer.late.rotation which switched from negative to postive slope

# for the interatction, the direction of the slopes were the same for each treatment across variables 
# except summer.late.rotation which switched from postive to negative slope
########################################################################################################
all.climate2 = read.csv("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Remote_sensing_READYFORANALYSIS/all_pheno_climate.csv")
summarized.pheno.all
all.climate2 = all.climate2 %>% rename(ALLOT_NAME = allot_name)
all.climate.pheno = merge(all.climate2,pastsDF,by='ALLOT_NAME')
all.climate.pheno = all.climate.pheno %>% rename(late.season.PPT_mm = late.seaon.PPT_mm)
all.climate.pheno = all.climate.pheno %>% select(year,spring.Tmax_C,spring.Tmin_C,spring.PPT_mm,summer.Tmax_C,summer.Tmin_C,summer.PPT_mm,fall.Tmax_C,fall.Tmin_C,fall.PPT_mm,early.season.Tmax_C,early.season.Tmin_C,early.season.PPT_mm,mid.season.Tmax_C,mid.season.Tmin_C,mid.season.PPT_mm,late.season.Tmax_C,late.season.Tmin_C,late.season.PPT_mm,annual.Tmax_C,annual.Tmin_C,annual.PPT_mm,elevation_m,combo_NO)
pheno.variables = summarized.pheno.all %>% left_join(all.climate.pheno,join_by(year==year,combo_NO==combo_NO))
pheno.variables = pheno.variables[complete.cases(pheno.variables),]
########## lets look at the phenology variables and see if anyhting seems reasonable #############################################
# start with pos
ggplot(pheno.variables,aes(annual.Tmax_C,mean.eos)) +
  geom_point()
summary(lm(early.season.Tmin_C + early.season.Tmax_C ~ mean.sos,data=pheno.variables))
summary(lm(elevation_m ~ mean.pos,data=pheno.variables[pheno.variables$year == 2020,]))
summary(lm(annual.Tmin_C + annual.Tmax_C ~ mean.eos,data=pheno.variables))

ggplot(pheno.variables,aes(spring.Tmin_C,summer.Tmin_C)) +
  geom_point()

ggplot(pheno.variables,aes(early.season.Tmin_C,spring.Tmin_C)) +
  geom_point()
summary(lm(early.season.Tmin_C ~ elevation_m,data=pheno.variables)) 
# models certainly should not include both Tmin and Tmax by time frame

ggplot(pheno.variables,aes(early.season.Tmin_C,elevation_m)) +
  geom_point()
summary(lm(early.season.Tmin_C ~ elevation_m,data=pheno.variables)) 
#temperature and elevation are super related, should not be used together
#################################### start with peak of season (mean.pos) #######################33
# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(mean.pos ~ 1 + (1|year) + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #16378

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') ##  p =0.39 -> dont include

m3.lmer = update(m2.lmer, .~.+ spring.Tmin_C) ## p = 0.12 -> not great, circle back if nothing else works
anova(m2.lmer,m3.lmer,test='Chi')

# next mid season tmax/tmin 
m3.lmer = update(m2.lmer, .~.+ summer.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.91 -> dont include

m3.lmer = update(m2.lmer, .~.+ summer.Tmin_C)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.07 -> include
# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.58 -> dont include

m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.48 dont include

mtest = lmer(mean.pos ~ Treatment + (1|year) + (1|combo_NO),REML = F, data = pheno.variables)
anova(m2.lmer,mtest)# p = 0.49

# since we are not going to use elevation, the only fixed effect outside treatment will be
# summer min temp
m1.lmer = lmer(mean.pos ~ 1 + (1|year) + (1|combo_NO) + summer.Tmin_C, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.42

m2.lmer = update(m1.lmer, .~.+ Treatment*summer.Tmax_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.15

##### it would appear that there is nothing for treatment and peak of season.....
##############################3
# next with sos

#temperature and elevation are super related, should not be used together

# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(mean.sos ~ 1 + (1|year) + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #17041

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') ##  p =0.007 -> dont include

m3.lmer = update(m2.lmer, .~.+ spring.Tmin_C) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.0007 model is even better, so include tmin but not tmax due to collinearity

# going to skip summer since sos are before summer

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.03 -> include

# so spring tmin and ppt are both in
m3.lmer = update(m2.lmer, .~. + spring.Tmin_C + spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.00001 -> wooo

mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.93 ooof

m1.lmer = lmer(mean.sos ~ 1 + (1|year) + (1|combo_NO) + spring.Tmin_C + spring.PPT_mm, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.87

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tmin_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.98

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.95
##### it would appear that there is nothing for treatment and start of season.....

##############################3
# next with eos

#temperature and elevation are super related, should not be used together

# model with site and and year as random effect and no fixed effects
m2.lmer = lmer(mean.eos ~ 1 + (1|year) + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #17041

# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') ##  p =0.34 -> dont include

m3.lmer = update(m2.lmer, .~.+ spring.Tmin_C) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.06 probably include

# summer 
m3.lmer = update(m2.lmer, .~.+ summer.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') ##  p =0.19 -> dont include

m3.lmer = update(m2.lmer, .~.+ summer.Tmin_C) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.10 maybe include

# fall
m3.lmer = update(m2.lmer, .~.+ fall.Tmax_C)
anova(m2.lmer,m3.lmer,test='Chi') ##  p =0.52 -> dont include

m3.lmer = update(m2.lmer, .~.+ fall.Tmin_C) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.53 dont include

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.59 -> dont include

m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.93 -> dont include

m3.lmer = update(m2.lmer, .~.+ fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.001 -> include!

# so spring tmin, summer tmin and fall ppt are all in
m3.lmer = update(m2.lmer, .~. + spring.Tmin_C + summer.Tmin_C + fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.004 -> wooo

mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.07 interesting!!
confint(mtest) ## once again all treatment confints include 0...

m1.lmer = lmer(mean.eos ~ 1 + (1|year) + (1|combo_NO) + spring.Tmin_C + summer.Tmin_C + fall.PPT_mm, REML = F, data = pheno.variables)
confint(m1.lmer) # only fall.PPT_mm doesn't contain 0
m1.lmer = lmer(mean.eos ~ 1 + (1|year) + (1|combo_NO) + fall.PPT_mm, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.05!!!! significant results!!

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tmin_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.22

m2.lmer = update(m1.lmer, .~.+ Treatment*summer.Tmin_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.07 meh

m2.lmer = update(m1.lmer, .~.+ Treatment*fall.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.16

##### it would appear that treatment is important for end of season!!!

m1.lmer = lmer(mean.eos ~ 1 + (1|year) + (1|combo_NO) + fall.PPT_mm, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.05!!!! significant results!!
confint(m2.lmer)
## however upon looking at confints all treatment confints contain 0....
car::Anova(m2.lmer,type='II') #both are significant
emmeans(m2.lmer, ~ Treatment) # they all overlap
pairs(emmeans(m2.lmer, ~ Treatment)) # no significant differences
summary(m2.lmer)

#Random effects:
#  Groups   Name        Variance Std.Dev.
#combo_NO (Intercept)  98.48    9.924  
#year     (Intercept) 154.71   12.438  
#Residual             598.56   24.466  
#Number of obs: 1771, groups:  combo_NO, 116; year, 17

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)                   328.86256   17.93782  18.333
#spring.Tmin_C                   1.60291    2.09475   0.765
#summer.Tmin_C                  -0.26987    2.23296  -0.121
#fall.PPT_mm                    -0.10951    0.03272  -3.347
#TreatmentEarly                 -5.56522    3.70494  -1.502
#TreatmentEarly.Late.Roatation  12.19430    9.24920   1.318
#TreatmentExclosure              1.22711    7.54984   0.163
#TreatmentLate                  -3.41990    4.92041  -0.695
#TreatmentRest.Rotation         -5.69078   12.62452  -0.451
#TreatmentSummer                 1.69929    3.90982   0.435
#TreatmentSummer.Late.Rotation  13.90143    9.36662   1.484

summary(aov(pos ~ Treatment,data=average.summarized.pheno.all)) #0.18
summary(aov(sos ~ Treatment,data=average.summarized.pheno.all)) #0.89
summary(aov(eos ~ Treatment,data=average.summarized.pheno.all)) #0.04 ############



fitted.response.annual.average = fitted.response.variables %>%
  group_by(combo_NO) %>% 
  summarise(
    gs_integral = mean(gs_integral),
    annual_integral = mean(annual_integral),
    peak_NDVI = mean(peak_NDVI),
    Treatment = first(Treatment)
  )


summary(aov(gs_integral ~ Treatment,data=fitted.response.annual.average)) #0.57
summary(aov(annual_integral ~ Treatment,data=fitted.response.annual.average)) #0.722
summary(aov(peak_NDVI ~ Treatment,data=fitted.response.annual.average)) #0.38


####################33 scratch work ################################################
pre.landsat = data.frame(DATE_ACQUIRED = as.character(),NDVI = as.numeric(),QA_PIXEL = as.numeric(),combo_NO = as.character())
for(i in combo_List){
  pre = get(paste0(i,'_csv_pre2013'))
  pre$combo_NO = i
  pre.landsat = rbind(pre.landsat,pre)
}

post.landsat = data.frame(DATE_ACQUIRED = as.character(),NDVI = as.numeric(),QA_PIXEL = as.numeric(),combo_NO = as.character())
for(i in combo_List){
  post = get(paste0(i,'_csv_post2013'))
  post$combo_NO = i
  post.landsat = rbind(post.landsat,post)
}


# Landsat 4-7 QA breakdown Updated!
# 1 = fill -> bad
# 5440 = clear -> good
# 5442 = dilated clouds over land -> marginal??
# 5504 = water with lows set -> good
# 5506 = dilated cloud over water -> marginal??
# 5696 = mid conf cloud -> marginal
# 5698 = dilated, clead, mid conf cloud -> marginal 
# 5760 = mid conf cloud over water -> marginal
# 5896 = high conf cloud -> bad
# 7440 = high conf cloud shadow -> bad
# 7442 = high conf cloud shadow -> bad 
# 7568 = water with cloud shadow -> bad
# 7696 = mid conf cloud with shadow -> bad
# 7698 = dilated cloud, high conf cloud shadow, mid conf cloud -> bad
# 7824 = mid conf cloud with shadow on water -> bad
# 7960 = high conf cloud with shadow -> bad
# 8088 = high conf cloud with shadow on water -> bad
# 13600 = high conf snow -> bad
# 13602 = high conf snow -> bad
# 13664 = high conf snow -> bad
# 13856 = high conf snow -> bad
# 13858 = high conf snow -> bad #############


temp = pre.landsat %>%
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


# Landsat 8 QA breakdown
# 1 = fill -> bad
# 21762 = dilated -> marginal
# 21824 = clear -> good
# 21826 = dilated clouds over land -> marginal??
# 21888 = water -> good
# 21890 = dilated cloud over water -> marginal??
# 21952 = water -> good
# 22018 = dilated, mid conf clouds -> marginal
# 22080 = mid conf cloud -> marginal
# 22144 = mid conf cloud over water -> marginal
# 22280 = high conf cloud -> bad
# 23826 = dilated, high conf cloud shadow -> bad
# 23888 = high conf cloud shadow -> bad
# 23952 = water with cloud shadow -> bad
# 24082 = dilated, high conf cloud shadow -> bad
# 24088 = mid conf cloud with shadow -> bad
# 24144 = high conf cloud shadow -> bad
# 24216 = mid conf cloud with shadow on water -> bad
# 24344 = high conf cloud with shadow -> bad
# 24472 = high conf cloud with shadow on water -> bad
# 29986 = high conf snow -> bad
# 30048 = high conf snow -> bad
# 30242 = high conf snow -> bad
# 30304 = high conf snow -> bad
# 54534 = dilated, high conf cirrus -> marginal
# 54596 = high conf cirrus -> good
# 54790 = high conf cirrus, mid conf clouds -> marginal
# 54852 = cirrus, mid cloud -> marginal
# 55052 = cirrus, high cloud -> bad
# 56598 = cirrus, high conf cloud shadow -> bad
# 56660 = cirrus, high conf cloud shadow -> bad
# 56854 = cirrus, high conf cloud shadow -> bad
# 56916 = cirrus, high conf cloud shadow -> bad
# 62758 = cirrus, high conf snow -> bad
# 62820 = cirrus, high conf snow -> bad
# 63014 = cirrus, high conf snow -> bad
# 63076 = cirrus, high conf snow -> bad



temp2 = post.landsat %>%
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
