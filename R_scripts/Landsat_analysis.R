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
pastsSF = st_read("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")

pastsDF = as.data.frame(pastsSF[,c('ALLOT_NO','ALLOT_NAME','PAST_NO','PAST_NAME')])
pastsDF$area = drop_units(st_area(pastsSF))

pastsDF$combo_NO = paste0(pastsDF$ALLOT_NO,'_',pastsDF$PAST_NO)


combo_List = pastsDF$combo_NO

ls_sampleSize = data.frame(combo_NO = as.character(), size_pixels = as.numeric())
for(i in combo_List){
  temp = read.csv(paste0("Data/SFO_Riparian_SampleSize/",i,'.csv'))
  temp$combo_NO = i
  temp = temp %>% rename(size_pixels = SR_B1)
  ls_sampleSize = rbind(ls_sampleSize,temp)
}

ls_sampleSize = ls_sampleSize %>% filter(size_pixels >= 10)
sites_to_keep = c(ls_sampleSize$combo_NO)

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
# this site has bad phenofit and needed to be dropped, we need to mention this somewhere and why we think that the phenofit was unreliable......
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
fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

################# see what happens if we remove treatments with very few samples
fitted.response.climate = fitted.response.climate %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')

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
m1.lmer = lmer(gs_integral ~ 1 + (1|year) + (1|combo_NO), REML = F, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #10323
anova(m0.lmer,m2.lmer,test='Chi') # so including year and site results in lower AIC and p = <0.0001 so this model choice is definitely the move

m2.lmer = lmer(gs_integral ~ year + (1|combo_NO),REML=F,data=fitted.response.climate)
anova(m1.lmer,m2.lmer,test='Chi') # so including year and site results in lower AIC and p = <0.0001 so this model choice is definitely the move

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
anova(m2.lmer,m3.lmer,test='Chi') ## p = 0.34

m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + year + (1|combo_NO),data=fitted.response.climate)
car::Anova(m1.lmer,type='II')
confint(m1.lmer) # this shows that temp and precip are both important and increase the gs_integral
#m1.lmer = lmer(gs_integral ~ PPT_mm + (1|year) + (1|combo_NO),data=fitted.response.climate)

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') #0.20

m2.lmer = update(m1.lmer, .~.+Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') #0.01


m2.lmer = update(m1.lmer, .~.+Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') #0.27

# so lets look at the interaction
m1.lmer = lmer(gs_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + year + (1|combo_NO),data=fitted.response.climate)
confint(m1.lmer) # all interaction terms cross 0
car::Anova(m1.lmer,type='II') # everything except just treatment is significant
emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences
plot(emmeans(m1.lmer, ~ Treatment*Tavg_C))
### overall, theres not a lot that can be said about only a significant result from the interaction....
(RG4 <- ref_grid(m1.lmer))
emmip(m1.lmer, Treatment~Tavg_C,style='factor',at = list(Tavg_C = 2:9))



plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
summary(m1.lmer)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# combo_NO (Intercept) 29.76    5.455   
# Residual             20.26    4.502   
# Number of obs: 1636, groups:  combo_NO, 106
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          -987.84853   49.86488 -19.811
# Tavg_C                                  1.32155    0.54419   2.428
# PPT_mm                                  4.09666    0.69321   5.910
# TreatmentEarly                          3.76709    3.91785   0.962
# TreatmentEarly.Late.Roatation         -20.26697   11.25603  -1.801
# TreatmentExclosure                     -4.76161    7.16866  -0.664
# TreatmentLate                           6.15531    4.99790   1.232
# TreatmentRest.Rotation                 28.11224   17.11558   1.642
# TreatmentSummer                        -1.74785    3.93751  -0.444
# TreatmentSummer.Late.Rotation         -14.86540   10.04481  -1.480
# year                                    0.50085    0.02489  20.121
# Tavg_C:TreatmentEarly                  -0.40581    0.61520  -0.660
# Tavg_C:TreatmentEarly.Late.Roatation    1.94047    1.54975   1.252
# Tavg_C:TreatmentExclosure               2.11055    1.17089   1.803
# Tavg_C:TreatmentLate                   -0.53274    0.84010  -0.634
# Tavg_C:TreatmentRest.Rotation          -4.06219    2.19142  -1.854
# Tavg_C:TreatmentSummer                  0.60031    0.63101   0.951
# Tavg_C:TreatmentSummer.Late.Rotation    3.16896    1.75511   1.806

###### second variable -> annual integral (annual_integral) ########################
fitted.response.climate = fitted.response.climate[fitted.response.climate$combo_NO %in% sites_to_keep,]

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
m1.lmer = lmer(annual_integral ~ 1 + (1|year) + (1|combo_NO), REML = F, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #11344

m2.lmer = lmer(annual_integral ~ year + (1|combo_NO), REML = F, data = fitted.response.climate)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m

# first tmax
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.0001 


# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.00000 -> way better

## next treatment
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.63


m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + year + (1|combo_NO),REML = F,data = fitted.response.climate)
confint(m1.lmer) # 
car::Anova(m1.lmer,type='II')

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.25

m2.lmer = update(m1.lmer, .~.+ Treatment*Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.05
car::Anova(m2.lmer,type='II') # all except just treatment are significant

m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.45

m1.lmer = lmer(annual_integral ~ Tavg_C + PPT_mm + Treatment*Tavg_C + year + (1|combo_NO),REML = F,data = fitted.response.climate) 
confint(m1.lmer) #only summer.late.rotation doesn't include 0
car::Anova(m1.lmer,type='II') 
emmeans(m1.lmer, ~ Treatment*Tavg_C) # they all overlap
pairs(emmeans(m1.lmer, ~ Treatment*Tavg_C)) # no significant differences

plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
summary(m1.lmer)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# combo_NO (Intercept) 51.98    7.210   
# Residual             37.10    6.091   
# Number of obs: 1636, groups:  combo_NO, 106
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                          -1.573e+03  6.746e+01 -23.322
# Tavg_C                                2.784e+00  7.346e-01   3.790
# PPT_mm                                4.994e+00  9.374e-01   5.327
# TreatmentEarly                        6.247e+00  5.266e+00   1.186
# TreatmentEarly.Late.Roatation        -2.620e+01  1.518e+01  -1.726
# TreatmentExclosure                   -3.474e+00  9.622e+00  -0.361
# TreatmentLate                         6.455e+00  6.716e+00   0.961
# TreatmentRest.Rotation                2.258e+01  2.310e+01   0.978
# TreatmentSummer                       3.539e-01  5.292e+00   0.067
# TreatmentSummer.Late.Rotation        -2.074e+01  1.353e+01  -1.533
# year                                  7.908e-01  3.368e-02  23.483
# Tavg_C:TreatmentEarly                -6.259e-01  8.305e-01  -0.754
# Tavg_C:TreatmentEarly.Late.Roatation  2.604e+00  2.096e+00   1.242
# Tavg_C:TreatmentExclosure             2.131e+00  1.579e+00   1.349
# Tavg_C:TreatmentLate                 -4.047e-01  1.134e+00  -0.357
# Tavg_C:TreatmentRest.Rotation        -3.933e+00  2.965e+00  -1.326
# Tavg_C:TreatmentSummer                5.068e-01  8.519e-01   0.595
# Tavg_C:TreatmentSummer.Late.Rotation  4.943e+00  2.374e+00   2.082

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
m1.lmer = lmer(peak_NDVI ~ 1 + (1|year) + (1|combo_NO), REML = T, data = fitted.response.climate)
AIC(logLik(m2.lmer)) #11344

m2.lmer = lmer(peak_NDVI ~ year + (1|combo_NO), REML = F, data = fitted.response.climate)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first
# we should also be looking at PPT_mm, Tmax_C, Tmin_C, elevation_m

# first tmax
m3.lmer = update(m2.lmer, .~.+ Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.07 -> nah


# next ppt_mm
m3.lmer = update(m2.lmer, .~.+ PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.00000 -> way better

## next treatment
m3.lmer = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,m3.lmer,test='Chi') ## 0.14


m1.lmer = lmer(peak_NDVI ~ PPT_mm + year + (1|combo_NO),REML = F,data = fitted.response.climate)
confint(m1.lmer) #tavg contains 0
car::Anova(m1.lmer,type='II')

m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.17

m2.lmer = update(m1.lmer, .~.+ Treatment*PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## 0.08

m1.lmer = lmer(peak_NDVI ~ PPT_mm + Treatment*PPT_mm + year + (1|combo_NO),REML = F,data = fitted.response.climate) 
confint(m1.lmer) #only summer doesn't include 0
car::Anova(m1.lmer,type='II') #interaction is not significant, i think its a bust

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
pheno.variables = pheno.variables[pheno.variables$combo_NO %in% sites_to_keep,]
pheno.variables = pheno.variables %>%
  filter(Treatment != 'Early.Late.Rotation',
         Treatment != 'Exclosure',
         Treatment != 'Rest.Rotation',
         Treatment != 'Summer.Late.Rotation',
         Treatment != 'Early.Late.Roatation')

pheno.variables$spring.Tavg_C = (pheno.variables$spring.Tmax_C + pheno.variables$spring.Tmin_C)/2
pheno.variables$summer.Tavg_C = (pheno.variables$summer.Tmax_C + pheno.variables$summer.Tmin_C)/2
pheno.variables$fall.Tavg_C = (pheno.variables$fall.Tmax_C + pheno.variables$fall.Tmin_C)/2
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
m1.lmer = lmer(mean.pos ~ 1 + (1|year) + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #16378
m2.lmer = lmer(mean.pos ~ year + (1|combo_NO), REML = F, data = pheno.variables)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first spring tavg
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.000

# next mid season tavg
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.000

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.000000 

m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm) 
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.000000

mtest = lmer(mean.pos ~ Treatment + year + (1|combo_NO),REML = F, data = pheno.variables)
anova(m2.lmer,mtest)# p = 0.03

# since we are not going to use elevation, the only fixed effect outside treatment will be
# summer min temp
m1.lmer = lmer(mean.pos ~ year + spring.Tavg_C + spring.PPT_mm + summer.Tavg_C + summer.PPT_mm + (1|combo_NO), REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.0005 ################################ need to figure out what this means!!!
confint(m2.lmer) # all include 0
car::Anova(m2.lmer,type='II') #treatment is allegedly important
emmeans(m2.lmer, ~ Treatment) # they all overlap
pairs(emmeans(m2.lmer, ~ Treatment)) # no significant differences
plot(emmeans(m2.lmer, ~ Treatment))
plot(m1.lmer, Treatment ~ resid(.), abline = 0 ) # generate diagnostic plots

# seeing a lot of outliers in early and summer

plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | Treatment, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# looks pretty random
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
##############################3
# next with sos

#temperature and elevation are super related, should not be used together

# model with site and and year as random effect and no fixed effects
m1.lmer = lmer(mean.sos ~ year + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #17041

m2.lmer = lmer(mean.sos ~ year + (1|combo_NO), REML = F, data = pheno.variables)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') # significant

# going to skip summer since sos are before summer

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.000003 -> include

# so spring tmin and ppt are both in
m3.lmer = update(m2.lmer, .~. + spring.Tavg_C + spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.00001 -> wooo

mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.67

m1.lmer = lmer(mean.sos ~ year + (1|combo_NO) + spring.Tavg_C + spring.PPT_mm, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.053 hmmmmmmm

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.51

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.51
##### it would appear that there is nothing for treatment and start of season.....

##############################3
# next with eos

#temperature and elevation are super related, should not be used together

# model with site and and year as random effect and no fixed effects
m1.lmer = lmer(mean.eos ~ 1 + (1|year) + (1|combo_NO), REML = F, data = pheno.variables)
AIC(logLik(m2.lmer)) #17041

m2.lmer = lmer(mean.eos ~ year + (1|combo_NO), REML = F, data = pheno.variables)
# now we need to figure out our fixed effects! Treatment is of the most interest but lets see if there are any correlation issues first

# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.001

# summer 
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.14

# fall
m3.lmer = update(m2.lmer, .~.+ fall.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.00000

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.98 -> dont include

m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.0000 -> dont include

m3.lmer = update(m2.lmer, .~.+ fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.01 -> include!

# so spring tavg, fall tavg, summer ppt and fall ppt are all in
m3.lmer = update(m2.lmer, .~. + spring.Tavg_C + fall.Tavg_C + summer.PPT_mm + fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.00000 -> wooo

mtest = update(m2.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.17


m1.lmer = lmer(mean.eos ~ year + (1|combo_NO) + spring.Tavg_C + fall.Tavg_C + summer.PPT_mm + fall.PPT_mm, REML = F, data = pheno.variables)
confint(m1.lmer) # only fall.PPT_mm doesn't contain 0

# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.26

m2.lmer = update(m1.lmer, .~.+ Treatment*spring.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.27

m2.lmer = update(m1.lmer, .~.+ Treatment*fall.Tavg_C)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.29 meh

m2.lmer = update(m1.lmer, .~.+ Treatment*fall.PPT_mm)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.11

##### wothout low sample size treatments it would seem there is nothing for end of season......

m1.lmer = lmer(mean.eos ~ year + (1|combo_NO) + spring.Tavg_C + fall.Tavg_C + summer.PPT_mm + fall.PPT_mm, REML = F, data = pheno.variables)
# lets look at treatment
m2.lmer = update(m1.lmer, .~.+ Treatment)
anova(m1.lmer,m2.lmer,test='Chi') ## p = 0.06
confint(m2.lmer)
## however upon looking at confints all treatment confints contain 0....
car::Anova(m2.lmer,type='II') #both are significant
emmeans(m2.lmer, ~ Treatment) # they all overlap
pairs(emmeans(m2.lmer, ~ Treatment)) # no significant differences
plot(emmeans(m2.lmer, ~ Treatment)) # no significant differences
summary(m2.lmer)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# combo_NO (Intercept) 101.7    10.09   
# Residual             708.7    26.62   
# Number of obs: 1636, groups:  combo_NO, 106
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                   -16.388979 299.284221  -0.055
# year                            0.175675   0.148974   1.179
# spring.Tavg_C                  -1.402676   0.501904  -2.795
# fall.Tavg_C                    -3.037936   0.641695  -4.734
# summer.PPT_mm                  -0.096775   0.029486  -3.282
# fall.PPT_mm                    -0.004963   0.021479  -0.231
# TreatmentEarly                 -6.696191   3.990153  -1.678
# TreatmentEarly.Late.Roatation  17.945852   9.603361   1.869
# TreatmentExclosure             -3.895893   7.880124  -0.494
# TreatmentLate                  -9.883751   5.454557  -1.812
# TreatmentRest.Rotation          5.718979  13.071435   0.438
# TreatmentSummer                -4.369270   4.150990  -1.053
# TreatmentSummer.Late.Rotation   9.455586   9.793146   0.966

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

################################ check out length of season??? ##########################################33
pheno.variables$mean.los = pheno.variables$mean.eos - pheno.variables$mean.sos
pheno.variables = pheno.variables[pheno.variables$mean.los < 300,]

m2.lmer = lmer(mean.los ~ year + (1|combo_NO), REML = F, data = pheno.variables)
# first early season tmax/tmin
m3.lmer = update(m2.lmer, .~.+ spring.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.001

# summer 
m3.lmer = update(m2.lmer, .~.+ summer.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.0000

# fall
m3.lmer = update(m2.lmer, .~.+ fall.Tavg_C)
anova(m2.lmer,m3.lmer,test='Chi') #0.03

# now precip
m3.lmer = update(m2.lmer, .~.+ spring.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.03

m3.lmer = update(m2.lmer, .~.+ summer.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.89

m3.lmer = update(m2.lmer, .~.+ fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.0000000000

# so spring tavg, fall tavg, summer ppt and fall ppt are all in
m3.lmer = update(m2.lmer, .~. + spring.Tavg_C + summer.Tavg_C + fall.Tavg_C + spring.PPT_mm + fall.PPT_mm)
anova(m2.lmer,m3.lmer,test='Chi') # p = 0.00000 -> wooo
car::Anova(m3.lmer,type='II') # based on this lets drop summer temp and fall temp

m1.lmer = lmer(mean.los ~ year + spring.Tavg_C + spring.PPT_mm + fall.PPT_mm + (1|combo_NO), data = pheno.variables)
car::Anova(m1.lmer,type='II') # looks like get rid of spring precip

m1.lmer = lmer(mean.los ~ year + spring.Tavg_C + fall.PPT_mm + (1|combo_NO), data = pheno.variables)
car::Anova(m1.lmer,type='II') # looks like get rid of spring precip

mtest = update(m1.lmer, .~.+ Treatment)
anova(m2.lmer,mtest)# p = 0.0000000 ## wowza
car::Anova(mtest,type='II') # shows treatment as being very unimportant
emmeans(mtest,~Treatment)
plot(emmeans(mtest,~Treatment))
pairs(emmeans(mtest,~Treatment)) # no significant differences

mtest = update(m1.lmer, .~.+ spring.Tavg_C*Treatment)
anova(m2.lmer,mtest)# p = 0.0000000 ## wowza
car::Anova(mtest,type='II') # shows treatment and the interaction as being very unimportant
emmeans(mtest,~spring.Tavg_C*Treatment)
plot(emmeans(mtest,~spring.Tavg_C*Treatment))
pairs(emmeans(mtest,~spring.Tavg_C*Treatment)) # no significant differences

mtest = update(m1.lmer, .~.+ fall.PPT_mm*Treatment)
anova(m2.lmer,mtest)# p = 0.0000000 ## wowza
car::Anova(mtest,type='II') # shows treatment and the interaction as being very unimportant
emmeans(mtest,~spring.Tavg_C*Treatment)
plot(emmeans(mtest,~spring.Tavg_C*Treatment))
pairs(emmeans(mtest,~spring.Tavg_C*Treatment)) # no significant differences
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
