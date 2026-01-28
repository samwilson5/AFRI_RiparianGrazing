library(phenofit)
library(dplyr)
library(ggplot2)
library(stringr)

######### need to fix nptperyear
modis = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Modis_centerRidgeA.csv")
modis = modis[,c(1,2,5)]
modis = modis %>% 
  rename(date = system.time_start,
         QC = StateQA_mode,
         y = NDVI_mean)
modis$t = as.Date(modis$date, format = '%b %d, %Y')
modis$QC = as.numeric(gsub(',', '', modis$QC))

modis$QC_flag = qc_StateQA(modis$QC)$QC_flag
modis$w = qc_StateQA(modis$QC)$w
modis = modis[,-3]
d = modis

lambda         <- 15
nptperyear     <- 23
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0.2
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

INPUT <- check_input(d$t, d$y, d$w,
                     QC_flag = d$QC_flag,
                     nptperyear = nptperyear,
                     maxgap = nptperyear / 4, wmin = 0.2
)

brks <- season_mov(INPUT,
                   list(FUN = "smooth_wWHIT", wFUN = wFUN,
                        maxExtendMonth = 3,
                        wmin = wmin, r_min = 0.1
                   ))

## 2.4 Curve fitting
fit <- curvefits(INPUT, brks,
                 list(
                   methods = methods_fine, # ,"klos",, 'Gu'
                   wFUN = wFUN,
                   iters = 2,
                   wmin = wmin,
                   # constrain = FALSE,
                   nextend = 2,
                   maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                   minPercValid = minPercValid
                 ))

## check the curve fitting parameters
l_param <- get_param(fit)
print(l_param$Beck)
dfit <- get_fitting(fit)
print(dfit)

## 2.5 Extract phenology
TRS <- c(0.1, 0.2, 0.5)
l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) # %>% map(~melt_list(., "meth"))
print(l_pheno$doy$Beck)

pheno <- l_pheno$doy %>% melt_list("meth")

plot_season(INPUT, brks, ylab = "NDVI")

# fine curvefitting
g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "EVI", angle = 0)
grid::grid.newpage()
grid::grid.draw(g)

l_pheno2 <- get_pheno(fit[1:7], #method = "AG", 
                      TRS = TRS, IsPlot = TRUE, show.title = FALSE)
write.csv(pheno,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_MODIS_pheno.csv")

##### clear cache ##############################################################
library(zoo)
library(phenofit)
library(dplyr)
library(ggplot2)
library(stringr)

landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/Landsat_centerRidgeA.csv")
#landsat = landsat[complete.cases(landsat),]
landsat = landsat[-473,]
landsat = landsat %>% 
  rename(date = DATE_ACQUIRED,
         y = NDVI)
landsat$t = as.Date(landsat$date, format = '%b %d, %Y')
landsat$w = 1.0
landsat$w = ifelse(is.na(landsat$y)==T,0.2,landsat$w)
landsat$y = na.approx(landsat$y,na.rm=F)
landsat$w = ifelse(is.na(landsat$y)==T,0,landsat$w)
landsat$y = ifelse(is.na(landsat$y)==T,0,landsat$y)
d = landsat

#lambda         <- 5
nptperyear     <- 22
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

INPUT <- check_input(d$t, d$y, d$w,
                     #QC_flag = d$QC_flag,
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
                   methods = methods_fine, # ,"klos",, 'Gu'
                   wFUN = wFUN,
                   iters = 2,
                   wmin = wmin,
                   # constrain = FALSE,
                   nextend = 2,
                   maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                   minPercValid = minPercValid
                 ))

## check the curve fitting parameters
l_param <- get_param(fit)
print(l_param$Beck)
dfit <- get_fitting(fit)
print(dfit)

## 2.5 Extract phenology
TRS <- 0.2#c(0.1, 0.2, 0.5)
l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) # %>% map(~melt_list(., "meth"))
print(l_pheno$doy$Beck)

pheno <- l_pheno$doy %>% melt_list("meth")

plot_season(INPUT, brks, ylab = "NDVI")

# fine curvefitting
g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "NDVI", angle = 0)
grid::grid.newpage()
grid::grid.draw(g)

l_pheno <- get_pheno(fit[1:7], #method = "GU", 
                     TRS = TRS, IsPlot = TRUE, show.title = FALSE)

l_pheno <- get_pheno(fit[7], #method = "GU", 
                     TRS = TRS, IsPlot = TRUE, show.title = T)

fitted = data.frame(doy = fit[["2006_1"]][["model"]][["AG"]][["tout"]],
                    NDVI = fit[["2006_1"]][["model"]][["AG"]][["zs"]][["iter2"]])
unfitted = data.frame(ogNDVI = fit[["2006_1"]][["data"]][["y"]],
                      ofDOY = fit[["2006_1"]][["data"]][["t"]])
unfitted$Date = as.Date(unfitted$ofDOY, origin = '2000-01-01')
fitted$Date = as.Date(fitted$doy, origin = '2000-01-01')

p <- ggplot() +
  geom_line(aes(x=unfitted$Date,y=unfitted$ogNDVI),alpha=0.4,)+
  labs(x='Date',y='NDVI')+
  geom_point(aes(x=unfitted$Date,y=unfitted$ogNDVI))+
  geom_line(aes(x=fitted$Date,y=fitted$NDVI),linewidth=1.1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.sos"]],color='darkgreen',linewidth=1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.eos"]],color='red',linewidth=1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["DER.pos"]],color='blue',linewidth=1)+
  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.sos"]] + 20,
           y = 0.1, label = paste0('SOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.sos"]],format='%m/%d')),color = "darkgreen")+
  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.eos"]] + 20,
           y = 0.22, label = paste0('EOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.eos"]],format='%m/%d')),color = "red")+
  annotate("text", x = l_pheno[["date"]][["AG"]][["DER.pos"]] + 20,
           y = 0.65, label = paste0('POS: ',format(l_pheno[["date"]][["AG"]][["DER.pos"]],format='%m/%d')),color = "blue")
  

p

write.csv(pheno,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno.csv")

##### clear cache ##############################################################
library(zoo)
landsat = read.csv("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/Landsat_centerRidgeA_testAdjacent.csv")
#landsat = landsat[complete.cases(landsat),]
landsat = landsat[-473,]
landsat = landsat %>% 
  rename(date = DATE_ACQUIRED,
         y = NDVI)
landsat$t = as.Date(landsat$date, format = '%b %d, %Y')
landsat$w = 1.0
landsat$w = ifelse(is.na(landsat$y)==T,0.2,landsat$w)
landsat$y = na.approx(landsat$y,na.rm=F)
landsat$w = ifelse(is.na(landsat$y)==T,0,landsat$w)
landsat$y = ifelse(is.na(landsat$y)==T,0,landsat$y)
landsat$y = ifelse(landsat$y < 0,0,landsat$y)
d = landsat

#lambda         <- 5
nptperyear     <- 22
minExtendMonth <- 0.5
maxExtendMonth <- 1
minPercValid   <- 0
wFUN           <- wTSM # wBisquare
wmin           <- 0
methods_fine <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

INPUT <- check_input(d$t, d$y, d$w,
                     #QC_flag = d$QC_flag,
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
                   methods = methods_fine, # ,"klos",, 'Gu'
                   wFUN = wFUN,
                   iters = 2,
                   wmin = wmin,
                   # constrain = FALSE,
                   nextend = 2,
                   maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                   minPercValid = minPercValid
                 ))

## check the curve fitting parameters
l_param <- get_param(fit)
print(l_param$Beck)
dfit <- get_fitting(fit)
print(dfit)

## 2.5 Extract phenology
TRS <- 0.2#c(0.1, 0.2, 0.5)
l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) # %>% map(~melt_list(., "meth"))
print(l_pheno$doy$Beck)

pheno <- l_pheno$doy %>% melt_list("meth")

plot_season(INPUT, brks, ylab = "NDVI")

# fine curvefitting
g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "EVI", angle = 0)
grid::grid.newpage()
grid::grid.draw(g)

l_pheno <- get_pheno(fit[1:7], #method = "GU", 
                     TRS = TRS, IsPlot = TRUE, show.title = FALSE)


l_pheno <- get_pheno(fit[7], #method = "GU", 
                     TRS = TRS, IsPlot = TRUE, show.title = T)

fitted = data.frame(doy = fit[["2006_1"]][["model"]][["AG"]][["tout"]],
                    NDVI = fit[["2006_1"]][["model"]][["AG"]][["zs"]][["iter2"]])
unfitted = data.frame(ogNDVI = fit[["2006_1"]][["data"]][["y"]],
                      ofDOY = fit[["2006_1"]][["data"]][["t"]])
unfitted$Date = as.Date(unfitted$ofDOY, origin = '2000-01-01')
fitted$Date = as.Date(fitted$doy, origin = '2000-01-01')

p <- ggplot() +
  geom_line(aes(x=unfitted$Date,y=unfitted$ogNDVI),alpha=0.4,)+
  labs(x='Date',y='NDVI')+
  geom_point(aes(x=unfitted$Date,y=unfitted$ogNDVI))+
  geom_line(aes(x=fitted$Date,y=fitted$NDVI),linewidth=1.1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.sos"]],color='darkgreen',linewidth=1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["TRS2.eos"]],color='red',linewidth=1)+
  geom_vline(xintercept = l_pheno[["date"]][["AG"]][["DER.pos"]],color='blue',linewidth=1)+
  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.sos"]] + 20,
           y = 0.1, label = paste0('SOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.sos"]],format='%m/%d')),color = "darkgreen")+
  annotate("text", x = l_pheno[["date"]][["AG"]][["TRS2.eos"]] + 20,
           y = 0.22, label = paste0('EOS: ',format(l_pheno[["date"]][["AG"]][["TRS2.eos"]],format='%m/%d')),color = "red")+
  annotate("text", x = l_pheno[["date"]][["AG"]][["DER.pos"]] + 20,
           y = 0.65, label = paste0('POS: ',format(l_pheno[["date"]][["AG"]][["DER.pos"]],format='%m/%d')),color = "blue")


p

write.csv(pheno,"C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/CenterRidgeA_initial/CRA_Landsat_pheno_TESTADJACENT.csv")
