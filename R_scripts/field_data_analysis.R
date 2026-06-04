library(dplyr)
library(ggplot2)
library(Kendall)
library(ggpubr)

################ Perform Mann-Kendall tests on field variables #############################
# Load in field data
field.data = read.csv('Data/SalmonFieldData_combined.csv')

for(i in 1:nrow(field.data)){
  if(field.data[i,4]=='092-02-K'){
    field.data[i,2] = 'Lower Cow Creek 02-K'
  }
  if(field.data[i,4]=='092-02-I'){
    field.data[i,2] = 'Lower Cow Creek 02-I'
  }
}

# For combining field data protocols, this summarises the data for when there were multiple
# measures for a single year
# also filter out pastures that have less than 4 observations
field.sum = field.data %>%
  group_by(MIM.siteName,PIBO.siteName,Year) %>%
  summarise(#MIM.siteName = MIM.siteName,
            #PIBO.siteName = PIBO.siteName,
            Unique = paste0(first(MIM.siteName),'_',first(PIBO.siteName)),
            Allotment = first(Allotment),
            Pasture = first(Pasture),
            GGW = mean(GGW),
            bankfull.width = mean(bankfull.width),
            D50 = mean(D50),
            percent.fines = mean(percent.fines),
            covered.stable = mean(covered.stable),
            stable = mean(stable),
            wetland.rating = mean(wetland.rating)) %>% 
  group_by(MIM.siteName,PIBO.siteName) %>%
  filter(n() > 3)

# Get a list of all pastures that we have data for
pasts = unique(field.sum$Unique)

# create columns for the p-values to be stored by pasture and by metric
field.sum[c('GGW.MK','bankfull.width.MK','D50.MK',
            'percent.fines.MK','covered.stable.MK','stable.MK',
            'wetland.rating.MK')] = 0
measures = c('GGW','bankfull.width','D50','percent.fines',
             'covered.stable','stable','wetland.rating')

# Loop through each metric for each pasture and perform a Mann-Kendall test
# to identify significant trends
# return NA if too few observations
for(i in pasts){
  locs = which(field.sum$Unique == i)
  df.lim = field.sum[locs,]
  for(k in measures){
    pvalue = ifelse(
      isTRUE(grep('WARNING',capture.output(MannKendall(unlist(df.lim[k]))))==1),
      NA,
      MannKendall(unlist(df.lim[k]))$sl
    )
    co = paste0(k,'.MK')
    field.sum[locs,co] = pvalue
  }
}

# save csv with p-values
write.csv(field.sum,'Data/SFO_fieldData_MannKendall.csv')

# Create a plot item for each pasture and metric, graphing the metric over time
# also adding text about the significance of the relationship for each graph
plot_list <- lapply(split(field.sum, field.sum$Pasture), function(x)
{
  #if(is.na(x$wetland.rating.MK[1])){
  #  print('skip')
  #}
  if(!is.na(x$percent.fines.MK[1])){
    print("don't skip")
    ggplot(x, aes(Year, percent.fines)) +
    geom_line(data=x[!is.na(x$percent.fines),]) +
    geom_point() +
    ylim(0,1) + 
    xlim(2000,2023)+
    ggtitle(x$Pasture)+
    annotate('text',x = 2008,y=.15,label = paste0('p = ',round(x$percent.fines.MK[1],3)),size=4)}
  else{NA}
})
plot_list = plot_list[!is.na(plot_list)]
for(i in 1:length(plot_list)){print(plot_list[[i]])}
# Loop through each graph item and save it to the relevant folder
for(i in 1:length(plot_list)){
  title = plot_list[[i]]$data$Pasture[1]
  png(paste0(title,'.png'))
  print(plot_list[[i]])
  dev.off()
}

################################################################################
#### Perform NMDS and assess differneces in scaled variables between treatments ###########
library(vegan)
library(dplyr)
library(stringr)
library(ggplot2)

# Load in field data
field.data = read.csv('Data/SalmonFieldData_combined.csv')

# cow creek has two sites, need to make sure they stay separate
for(i in 1:nrow(field.data)){
  if(field.data[i,4]=='092-02-K'){
    field.data[i,2] = 'Lower Cow Creek 02-K'
  }
  if(field.data[i,4]=='092-02-I'){
    field.data[i,2] = 'Lower Cow Creek 02-I'
  }
}

# For combining field data protocols, this summarizes the data for when there were multiple
# measures for a single year
# also filter out pastures that have less than 4 observations
field.sum = field.data %>%
  group_by(MIM.siteName,PIBO.siteName,Year) %>%
  summarise(#MIM.siteName = MIM.siteName,
    #PIBO.siteName = PIBO.siteName,
    Unique = paste0(first(MIM.siteName),'_',first(PIBO.siteName)),
    Allotment = first(Allotment),
    Pasture = first(Pasture),
    GGW = mean(GGW),
    bankfull.width = mean(bankfull.width),
    D50 = mean(D50),
    percent.fines = mean(percent.fines),
    covered.stable = mean(covered.stable),
    stable = mean(stable),
    wetland.rating = mean(wetland.rating)) %>% 
  group_by(MIM.siteName,PIBO.siteName) %>%
  filter(n() > 3)

## switch this to alltreatments and move that csv into the data folder
#treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/salmonFieldData_treatments.csv')
treatments = read.csv('Data/SFO_allTreatments.csv')
treatments$Pasture = str_replace(treatments$Pasture,'W\\. Sandy','West Sandy')
treatments$Allotment = str_replace(treatments$Allotment,'Ryegrass\\/North Hayden','Ryegrass')
treatments$Pasture = str_replace(treatments$Pasture,'Ryegrass BLM','Ryegrass')
treatments$Pasture = str_replace(treatments$Pasture,'Ramsey MTN\\.','Ramsey Mountain')
treatments$Pasture = str_replace(treatments$Pasture,'Upper Reese','Upper Reese Creek')

field.sum.treat = field.sum %>% left_join(treatments, join_by(Allotment == Allotment, Pasture == Pasture))
field.sum.treat$Treatment = ifelse(field.sum.treat$Treatment == '',
                                   NA,
                                   field.sum.treat$Treatment)

field.sum.treat = field.sum.treat[!is.na(field.sum.treat$Treatment),]

# not enough observations to justify keeping Rest.Rotation
field.sum.treat = field.sum.treat[field.sum.treat$Treatment!= 'Rest.Rotation',]

nmds.field = field.sum.treat
nmds.field = nmds.field[!is.na(nmds.field$bankfull.width),]
nmds.field = nmds.field[!is.na(nmds.field$D50),]
nmds.field = nmds.field[!is.na(nmds.field$stable),]
nmds.field = nmds.field[!is.na(nmds.field$wetland.rating),]
#nmds.field = nmds.field[!is.na(nmds.field$percent.fines),]

#make community matrix - extract columns with abundance information
com = nmds.field[,c(8,9,11,13)]
#com = nmds.field[,c(8,9,11)]

#turn abundance data frame into a matrix
m_com = as.matrix(com)

set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
plot(nmds)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores = as.data.frame(scores(nmds)$sites)

#add columns to data frame 
data.scores$Treatment = nmds.field$Treatment

# metric correlations
metric.fit <- envfit(nmds, m_com, permutations = 999)
head(metric.fit)
metric.fit.scores <- as.data.frame(scores(metric.fit, display = "vectors"))
metric.fit.scores <- cbind(metric.fit.scores, metric.variables = rownames(metric.fit.scores))

metric.fit.scores.labels = metric.fit.scores
metric.fit.scores.labels[1:4,1] = c(0.265,0.3,-0.11,-0.255)
metric.fit.scores.labels[1:4,2] = c(-0.305,0.25,0.195,-0.165)
metric.fit.scores.labels[1:4,3] = c('bankfull width','median substrate size','% stable bank','wetland rating')
# ordination plot


ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 18, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        #legend.position = 'inside',
        #legend.position.inside = c(0.85,0.85),
        legend.position = 'bottom',
        axis.title.x = element_text(face = "bold", size = 18, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 18, colour = "black"),
        legend.title = element_text(size = 22, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Grazing Timing", y = "NMDS2")+
  geom_segment(data = metric.fit.scores, aes(x = 0, xend=NMDS1*0.4, y=0, yend=NMDS2*0.4), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  ggrepel::geom_text_repel(data = metric.fit.scores.labels, aes(x=NMDS1, y=NMDS2, label = metric.variables), 
                           cex = 5, direction = "both", segment.size = 0.25)



# lets use a permanova to see if there is a difference between groups in this space
adonis2(data.scores[,c('NMDS1','NMDS2')] ~ data.scores$Treatment,
        method = 'euc') # p = 0.004 significant difference

# we will use betadisper to see if there is significant differnce between dispersion
# if not then the permanova is likely pointing to differences in centroids

# use the betadisper function from 'vegan' to measure the distances between points
# and the centroid of their group
ordinationDist = betadisper(d = dist(data.scores[,c('NMDS1','NMDS2')]),
                            group = data.scores$Treatment,
                            type = 'centroid')
ordinationDist$distances
plot(ordinationDist)

metric.fit <- envfit(nmds, m_com, permutations = 999)
head(metric.fit)
plot(ordinationDist)
plot(metric.fit)

# correlation coefficients
cor.coef <-
  cor(m_com,
      nmds$points,
      use = "complete.obs",
      method = "pearson")

# is there a difference in dispersion between groups?
anova(ordinationDist) #p-value equals 0.3 so no difference in dispersion
adonis2(dist(ordinationDist$distances) ~ data.scores$Treatment) #p = 0.09

TukeyHSD(ordinationDist) # no significant differences between any groups in dispersion

summary(aov(data.scores$NMDS1 ~ data.scores$Treatment))
summary(aov(data.scores$NMDS2 ~ data.scores$Treatment))

# generally, there seems to be significant differences in centroid but not in dispersion

#################################################################################
## Assess differences in "treatment effect" or just difference between first and last observation
# now lets look at just beginning and end data 
# Load in field data
field.data = read.csv('Data/SalmonFieldData_combined.csv')
treatments = read.csv('Data/SFO_alltreatments.csv')

# correct a few differences in naming conventions between different BLM data
treatments$Pasture = str_replace(treatments$Pasture,'W\\. Sandy','West Sandy')
treatments$Allotment = str_replace(treatments$Allotment,'Ryegrass\\/North Hayden','Ryegrass')
treatments$Pasture = str_replace(treatments$Pasture,'Ryegrass BLM','Ryegrass')
treatments$Pasture = str_replace(treatments$Pasture,'Ramsey MTN\\.','Ramsey Mountain')
treatments$Pasture = str_replace(treatments$Pasture,'Upper Reese','Upper Reese Creek')

for(i in 1:nrow(field.data)){
  if(field.data[i,4]=='092-02-K'){
    field.data[i,2] = 'Lower Cow Creek 02-K'
  }
  if(field.data[i,4]=='092-02-I'){
    field.data[i,2] = 'Lower Cow Creek 02-I'
  }
}

# summarise the data where multiple measurements exist for a single year
# filter out sites that have less than 4 observations
field.sum = field.data %>%
  group_by(MIM.siteName,PIBO.siteName,Year) %>%
  summarise(#MIM.siteName = MIM.siteName,
    #PIBO.siteName = PIBO.siteName,
    Unique = paste0(first(MIM.siteName),'_',first(PIBO.siteName)),
    Allotment = first(Allotment),
    Pasture = first(Pasture),
    GGW = mean(GGW),
    bankfull.width = mean(bankfull.width),
    D50 = mean(D50),
    percent.fines = mean(percent.fines),
    covered.stable = mean(covered.stable),
    stable = mean(stable),
    wetland.rating = mean(wetland.rating)) %>% 
  group_by(MIM.siteName,PIBO.siteName) %>%
  filter(n() > 3)

# join treatments to the field data
field.sum.treat = field.sum %>% left_join(treatments, join_by(Allotment == Allotment, Pasture == Pasture))
#field.sum.treat$Treatment = ifelse(field.sum.treat$Treatment == '',
#                                   NA,
#                                   field.sum.treat$Treatment)

# throw out NA's
field.sum.treat = field.sum.treat[!is.na(field.sum.treat$Treatment),]


past_list = unique(field.sum.treat$Unique)

# function to find the difference between last and first field measure
find.change = function(list){
  list2 = na.omit(list)
  if(length(list2) == 0){
    return(NA)
  }else{
  list.length = length(list2)
  list.change = list2[list.length] - list2[1]
  return(list.change)}
}
# positive means increase, negative means decrease
change.df = data.frame(unique = character(),
                       Treatment = character(),
                       years = numeric(),
                       bankfull.width = numeric(),
                       D50 = numeric(),
                       stable = numeric(),
                       wetland.rating = numeric())

for(i in past_list){
  df.limit = field.sum.treat[field.sum.treat$Unique == i,]
  graze = df.limit$Treatment[1]
  year.change = find.change(df.limit$Year)
  bankfull.width.change = find.change(df.limit$bankfull.width)
  D50.change = find.change(df.limit$D50)
  stable.change = find.change(df.limit$stable)
  wetland.change = find.change(df.limit$wetland.rating)
  df.new = data.frame(unique = i,
                      Treatment = graze,
                      years = year.change,
                      bankfull.width = bankfull.width.change,
                      D50 = D50.change,
                      stable = stable.change,
                      wetland.rating = wetland.change)
  change.df = rbind(change.df,df.new)
}

# not enough observations to keep Rest.Rotation, Late,Summer.Late.Rotation or Late.Rotation
change.df = change.df[change.df$Treatment!= 'Rest.Rotation',]
change.df = change.df[change.df$Treatment!= 'Late',]
change.df = change.df[change.df$Treatment!= 'Late.Rotation',]
change.df = change.df[change.df$Treatment!= 'Summer.Late.Rotation',]

adonis2(change.df[,3:6] ~ change.df$Treatment, na.rm = T,
        method = 'euc') # p = 0.016 significant difference


summary(aov(change.df$bankfull.width ~ change.df$Treatment)) #p = 0.66
ggplot(change.df, aes(x = Treatment, y = bankfull.width)) + 
  geom_boxplot()

summary(aov(change.df$D50 ~ change.df$Treatment)) #p = 0.02
TukeyHSD(aov(change.df$D50 ~ change.df$Treatment)) # exclosure and summer are different, exclosure and early and somewhat different 
ggplot(change.df, aes(x = Treatment, y = D50)) + 
  theme_bw()+
  geom_boxplot()+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"),
        axis.title.y = element_text(face = "bold", size = 14, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))+
  labs(x = 'Treatment',y = 'Median Substrate Effect Size')

summary(aov(change.df$stable ~ change.df$Treatment)) #p = 0.97
ggplot(change.df, aes(x = Treatment, y = stable)) + 
  geom_boxplot()

summary(aov(change.df$wetland.rating ~ change.df$Treatment)) #p = 0.15
ggplot(change.df, aes(x = Treatment, y = wetland.rating)) + 
  geom_boxplot()
