library(dplyr)
library(ggplot2)
library(Kendall)
library(ggpubr)

# Load in field data
field.data = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SalmonFieldData_combined.csv')

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

# Loop through each metric for each pasture and perform a MannKendall test
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
#for(i in 1:length(plot_list)){print(plot_list[[i]])}
# Loop through each graph item and save it to the relevant folder
#for(i in 1:length(plot_list)){
#  title = plot_list[[i]]$data$Pasture[1]
#  png(paste0('C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/MannKendall_fieldData/wetland.rating/',title,'.png'))
#  print(plot_list[[i]])
#  dev.off()
#}

rows = ceiling(length(plot_list)/3)
png('C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/MannKendall_fieldData/percent.fines/combined.png')
ggarrange(plot_list[[1]],ncol = 1, nrow = rows)
dev.off()


#,plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
#plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],
#plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],
#plot_list[[16]],plot_list[[17]]
################################################################################################
# now lets look at only MIM data to see if things change??

# Load in field data
field.data = read.csv('C:/Users/Sam/OneDrive - University of Idaho/SalmonFieldData_combined.csv')

field.sum = field.data %>%
  filter(Protocol == 'MIM') %>% 
  group_by(Allotment,Pasture) %>%
  filter(n() > 3)

# Get a list of all pastures that we have data for
pasts = unique(field.sum$MIM.siteName)

# create columns for the p-values to be stored by pasture and by metric
field.sum[c('GGW.MK','bankfull.width.MK','D50.MK',
            'percent.fines.MK','covered.stable.MK','stable.MK',
            'wetland.rating.MK')] = 0
measures = c('GGW','bankfull.width','D50','percent.fines',
             'covered.stable','stable','wetland.rating')

# Loop through each metric for each pasture and perform a MannKendall test
# to identify significant trends
# return NA if too few observations
for(i in pasts){
  locs = which(field.sum$MIM.siteName == i)
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
write.csv(field.sum,'C:/Users/Sam/OneDrive - University of Idaho/MIM_analyzed.csv')
################################################################################################
# now lets look at only PIBO data to see if things change??

# Load in field data
field.data = read.csv('C:/Users/Sam/OneDrive - University of Idaho/SalmonFieldData_combined.csv')

field.sum = field.data %>%
  filter(Protocol == 'PIBO') %>% 
  group_by(PIBO.siteName) %>%
  filter(n() > 3)

# Get a list of all pastures that we have data for
pasts = unique(field.sum$PIBO.siteName)

# create columns for the p-values to be stored by pasture and by metric
field.sum[c('GGW.MK','bankfull.width.MK','D50.MK',
            'percent.fines.MK','covered.stable.MK','stable.MK',
            'wetland.rating.MK')] = 0
measures = c('GGW','bankfull.width','D50','percent.fines',
             'covered.stable','stable','wetland.rating')

# Loop through each metric for each pasture and perform a MannKendall test
# to identify significant trends
# return NA if too few observations
for(i in pasts){
  locs = which(field.sum$PIBO.siteName == i)
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

write.csv(field.sum,'C:/Users/Sam/OneDrive - University of Idaho/PIBO_analyzed.csv')




################################################################################
library(vegan)
library(dplyr)
# Load in field data
field.data = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SalmonFieldData_combined.csv')

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
    wetland.rating = mean(wetland.rating))

treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/salmonFieldData_treatments.csv')

field.sum.treat = field.sum %>% left_join(treatments, join_by(Allotment == Allotment, Pasture == Pasture))
field.sum.treat$Treatment = ifelse(field.sum.treat$Treatment == '',
                                   NA,
                                   field.sum.treat$Treatment)

field.sum.treat = field.sum.treat[!is.na(field.sum.treat$Treatment),]

nmds.field = field.sum.treat
nmds.field = nmds.field[!is.na(nmds.field$bankfull.width),]
nmds.field = nmds.field[!is.na(nmds.field$D50),]
nmds.field = nmds.field[!is.na(nmds.field$stable),]
#nmds.field = nmds.field[!is.na(nmds.field$wetland.rating),]
#nmds.field = nmds.field[!is.na(nmds.field$percent.fines),]

#make community matrix - extract columns with abundance information
#com = nmds.field[,c(8,9,11,13)]
com = nmds.field[,c(8,9,11)]

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

head(data.scores)
library(ggplot2)
ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Treatment", y = "NMDS2")

ordiplot(nmds,type="n")
ordihull(nmds,groups=data.scores$Treatment,draw="polygon",col="grey90",label=F)
orditorp(nmds,display = 'sites',col="red",air=0.01)

ordiplot(nmds,type="n")
ordiellipse(nmds,groups=data.scores$Treatment,draw="polygon",col="grey90",label=F)
orditorp(nmds,display = 'sites',col="red",air=0.01)

# lets use a permanova to see if there is a difference between groups in this space
adonis2(data.scores[,c('NMDS1','NMDS2')] ~ data.scores$Treatment,
        method = 'euc') # p = 0.001 significant difference

# we will use betadisper to see if there is significant differnce between dispersion
# if not then the permanova is likely pointing to differences in centroids

# use the betadisper function from 'vegan' to measure the distances between points
# and the centroid of their group
ordinationDist = betadisper(d = dist(data.scores[,c('NMDS1','NMDS2')]),
                            group = data.scores$Treatment,
                            type = 'centroid')
ordinationDist$distances
plot(ordinationDist)

# is there a difference in dispersion between groups?
anova(ordinationDist) #p-value equals 0.087 so there is some difference
adonis2(dist(ordinationDist$distances) ~ data.scores$Treatment) #p = 0.09

TukeyHSD(ordinationDist) # no significant differences between any groups in dispersion

summary(aov(data.scores$NMDS1 ~ data.scores$Treatment))
summary(aov(data.scores$NMDS2 ~ data.scores$Treatment))

# im concerned that the lack of samples in a few groups might be influencing things,
# gonna drop rest-rotation,rotation, and late and see what happens

data.scores = data.scores[data.scores$Treatment!= 'Late',]
data.scores = data.scores[data.scores$Treatment!= 'Rest.Rotation',]
data.scores = data.scores[data.scores$Treatment!= 'Rotation',]

# lets use a permanova to see if there is a difference between groups in this space
adonis2(data.scores[,c('NMDS1','NMDS2')] ~ data.scores$Treatment,
        method = 'euc') # p = 0.001 significant difference

# use the betadisper function from 'vegan' to measure the distances between points
# and the centroid of their group
ordinationDist = betadisper(d = dist(data.scores[,c('NMDS1','NMDS2')]),
                            group = data.scores$Treatment,
                            type = 'centroid')
ordinationDist$distances
plot(ordinationDist)

# is there a difference in dispersion between groups?
anova(ordinationDist) #p= 0.18
adonis2(dist(ordinationDist$distances) ~ data.scores$Treatment) #p = 0.17


# generally, there seems to be significant differences in centroid but not in dispersion

#################################################################################
# now lets look at just beginning and end data 
# Load in field data
field.data = read.csv('C:/Users/samwi/OneDrive - University of Idaho/SalmonFieldData_combined.csv')
treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/salmonFieldData_treatments.csv')
for(i in 1:nrow(field.data)){
  if(field.data[i,4]=='092-02-K'){
    field.data[i,2] = 'Lower Cow Creek 02-K'
  }
  if(field.data[i,4]=='092-02-I'){
    field.data[i,2] = 'Lower Cow Creek 02-I'
  }
}

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

field.sum.treat = field.sum %>% left_join(treatments, join_by(Allotment == Allotment, Pasture == Pasture))
field.sum.treat$Treatment = ifelse(field.sum.treat$Treatment == '',
                                   NA,
                                   field.sum.treat$Treatment)
field.sum.treat = field.sum.treat[!is.na(field.sum.treat$Treatment),]


past_list = unique(field.sum.treat$Unique)


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

adonis2(change.df[,3:6] ~ change.df$Treatment, na.rm = T,
        method = 'euc') # p = 0.001 significant difference

ggplot(change.df, aes(x = Treatment, y = bankfull.width)) + 
  geom_boxplot()

ggplot(change.df, aes(x = Treatment, y = D50)) + 
  geom_boxplot()

ggplot(change.df, aes(x = Treatment, y = stable)) + 
  geom_boxplot()

ggplot(change.df, aes(x = Treatment, y = wetland.rating)) + 
  geom_boxplot()

# looks like not much seperation except in D50 with exclosure

change.df = change.df[change.df$Treatment!= 'Continuous',]
change.df = change.df[change.df$Treatment!= 'Rest.Rotation',]

adonis2(change.df[,3:6] ~ change.df$Treatment, na.rm = T,
        method = 'euc') # p = 0.012

summary(aov(change.df$bankfull.width ~ change.df$Treatment)) #p = 0.46
ggplot(change.df, aes(x = Treatment, y = bankfull.width)) + 
  geom_boxplot()

summary(aov(change.df$D50 ~ change.df$Treatment)) #p = 0.02
TukeyHSD(aov(change.df$D50 ~ change.df$Treatment)) # exclosure is significantly different than early and summer, early and summer are not different
ggplot(change.df, aes(x = Treatment, y = D50)) + 
  geom_boxplot()+
  annotate('text',x = 'Exclosure',y=19,label = '*',
           size = 10)

summary(aov(change.df$stable ~ change.df$Treatment)) #p = 0.84
ggplot(change.df, aes(x = Treatment, y = stable)) + 
  geom_boxplot()

summary(aov(change.df$wetland.rating ~ change.df$Treatment)) #p = 0.27
ggplot(change.df, aes(x = Treatment, y = wetland.rating)) + 
  geom_boxplot()
