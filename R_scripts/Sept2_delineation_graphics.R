dat = read.csv("C:/Users/Sam/Desktop/delineation_test.csv")

library(ggplot2)
library(dplyr)
colors = c("Hand Delineation" = "black", "All Unsupervised" = "red", "Mixed" = "blue")
dat = dat %>% rename(
  RF_allBands_area = Clipped_oo_TEST_allBands_area,
  RF_allBands_pc = Clipped_oo_TEST_allBands_pc,
  RF_allBands_NHD_area = Clipped_allBands_area,
  RF_allBands_NHD_pc = Clipped_allBands_pc,
  RF_RGB_area = Clipped_oo_TEST_RGB_area,
  RF_RGB_pc = Clipped_oo_TEST_RGB_pc,
  RF_RGB_NHD_area = Clipped_RGB_area,
  RF_RGB_NHD_pc = Clipped_RGB_pc,
  unsupervised_area = oo_TEST_noMod_area,
  unsupervised_pc = oo_TEST_noMod_pc,
  unsupervised_NHD_area = NHDClipped_noMod_area,
  unsupervised_NHD_pc = NHDClipped_noMod_pc,
  hand_del_NHD_area = NHDClipped_hand_del_area,
  hand_del_NHD_pc = NHDClipped_hand_del_pc
)

# % Riparian for hand, all unsupervised, and RF
ggplot() +
  geom_point(aes(dat$hand_del_pc, dat$PAST_NAME,color = 'Hand Delineation'))+
  geom_point(aes(dat$unsupervised_pc, dat$PAST_NAME,color = 'All Unsupervised'))+
  geom_point(aes(dat$RF_RGB_pc, dat$PAST_NAME,color='Mixed'))+
  labs(x = 'Percent Riparian',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)

noMod_RMSE = sqrt(mean((dat$unsupervised_pc - dat$hand_del_pc)^2))
RF_RMSE = sqrt(mean((dat$RF_RGB_pc - dat$hand_del_pc)^2))

# Error for unsupervised vs RF with RMSE added as text
ggplot() +
  geom_point(aes(sqrt((dat$unsupervised_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color = 'All Unsupervised'))+
  geom_point(aes(sqrt((dat$RF_RGB_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color='Mixed'))+
  labs(x = 'Root Squared Error',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)+
  annotate('label',label='All Unsupervised RMSE = 13.6', x = 30,y='Dump',fill='white')+
  annotate('label',label='Mixed RMSE = 2.51', x = 30,y='Diamond Creek',fill='white')

# % riparian for hand del, clipped unsupervised, clipped rf
ggplot() +
  geom_point(aes(dat$hand_del_pc, dat$PAST_NAME,color='Hand Delineation'))+
  geom_point(aes(dat$unsupervised_NHD_pc, dat$PAST_NAME,color = 'All Unsupervised'))+
  geom_point(aes(dat$RF_RGB_NHD_pc, dat$PAST_NAME,color='Mixed'))+
  labs(x = 'Percent Riparian',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)
  
noMod_RMSE = sqrt(mean((dat$unsupervised_NHD_pc - dat$hand_del_pc)^2))
RF_RMSE = sqrt(mean((dat$RF_RGB_NHD_pc - dat$hand_del_pc)^2))

ggplot() +
  geom_point(aes(sqrt((dat$unsupervised_NHD_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color = 'All Unsupervised'))+
  geom_point(aes(sqrt((dat$RF_RGB_NHD_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color='Mixed'))+
  labs(x = 'Root Squared Error',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)+
  annotate('label',label='All Unsupervised RMSE = 0.97', x = 2,y='Dump',fill='white')+
  annotate('label',label='Mixed RMSE = 0.62', x = 2,y='Diamond Creek',fill='white')

