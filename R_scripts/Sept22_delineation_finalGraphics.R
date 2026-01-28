library(ggplot2)
library(dplyr)

dat = read.csv("C:/Users/Sam/Desktop/delineation_test_0925.csv")

colors = c("Hand Delineation" = "black", "Intersect" = "red", "Clip" = "blue")

# % Riparian for hand, all unsupervised, and RF
ggplot() +
  geom_point(aes(dat$hand_del_pc, dat$PAST_NAME,color = 'Hand Delineation'))+
  geom_point(aes(dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc, dat$PAST_NAME,color = 'Intersect'))+
  geom_point(aes(dat$oo_TEST_RF_allBands_1921_NHDclip_30_pc, dat$PAST_NAME,color='Clip'))+
  labs(x = 'Percent Riparian',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)

Intersect_RMSE = sqrt(mean((dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc - dat$hand_del_pc)^2))
Clip_RMSE = sqrt(mean((dat$oo_TEST_RF_allBands_1921_NHDclip_30_pc - dat$hand_del_pc)^2))

# Error for unsupervised vs RF with RMSE added as text
ggplot() +
  geom_point(aes(sqrt((dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color = 'Intersect'))+
  geom_point(aes(sqrt((dat$oo_TEST_RF_allBands_1921_NHDclip_30_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color='Clip'))+
  labs(x = 'Root Squared Error',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)+
  annotate('label',label=paste0('Intersect RMSE = ',round(Intersect_RMSE,2)), x = 1.2,y='Dump',fill='white')+
  annotate('label',label=paste0('Clip RMSE = ',round(Clip_RMSE,2)), x = 1.2,y='Diamond Creek',fill='white')

## the best is allBands_1921_NHDintersect!

colors = c("Hand Delineation" = "black", "permInt" = "red", "onlyPerm" = "blue")

# % Riparian for hand, all unsupervised, and RF
ggplot() +
  geom_point(aes(dat$hand_del_pc, dat$PAST_NAME,color = 'Hand Delineation'))+
  geom_point(aes(dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc, dat$PAST_NAME,color = 'permInt'))+
  geom_point(aes(dat$oo_TEST_RF_allBands_1921_NHDintersect_perm_pc, dat$PAST_NAME,color='onlyPerm'))+
  labs(x = 'Percent Riparian',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)

permInt_RMSE = sqrt(mean((dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc - dat$hand_del_pc)^2))
onlyPerm = sqrt(mean((dat$oo_TEST_RF_allBands_1921_NHDintersect_perm_pc - dat$hand_del_pc)^2))

# Error for unsupervised vs RF with RMSE added as text
ggplot() +
  geom_point(aes(sqrt((dat$oo_TEST_RF_allBands_1921_NHDintersect_30_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color = 'permInt'))+
  geom_point(aes(sqrt((dat$oo_TEST_RF_allBands_1921_NHDintersect_perm_pc - dat$hand_del_pc)^2), dat$PAST_NAME,color='onlyPerm'))+
  labs(x = 'Root Squared Error',y='Pasture Name',color = 'Legend') +
  scale_color_manual(values = colors)+
  annotate('label',label=paste0('permInt RMSE = ',round(permInt_RMSE,2)), x = 2.5,y='Dump',fill='white')+
  annotate('label',label=paste0('onlyPerm RMSE = ',round(onlyPerm,2)), x = 2.5,y='Diamond Creek',fill='white')
