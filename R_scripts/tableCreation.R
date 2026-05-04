library(gt)
prod.df = data.frame(
  'Response Variable' = c('Growing Season Integral','Growing Season Integral','Growing Season Integral','Annual Integral','Annual Integral','Annual Integral','Peak NDVI','Peak NDVI','Peak NDVI'),
  'Dependent Variable' = c('Temperature','Precipitation','Temperature + Precipitation + Treatment','Temperature','Precipitation','Temperature + Precipitation + Treatment','Temperature','Precipitation','Precipitation + Treatment'),
  'P-Value Compared to Base Model' = c('< 0.001','< 0.001','0.38','< 0.001','< 0.001','0.66','0.17','< 0.001','0.37'),
  check.names=F)
  
prod.tab = gt(prod.df) %>% tab_options(column_labels.font.weight = "bold")
prod.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/prod_reg_table.png')

phen.df = data.frame(
  'Response Variable' = c('Start of Growing Season','Start of Growing Season','Start of Growing Season','Peak of Growing Season','Peak of Growing Season','Peak of Growing Season','Peak of Growing Season','Peak of Growing Season','End of Growing Season','End of Growing Season','End of Growing Season','End of Growing Season','End of Growing Season','End of Growing Season','End of Growing Season'),
  'Dependent Variable' = c('Spring Temperature','Spring Precipitation','Spring Temperature + Spring Precipitation + Treatment','Spring Temperature','Summer Temperature','Spring Precipitation','Summer Precipitation','Spring Temperature + Summer Temperature + Spring Precipitation + Summer Precipitation + Treatment', 'Spring Temperature','Summer Temperature','Fall Temperature','Spring Precipitation','Summer Precipitation','Fall Precipitation','Spring Temperature + Fall Temperature + Fall Precipitation + Treatment'),
  'P-Value Compared to Base Model' = c('< 0.001','< 0.001','0.1','< 0.001','< 0.001','< 0.001','< 0.001','< 0.01','< 0.001','0.24','< 0.001','0.63','0.79','< 0.01','0.21'),
  check.names = F
)

phen.tab = gt(phen.df) %>% tab_options(column_labels.font.weight = "bold")
phen.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/phen_reg_table.png')

wood.df = data.frame(
  'Reponse Variable' = c('Woody Cover','Woody Cover','Woody Cover'),
  'Dependent Variable' = c('Temperature','Precipitation','Temperature + Treatment'),
  'P-Value Compared to Base Model' = c('< 0.001','0.12','0.49'),
  check.names = F
)
wood.tab = gt(wood.df) %>% tab_options(column_labels.font.weight = "bold")
wood.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/wood_reg_table.png')

mesic.df = data.frame(
  'Response Variable'=c('Mesic Extent','Mesic Extent','Mesic Extent'),
  'Dependent Variable'=c('Temperature','Precipitation','Precipitation + Treatment'),
  'P-Value Compared to Base Model'=c('0.96','< 0.001','< 0.001'),
  check.names=F
)

mesic.tab = gt(mesic.df) %>% tab_options(column_labels.font.weight = "bold")
mesic.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/mesic_reg_table.png')

summary.df = data.frame(
  'Remote Sensing Metric'=c('Growing Season Integral','Annual Integral','Peak NDVI','Start of Growing Season','Peak of Growing Season','End of Growing Season','Woody Cover','Mesic Extent'),
  'Final Model Variables'=c('Temperature + Precipitation + Treatment','Temperature + Precipitation + Treatment','Precipitation + Treatment','Spring Temperature + Spring Precipitation','Spring Temperature + Summer Temperature + Spring Precipitation + Summer Precipitation + Treatment','Spring Temperature + Fall Temperature + Fall Precipitation + Treatment','Temperature + Treatment','Precipitation + Treatment'),
  'P-Value Compared to Base Model'=c('0.38','0.66','0.37','0.1','0.002','0.21','0.49','<0.001'),
  check.names=F
)
sum.tab = gt(summary.df) %>% tab_options(column_labels.font.weight = "bold")
sum.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/summary_reg_table.png')

treat.df = data.frame(
  'Treatment' = c('Exclosure','Early Season','Summer','Late Season','Continuous'),
  'Dates Grazed' = c('NA','Before June 21','June 21 - September 21','After September 21','Before June 21 - After September 21'),
  check.names = F
)
treat.tab = gt(treat.df) %>% tab_options(column_labels.font.weight = "bold")
treat.tab %>% gtsave('C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/Writing/Tables/treatment_reg_table.png')
