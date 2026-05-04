library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# this script compares the billed use to actual use for pastures in the Salmon Field Office

billed = read_xlsx('Data/Salmon_billedUse.xlsx')
actual = read.csv('Data/Salmon_actualUse.csv')

# some pastures have multiple leases for each year, so add them here
actual.summarised = actual %>%
  group_by(Allotment,Year) %>%
  summarise(actual.AUM = sum(AUM))

# upper case
actual.summarised$Allotment = toupper(actual.summarised$Allotment)

# need to replace 'E.' in allotment name with EAST for billed
# need to replace 'CR.' in allotment name with CREEK for billed
# 'MAMMOTH/SAWMILL' needs to be 'MAMMOTH-SAWMILL' for actual.summarised
# billed calls is 'NEF NO. 3' actual calls it 'NEF 3'
# need to replace 'N.' in allotment name with NORTH for billed
# need to replace 'SO.' in allotment name with SOUTH for billed

billed$ALLOTMENT_NAME = str_replace(billed$ALLOTMENT_NAME,'E\\.','EAST')
billed$ALLOTMENT_NAME = str_replace(billed$ALLOTMENT_NAME,'CR\\.','CREEK')
actual.summarised$Allotment = str_replace(actual.summarised$Allotment,'MAMMOTH/SAWMILL','MAMMOTH-SAWMILL')
actual.summarised$Allotment = str_replace(actual.summarised$Allotment,'NEF 3','NEF NO.3')
billed$ALLOTMENT_NAME = str_replace(billed$ALLOTMENT_NAME,'N\\.','NORTH')
billed$ALLOTMENT_NAME = str_replace(billed$ALLOTMENT_NAME,'SO\\.','SOUTH')

billed.limit = billed %>% 
  dplyr::select(billed_aums,ALLOTMENT_NAME,grazing_year) %>%
  rename(Allotment = ALLOTMENT_NAME,Year = grazing_year)

# joining billed and actual use, removing NA's
all.use = billed.limit %>% left_join(actual.summarised,join_by(Allotment==Allotment,Year==Year))
all.use = all.use[complete.cases(all.use),]

# linear model between billed and actual use
mod = lm(billed_aums~actual.AUM,data=all.use)
summary(mod) # R2 = 0.8

plot(all.use$billed_aums,all.use$actual.AUM)

p1 <- ggplot(all.use, aes(x = billed_aums, y = actual.AUM)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Frequency\n(Prepositions)")
p1
