library(dplyr)

burp.site.data = read.csv('C:/Users/samwi/OneDrive - University of Idaho/BURP_data/station/station.csv')
burp.site.data = burp.site.data %>% filter(OrganizationIdentifier == 'IDEQ_WQX')
burp.site.name = burp.site.data[,c('MonitoringLocationIdentifier',
                                   'MonitoringLocationName')]

burp.bio = read.csv('C:/Users/samwi/OneDrive - University of Idaho/BURP_data/biologicalresult/biologicalresult.csv')
burp.bio = burp.bio %>% filter(OrganizationIdentifier == 'IDEQ_WQX')
burp.bio = burp.bio %>% left_join(burp.site.name, join_by('MonitoringLocationIdentifier' == 
                                                            'MonitoringLocationIdentifier'))

burp.phys = read.csv('C:/Users/samwi/OneDrive - University of Idaho/BURP_data/resultphyschem/resultphyschem.csv')
burp.phys = burp.phys %>% filter(OrganizationIdentifier == 'IDEQ_WQX')

burp.narrow = read.csv('C:/Users/samwi/OneDrive - University of Idaho/BURP_data/narrowresult/narrowresult.csv')
burp.narrow = burp.narrow %>% filter(OrganizationIdentifier == 'IDEQ_WQX')
burp.narrow = burp.narrow %>% left_join(burp.site.name, join_by('MonitoringLocationIdentifier' == 
                                                            'MonitoringLocationIdentifier'))

sites = unique(burp.phys$MonitoringLocationIdentifier)
burp.bio$CharacteristicName.Limited = ifelse(grepl('Substrate -',burp.bio$CharacteristicName,fixed = T),
                                             'Substrate',
                                             burp.bio$CharacteristicName)
burp.bio$CharacteristicName.Limited = ifelse(burp.bio$CharacteristicName == 'RBP Stream width' | burp.bio$CharacteristicName == 'Depth',
                                             'Width.Depth',
                                             burp.bio$CharacteristicName.Limited)

full.burp = data.frame(data.frame(Site = as.character(),SubjectTaxonomicName = as.character(),Result = as.numeric(),Text.Result = as.character(),Measurement.Unit = as.character(),CharacteristicName = as.character(),Sample.Date = as.character()))
for(i in sites){
  df.limit = burp.bio[burp.bio$MonitoringLocationIdentifier == i,]
  dates = unique(df.limit$ActivityStartDate)
  date.intermediate = data.frame(SubjectTaxonomicName = as.character(),Result = as.numeric(),Text.Result = as.character(),Measurement.Unit = as.character(),CharacteristicName = as.character(),Sample.Date = as.character())
  for(k in dates){
    df.limit.date = df.limit[df.limit$ActivityStartDate == k,]
    measurements = unique(df.limit.date$CharacteristicName.Limited)
    result = data.frame(Result = as.numeric(),Text.Result = as.character(),Measurement.Unit = as.character(),Measure = as.character())
    for(j in measurements){
      if(j == 'Length, Total (Fish)'){
        mid.result = df.limit.date[df.limit.date$CharacteristicName == 'Length, Total (Fish)',] %>%
          group_by(SubjectTaxonomicName) %>%
          summarise(
            Result = mean(as.numeric(ResultMeasureValue)),
            Text.Result = NA,
            Measurement.Unit = first(ResultMeasure.MeasureUnitCode),
            CharacteristicName = 'Mean.Fish.Length'
          )}
      else if(j == 'Count'){
        mid.result = df.limit.date[df.limit.date$CharacteristicName == 'Count',] %>%
          group_by(SubjectTaxonomicName) %>%
          summarise(
            Result = sum(as.numeric(ResultMeasureValue)),
            Text.Result = NA,
            Measurement.Unit = first(ResultMeasure.MeasureUnitCode),
            CharacteristicName = 'Insect.Count'
          )}
      else if(j == 'Dominant Habitat Type'){
        skip = 'skip'
      }
      ###########
      else if(j == 'Substrate'){
        mid.result = df.limit.date[df.limit.date$CharacteristicName.Limited == j,] 
        temp.sum = sum(as.numeric(mid.result$ResultMeasureValue))  
        mid.result = mid.result %>% group_by(CharacteristicName) %>%
          summarise(
            Result = sum(as.numeric(ResultMeasureValue))/temp.sum,
            Text.Result = NA,
            Measurement.Unit = '%',
            SubjectTaxonomicName = NA)
      }
      else if(j == 'Submerged Cover (%)'){
        mid.result = df.limit.date[df.limit.date$CharacteristicName == j,] %>%
          group_by(CharacteristicName) %>%
          summarise(
            Result = mean(as.numeric(ResultMeasureValue)),
            Text.Result = NA,
            Measurement.Unit = first(ResultMeasure.MeasureUnitCode),
            SubjectTaxonomicName = NA
          )
      }
      else if(j == 'Width.Depth'){
        mid.result = df.limit.date[df.limit.date$CharacteristicName.Limited == j,]
        mid.width = mean(as.numeric(mid.result[mid.result$CharacteristicName == 'RBP Stream width',]$ResultMeasureValue))
        mid.depth = mean(as.numeric(mid.result[mid.result$CharacteristicName == 'Depth',]$ResultMeasureValue))
        if(is.na(mid.width) || is.na(mid.depth)) {mid.result = data.frame(Result = as.numeric(),Text.Result = as.character(),Measurement.Unit = as.character(),SubjectTaxonomicName = as.character(),CharacteristicName = as.character())}
        else{
          mid.result = data.frame(
            Result = mid.depth/mid.width,
            Text.Result = NA,
            Measurement.Unit = 'Ratio',
            SubjectTaxonomicName = NA,
            CharacteristicName = j)}
      }  
      else{
        mid.limit = df.limit.date[df.limit.date$CharacteristicName == j,]
        if(is.na(as.numeric(mid.limit$ResultMeasureValue[1]))){
          mid.result = mid.limit %>%
            group_by(CharacteristicName) %>%
            summarise(
              Result = NA,
              Text.Result = ResultMeasureValue,
              Measurement.Unit = NA,
              SubjectTaxonomicName = NA
            )
        }
        else{ mid.result =mid.limit  %>%
          group_by(CharacteristicName) %>%
          summarise(
            Result = as.numeric(ResultMeasureValue),
            Text.Result = NA,
            Measurement.Unit = ResultMeasure.MeasureUnitCode,
            #Measure = j,
            SubjectTaxonomicName = NA
          )}
      }
      result = rbind(result,mid.result)
    }
    date.col = rep(k,times = nrow(result))
    result$Sample.Date = date.col
    date.intermediate = rbind(date.intermediate, result)
  }
  site.col = rep(i, times = nrow(date.intermediate))
  date.intermediate$Site = site.col
  full.burp = rbind(full.burp,date.intermediate)
}
#############################################

# now need to add the lat/lon of each site to determine the pasture that it is in
burp.site.data.limit = burp.site.data[,c('MonitoringLocationIdentifier',
                                         'MonitoringLocationName',
                                        'LatitudeMeasure',
                                        'LongitudeMeasure',
                                        'HorizontalCoordinateReferenceSystemDatumName')]
burp.site.data.limit = burp.site.data.limit %>% rename(Site = MonitoringLocationIdentifier)

new = merge(full.burp,burp.site.data.limit,by='Site')

library(sf)
library(ggplot2)
library(matrixStats)
pastures = st_read("C:/Users/samwi/OneDrive - University of Idaho/UI_ResearchTech/BLM_GISdata/Field_Office_Extents/Salmon_Pastures.shp")

new.nad83 = new[new$HorizontalCoordinateReferenceSystemDatumName == 'NAD83',]
new.wgs84 = new[new$HorizontalCoordinateReferenceSystemDatumName == 'WGS84',]

burp.nad83 = st_as_sf(new.nad83, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269)
burp.wgs84 = st_as_sf(new.wgs84, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
burp.nad83.reproject = st_transform(burp.nad83,crs=4326)

burp.allpoint = rbind(burp.nad83.reproject,burp.wgs84)

ggplot() + 
  geom_sf(data = pastures, fill = 'white') +
  geom_sf(data = burp.allpoint, color = 'red')

pastures.limit = pastures %>% select(ALLOT_NO,ALLOT_NAME,PAST_NO,PAST_NAME)

burp.allpoint.pasture = st_intersection(burp.allpoint,pastures.limit)

burp.temp = burp.allpoint.pasture %>% select(Site,
                                             geometry)
burp.unique = unique(burp.temp)
dist_matrix = st_distance(burp.unique)
diag(dist_matrix) = NA
burp.unique$distance = rowMins(dist_matrix,na.rm = T)

rowMins(dist_matrix,value= T,na.rm=T)
new.dist = matrix(dist_matrix,nrow = 78,ncol = 78)
dist.loc = which(new.dist == rowMins(dist_matrix,na.rm = T),arr.ind = T)
dist.loc[order(dist.loc[,1]),][-19,][,2]
burp.unique$closest = burp.unique$Site[dist.loc[order(dist.loc[,1]),][-19,][,2]]

#burp.unique[burp.unique$distance < 100,]
burp.unique = st_drop_geometry(burp.unique)

#st_write(burp.allpoint.pasture,'C:/Users/samwi/Documents/burp.allpoint.pasture.csv',
#         layer_options = 'GEOMETRY=AS_XY',append = F)
burp.allpoint.pasture.df = st_drop_geometry(burp.allpoint.pasture)
burp.dist.past = merge(burp.allpoint.pasture.df,burp.unique,by='Site')
write.csv(burp.dist.past,'C:/Users/samwi/Documents/burp_all_past.csv',row.names = F)

###############################################################################3
library(dplyr)
burp.data = read.csv('C:/Users/samwi/Documents/burp_all_past.csv')
burp.close = burp.data[burp.data$distance < 100,]
unique(burp.close$MonitoringLocationName)

treatments = read.csv('C:/Users/samwi/OneDrive - University of Idaho/sFO_allTreatments.csv')
treatments = treatments %>% rename(ALLOT_NAME = Allotment,PAST_NAME = Pasture)
burp.close = merge(burp.close,treatments,by=c('ALLOT_NAME','PAST_NAME'))

# at the end of all this, for sites that have repeat measurements, known treatments, and
# on BLM land, we have 1. Not going to both analyzing