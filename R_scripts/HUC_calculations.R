## Automate the process of calculating area of HUC that is riparian
library(sf)
library(raster)
library(dplyr)


riparian = st_read("C:/Users/Sam/Downloads/drive-download-20251014T223700Z-1-001/oo_TEST_unsup_1921_nhdPerr.shp")
huc = st_read("C:/Users/Sam/OneDrive - University of Idaho/UI_ResearchTech/Other_GISdata/Salmon_HUC12/SalmonHUC12.shp")

# in reality they should all be seperated by pasture so first need to just merge them all into one
dissolved = st_union(riparian)
# if crs are not the same for dissolved and huc
#trans.dissolve = dissolved %>% st_transform(st_crs(huc))

# now we need to run through each huc within SFO clip dissolved then calculate area?
areas = data.frame(huc12 = huc$huc12,
                   area = as.numeric(huc$areaacres),
                   riparian.area = 0,
                   percent.riparian = 0)
for(i in 1:83){
  loc = areas[i,1]
  shp = huc[huc$huc12 == loc,]
  int = st_intersects(shp, dissolved, sparse = T)
  if(length(int[[1]]) == 0){
    rip.area = 0
    ratio = 0
  }
  else{
    clip.dissolve = st_crop(dissolved,shp)
    rip.area = units::set_units(st_area(clip.dissolve), "acre")
    ratio = as.numeric(rip.area)/areas[i,2]}
  areas[i,3] = as.numeric(rip.area)
  areas[i,4] = ratio
}

# loop works, probably need to check if there is a reason there are so many
# spots with no intersection