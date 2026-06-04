var SFO = ee.FeatureCollection("projects/uiriparian/assets/Salmon_FO_extent"),
    pastures = ee.FeatureCollection("projects/uiriparian/assets/SFO_allPastures"),
    riparian = ee.FeatureCollection("projects/uiriparian/assets/SFO_riparianAreas");

// buffering riparian areas by 100 meters to make sure that mesic areas are still riparian related
riparian = riparian.map(function(feat){
  return feat.buffer(100);
});

// create a column that is allotment_pasture numbers
var addColumn = function(feature){
  var nombre = ee.String(feature.get('ALLOT_NO')).cat('_').cat(feature.get('PAST_NO'));
  return feature.set('uniqueID',nombre);
};
pastures = pastures.map(addColumn);

// add NDVI band to Landsat 7 
function addNDVIL7(image){
  var nir = image.select('SR_B4');
  var red = image.select('SR_B3');
  var one = nir.subtract(red);
  var two = nir.add(red);
  var ndvi = one.divide(two).rename('NDVI');
  return image.addBands(ndvi);
}

// add NDVI band to Landsat 8
function addNDVIL8(image){
  var nir = image.select('SR_B5');
  var red = image.select('SR_B4');
  var one = nir.subtract(red);
  var two = nir.add(red);
  var ndvi = one.divide(two).rename('NDVI');
  return image.addBands(ndvi);
}

// clip an image collection
function ICclip(plot) {
  function wrap(img){
    return img.clip(plot)}
  return wrap;
}

// mask clouds for Landsat 7
function maskL457sr(image) {
  // Bit 0 - Fill
  // Bit 1 - Dilated Cloud
  // Bit 2 - Unused
  // Bit 3 - Cloud
  // Bit 4 - Cloud Shadow
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
      .addBands(thermalBand, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}

// mask clouds for Landsat 8
function maskL8sr(image) {
  // Bit 0 - Fill
  // Bit 1 - Dilated Cloud
  // Bit 2 - Cirrus
  // Bit 3 - Cloud
  // Bit 4 - Cloud Shadow
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
      .addBands(thermalBands, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}

var l7_16day = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")
  .filterDate('2000-01-01','2012-12-31')
  .map(maskL457sr)
  .map(addNDVIL7)
  .filterBounds(SFO);

var l8_16day = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
  .filterDate('2013-01-01','2020-12-31')
  .map(maskL8sr)
  .map(addNDVIL8)
  .filterBounds(SFO);

var all_16day = l7_16day.merge(l8_16day);

var startingDates = ['2000-07-01','2001-07-01','2002-07-01','2003-07-01','2004-07-01','2005-07-01','2006-07-01','2007-07-01','2008-07-01','2009-07-01','2010-07-01','2011-07-01','2012-07-01','2013-07-01','2014-07-01','2015-07-01','2016-07-01','2017-07-01','2018-07-01','2019-07-01','2020-07-01'];

// getting the mean of each image collection that runs July 1 - August 31 of each year
var season_mesic_l7 = function(date) {
  var start = ee.Date(date);
  var end = start.advance(62, 'day');
  var year = ee.String(start.get('year'));
  var label = year.cat('_seasonMax');

  var filtered = all_16day
  .filterDate(start,end)
  .mean()
  .clipToCollection(riparian);
  return ee.Image(filtered).set('Year_period', label);
};

var filtered_mesic_l7 = ee.ImageCollection(startingDates.map(season_mesic_l7));

// get rid of images that had been entirely masked by clouds
filtered_mesic_l7 = filtered_mesic_l7.filter(ee.Filter.listContains("system:band_names", "NDVI"));

// select only the NDVI band
filtered_mesic_l7 = filtered_mesic_l7.select('NDVI');

// we only care about NDVI >= 0.3
var mesic_masked = filtered_mesic_l7.map(function(image){
  return image.mask(image.select("NDVI").gte(0.3));
});

Map.addLayer(mesic_masked)
////// Find max value for all mesic areas across all years
// Reduce the collection to a single image of maximum pixel values
var temporalMax = mesic_masked.max();

// Find the global max value within your ROI
var stats = temporalMax.reduceRegion({
  reducer: ee.Reducer.max(),
  geometry: SFO,
  scale: 30,       
  maxPixels: 1e13,
  bestEffort: true,
  tileScale: 2
});

// Print the result
print('Maximum pixel value:', stats);
//////

/////////// now lets export the count of NDVI over each pasture
function exportCSV_mesicCount(collection, point, name){
  
  // filter the images
  collection = ee.ImageCollection(collection.map(ICclip(point)));
  var feats = collection.map(function(image){
    return ee.Feature(null, image.select('NDVI').reduceRegion(ee.Reducer.count(), point, 30))
                .set('Year_period', image.get('Year_period'));
  });
  
  print('test export', feats);
  
  var discription = 'YOURDISCRIPTION'; 
    if (name) { discription = name; } 
  
  // export a CSV file
  Export.table.toDrive({
    collection: feats, 
    description: discription, 
    folder: 'SFO_mesicExtent', 
    fileFormat: 'CSV', 
    selectors: ['Year_period', 'NDVI']
  });
}

pastures = ee.FeatureCollection(pastures);

pastures.evaluate(function(evaluatedFeatures) { // get to the client-side
   print(evaluatedFeatures);
   evaluatedFeatures.features.map(function(feat){
     print(ee.Feature(feat).geometry());
     exportCSV_mesicCount(mesic_masked, ee.Feature(feat).geometry(), feat.properties.uniqueID); 
     });
});
