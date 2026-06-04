var SFO = ee.FeatureCollection("projects/uiriparian/assets/Salmon_FO_extent"),
    pastures = ee.FeatureCollection("projects/uiriparian/assets/SFO_allPastures"),
    riparian = ee.FeatureCollection("projects/uiriparian/assets/SFO_riparianAreas");


// create a function to add a column to a feature collection
var addColumn = function(feature){
  var nombre = ee.String(feature.get('ALLOT_NO')).cat('_').cat(feature.get('PAST_NO'));
  return feature.set('uniqueID',nombre);
};
pastures = pastures.map(addColumn);

//print(pastures);

// create NDVI band for landsat 8
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

// creating a date column that is in yyyy-mm-dd
var addVariables = function(image) {
  // Compute time in fractional years since the epoch.
  var date = image.date();
  var years = date.difference(ee.Date('1970-01-01'), 'year');
  // Return the image with the added bands.
  return image
  // Add an NDVI band.
  .addBands(image.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI'))
  // Add a time band.
  .addBands(ee.Image(years).rename('t')).float()
  // Add a constant band.
  .addBands(ee.Image.constant(1));
};

// start and end dates for landsat 8
var startDate = ee.Date('2013-01-01');
var endDate = ee.Date('2021-01-01');

// Function to create a list of dates at 16-day intervals
var dateList = ee.List.sequence(0, endDate.difference(startDate, 'day').subtract(1), 16)
  .map(function(dayOffset) {
    return startDate.advance(ee.Number(dayOffset), 'day');
  });
  
// Function to get the best image (e.g., least cloudy) in a 16-day window
var composite = function(date) {
  var start = ee.Date(date);
  var end = start.advance(16, 'day')
  var mid = start.advance(8, 'day');

  var filtered = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
    .filterDate(start, end)
    .map(addNDVIL8)
    .filterBounds(SFO)
    .sort('CLOUD_COVER')
    .mosaic(); // You can change this to median() or mosaic() if needed

  //return ee.Image(filtered).set('system:time_start', start.millis());
  return ee.Image(filtered).set('DATE_ACQUIRED', mid);
};

// create our 16-day return interval image collection for landsat 8
var l8_16day = ee.ImageCollection(dateList.map(composite));

// we only care about NDVI and the QA pixel value
var l8_16day = l8_16day.select(['NDVI','QA_PIXEL']);
// clip to our riparian areas
l8_16day = l8_16day.map(ICclip(riparian));

//Map.addLayer(l8_16day)
//Map.addLayer(riparian)

// function to export a CSV file
function exportCSV(collection, point, name){
  
  // filter the images
  //collection = collection.filterBounds(point);
  collection = ee.ImageCollection(collection.map(ICclip(point)));
  collection = collection.filterBounds(point)
  // get the values of band 4
  var feats = collection.map(function(image){
    return ee.Feature(null, image.select('NDVI').reduceRegion(ee.Reducer.median(), point, 30))
                .set('DATE_ACQUIRED', image.get('DATE_ACQUIRED'));
  });
  
  print('test export', feats);
  
  // if a name is defined
  var discription = 'YOURDISCRIPTION'; 
    if (name) { discription = name; } 
  
  // export a CSV file
  Export.table.toDrive({
    collection: feats, 
    description: discription, 
    folder: 'SFO_LANDSAT8_NDVI', 
    fileFormat: 'CSV', 
    selectors: ['DATE_ACQUIRED', 'NDVI']
  });
}


pastures = ee.FeatureCollection(pastures);

pastures.evaluate(function(evaluatedFeatures) { // get to the client-side
   print(evaluatedFeatures);
   evaluatedFeatures.features.map(function(feat){
     print(ee.Feature(feat).geometry())
     exportCSV(l8_16day, ee.Feature(feat).geometry(), feat.properties.uniqueID); //create new column in pastures then  just use that 
     // change 'id' to the property name of the property you want to plot
 });
});

////////////////////////////////////////////////////////////////////////

function exportCSV_QA(collection, point, name){
  
  // filter the images
  //collection = collection.filterBounds(point);
  collection = ee.ImageCollection(collection.map(ICclip(point)));
  collection = collection.filterBounds(point)
  // get the values of band 4
  var feats = collection.map(function(image){
    return ee.Feature(null, image.select('QA_PIXEL').reduceRegion(ee.Reducer.mode({maxRaw:9999}), point, 30))
                .set('DATE_ACQUIRED', image.get('DATE_ACQUIRED'));
  });
  
  print('test export', feats);
  
  // if a name is defined
  var discription = 'YOURDISCRIPTION'; 
    if (name) { discription = name; } 
  
  // export a CSV file
  Export.table.toDrive({
    collection: feats, 
    description: discription, 
    folder: 'SFO_LANDSAT8_QA', 
    fileFormat: 'CSV', 
    selectors: ['DATE_ACQUIRED', 'QA_PIXEL']
  });
}

pastures = ee.FeatureCollection(pastures);

pastures.evaluate(function(evaluatedFeatures) { // get to the client-side
   print(evaluatedFeatures);
   evaluatedFeatures.features.map(function(feat){
     print(ee.Feature(feat).geometry())
     exportCSV_QA(l8_16day, ee.Feature(feat).geometry(), feat.properties.uniqueID); 
     });
});
