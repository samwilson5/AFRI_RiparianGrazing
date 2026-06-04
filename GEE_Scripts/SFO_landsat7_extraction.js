var SFO = ee.FeatureCollection("projects/uiriparian/assets/Salmon_FO_extent"),
    pastures = ee.FeatureCollection("projects/uiriparian/assets/SFO_allPastures"),
    riparian = ee.FeatureCollection("projects/uiriparian/assets/SFO_riparianAreas");
// function to add a column to a feature collection, this allows us to export based on pasture and allotment number
var addColumn = function(feature){
  var nombre = ee.String(feature.get('ALLOT_NO')).cat('_').cat(feature.get('PAST_NO'));
  return feature.set('uniqueID',nombre);
};
pastures = pastures.map(addColumn);

//print(pastures);

// add NDVI to Landsat 7
function addNDVIL7(image){
  var nir = image.select('SR_B4');
  var red = image.select('SR_B3');
  var one = nir.subtract(red);
  var two = nir.add(red);
  var ndvi = one.divide(two).rename('NDVI');
  return image.addBands(ndvi);
}

// clip an image collection
function ICclip(plot) {
  function wrap(img){
    return img.clip(plot)}
  return wrap
}

// create a date column for image collection that is yyyy-mm-dd
var addVariables = function(image) {
  // Compute time in fractional years since the epoch.
  var date = image.date();
  var years = date.difference(ee.Date('1970-01-01'), 'year');
  // Return the image with the added bands.
  return image
  // Add an NDVI band.
  .addBands(image.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI'))
  // Add a time band.
  .addBands(ee.Image(years).rename('t')).float()
  // Add a constant band.
  .addBands(ee.Image.constant(1));
};

// start and end dates for landsat 7 imagery of use
var startDate = ee.Date('2000-01-01');
var endDate = ee.Date('2013-12-31');

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

  var filtered = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")
    .filterDate(start, end)
    //.map(maskL457sr)
    .map(addNDVIL7)
    .filterBounds(SFO)
    .sort('CLOUD_COVER')
    .mosaic(); // You can change this to median() or mosaic() if needed

  //return ee.Image(filtered).set('system:time_start', start.millis());
  return ee.Image(filtered).set('DATE_ACQUIRED', mid);
};

// create our 16-day return interval for landsat 7
var l7_16day = ee.ImageCollection(dateList.map(composite));

// we only care about NDVI and the QA pixel values
var l7_16day = l7_16day.select(['NDVI','QA_PIXEL']);

// clip the imagery to our riparian areas
l7_16day = l7_16day.map(ICclip(riparian));


// function to exprt a CSV file with NDVI values for each timestep with a seperate CSV for each polygon
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
    folder: 'SFO_LANDSAT_NDVI', 
    fileFormat: 'CSV', 
    selectors: ['DATE_ACQUIRED', 'NDVI']
  });
}


pastures = ee.FeatureCollection(pastures);


pastures.evaluate(function(evaluatedFeatures) { // get to the client-side
   print(evaluatedFeatures);
   evaluatedFeatures.features.map(function(feat){
     print(ee.Feature(feat).geometry())
     exportCSV(l7_16day, ee.Feature(feat).geometry(), feat.properties.uniqueID); //create new column in pastures then  just use that 
         // change 'id' to the property name of the property you want to plot
 });
});

////////////////////////////////////////////////////////////////////////
// function to exprt a CSV file with QA values for each timestep with a seperate CSV for each polygon
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
    folder: 'SFO_LANDSAT_QA', 
    fileFormat: 'CSV', 
    selectors: ['DATE_ACQUIRED', 'QA_PIXEL']
  });
}

pastures = ee.FeatureCollection(pastures);

pastures.evaluate(function(evaluatedFeatures) { // get to the client-side
  print(evaluatedFeatures);
   evaluatedFeatures.features.map(function(feat){
     print(ee.Feature(feat).geometry())
     exportCSV_QA(l7_16day, ee.Feature(feat).geometry(), feat.properties.uniqueID); 
     });
});
