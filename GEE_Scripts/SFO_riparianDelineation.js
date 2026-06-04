var table2 = ee.FeatureCollection("projects/uiriparian/assets/OO_delineation_SalmonTest"),
    geometry = ee.FeatureCollection("projects/uiriparian/assets/Salmon_FO_extent"),
    table = ee.FeatureCollection("projects/uiriparian/assets/NHD_streams");
// function selects NIR and red bands, calculates NDVI, and adds it as a band to each image
function addNDVI(image){
  var nir = image.select('N');
  var red = image.select('R');
  var one = nir.subtract(red);
  var two = nir.add(red);
  var ndvi = one.divide(two).rename('NDVI');
  return image.addBands(ndvi);
}
// function selects NIR, red, and blue bands, calculates EVI, and adds it as a band to each image
function addEVI(image){
  var nir = image.select('N');
  var red = image.select('R');
  var blue = image.select('B');
  var oneone = nir.subtract(red);
  var one = oneone.multiply(2.5);
  var twoone = nir.add(6);
  var twotwo = red.subtract(7.5);
  var twothree = blue.add(1);
  var two = twoone.multiply(twotwo).multiply(twothree);
  var evi = one.divide(two).rename('EVI');
  return image.addBands(evi);
}

// takes imagery from 2019 and 2021, adds EVI and NDVI, then takes the median to create a single composite image
var img = ee.ImageCollection('USDA/NAIP/DOQQ')
                  .filterDate('2019-01-01', '2021-12-31')
                  .filterBounds(geometry)
                  .map(addNDVI)
                  .map(addEVI)
                  .median();

var bands = ['R','G', 'N','NDVI','EVI'];

// bands need to be rescaled
img = ee.Image(img)
  .clip(geometry)
  .divide(255)
  .select(bands);

//Map.addLayer(img.select(['N','R','G']), {gamma: 0.8}, 'RGBN');

var seeds = ee.Algorithms.Image.Segmentation.seedGrid(16);

// Run SNIC on the regular square grid.
var snic = ee.Algorithms.Image.Segmentation.SNIC({
  image: img,
  size: 16,
  compactness: 0.3,
  connectivity: 8,
  neighborhoodSize: 32,
  seeds: seeds
}).select(['R_mean', 'G_mean','N_mean','NDVI_mean','EVI_mean','clusters'], ['R', 'G', 'N','NDVI','EVI','clusters']);
//snic= snic.addBands(seeds)

var clusters = snic.select('clusters');

// Compute per-cluster stdDev.
var stdDev = img.addBands(clusters).reduceConnectedComponents(ee.Reducer.stdDev(), 'clusters', 256);

// Area, Perimeter, Width and Height
var area = ee.Image.pixelArea().addBands(clusters).reduceConnectedComponents(ee.Reducer.sum(), 'clusters', 256);

var minMax = clusters.reduceNeighborhood(ee.Reducer.minMax(), ee.Kernel.square(1));
var perimeterPixels = minMax.select(0).neq(minMax.select(1)).rename('perimeter');

var perimeter = perimeterPixels.addBands(clusters)
    .reduceConnectedComponents(ee.Reducer.sum(), 'clusters', 256);

var sizes = ee.Image.pixelLonLat().addBands(clusters).reduceConnectedComponents(ee.Reducer.minMax(), 'clusters', 256);
var width = sizes.select('longitude_max').subtract(sizes.select('longitude_min')).rename('width');
var height = sizes.select('latitude_max').subtract(sizes.select('latitude_min')).rename('height');

var objectPropertiesImage = ee.Image.cat([
  snic.select(bands),
  stdDev,
  area,
  perimeter,
  width,
  height
]).float();

var afn_Kmeans=function(input, numberOfUnsupervisedClusters,
   defaultStudyArea, nativeScaleOfImage){

   // Make a new sample set on the input. Here the sample set is
   // randomly selected spatially.
   var training=input.sample({
       region: defaultStudyArea,
       scale: nativeScaleOfImage,
       numPixels: 1000
   });
   var cluster=ee.Clusterer.wekaKMeans(
           numberOfUnsupervisedClusters)
       .train(training);

   // Now apply that clusterer to the raw image that was also passed in.
   var toexport=input.cluster(cluster);

   // The first item is the unsupervised classification. Name the band.
   var clusterUnsup=toexport.select(0).rename(
       'unsupervisedClass');
   return (clusterUnsup);
};

// 6.2 SNIC Unsupervised Classification for Comparison
var bandsToUse=['N','R','G','NDVI','EVI'];
var afn_addMeanToBandName=(function(i){
   return i + '_mean';
});
var bandMeansNames=bandsToUse.map(afn_addMeanToBandName);

var meanSegments = snic.select(bandsToUse);

var SegmentUnsupervised=afn_Kmeans(meanSegments,
   6,//6,
   geometry,
   0.6);

Map.addLayer(SegmentUnsupervised.randomVisualizer(),{},
   '6.3 SNIC Clusters Unsupervised', true);
   

var sieved = SegmentUnsupervised.focalMode({radius:1,iterations:2});
Map.addLayer(sieved.randomVisualizer(),{},
   'Sieved', true);

// select the cluster that represents riparian areas
var groups = sieved.select(['unsupervisedClass']);
var groupMask = groups.eq(4);
groups = groups.updateMask(groupMask);

// filter out intermittentant and seasonal streams from NHD layer
table = table.filter(ee.Filter.neq('fcode',46007));
table = table.filter(ee.Filter.neq('fcode',46003));
table = table.filterBounds(geometry)

// buffer by 30 meters
var nhd = table.map(function(feat){return feat.buffer(30)})
groups = groups.clipToCollection(nhd)

var classes = groups.reduceToVectors({
  reducer: ee.Reducer.countEvery(), 
  geometry: geometry, 
  scale: 1.0,
  maxPixels: 1e12,
  eightConnected: false,
  bestEffort: false
});

Map.addLayer(classes)
Export.table.toDrive({
  collection:classes,
  // export polygon will be used as riparian extent in all other GEE scripts
  description: 'SFO_riparianAreas',
  maxVertices: 1000,
  fileFormat: 'SHP'
});
