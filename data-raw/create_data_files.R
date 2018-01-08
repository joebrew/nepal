library(devtools)
library(raster)
library(rgdal)
library(sp)
library(tidyverse)
library(RColorBrewer)
library(raster)

# Initiate the raw-data into package session
devtools::use_data_raw()

# Get Nepalese shapefiles
nep0 <- raster::getData(name = 'GADM', download = TRUE, country = 'NPL', level = 0)
nep1 <- raster::getData(name = 'GADM', download = TRUE, country = 'NPL', level = 1)
nep2 <- raster::getData(name = 'GADM', download = TRUE, country = 'NPL', level = 2)

# Save for use in package
devtools::use_data(nep0,
                   overwrite = TRUE)
devtools::use_data(nep1,
                   overwrite = TRUE)
devtools::use_data(nep2,
                   overwrite = TRUE)

# Get world cities
cities <- readr::read_csv('simplemaps-worldcities-basic.csv')
cities_sp <- cities
coordinates(cities_sp) <- ~lng+lat
proj4string(cities_sp) <- proj4string(nep0)
keep <- over(cities_sp, polygons(nep0))
keep <- !is.na(keep)
cities <- cities[keep,]
cities_sp <- cities_sp[keep,]
devtools::use_data(cities,
                   overwrite = TRUE)
devtools::use_data(cities_sp,
                   overwrite = TRUE)



# GRD
dpath<- 'SRTM v4.1 - Resampled/srtmv4_30s/'
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
r <- raster(xx)

# Use raster to get elevation
bb <- bbox(nep0)
# nepal_centroid <- coordinates(nepal0)
# Combine different tiles: http://www.gis-blog.com/r-raster-data-acquisition/

# Modified from here
get_srtm <- function(lon, lat, download, path) {
  .download <- function(aurl, filename) {
    fn <- paste(tempfile(), '.download', sep='')
    res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
    if (res == 0) {
      w <- getOption('warn')
      on.exit(options('warn' = w))
      options('warn'=-1) 
      if (! file.rename(fn, filename) ) { 
        # rename failed, perhaps because fn and filename refer to different devices
        file.copy(fn, filename)
        file.remove(fn)
      }
    } else {
      stop('could not download the file' )
    }
  }
  
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  
  rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
  rowTile <- rowFromY(rs, lat)
  colTile <- colFromX(rs, lon)
  if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
  if (colTile < 10) { colTile <- paste('0', colTile, sep='') }
  
  f <- paste('srtm_', colTile, '_', rowTile, sep="")
  zipfilename <- paste(path, "/", f, ".ZIP", sep="")
  tiffilename <- paste(path, "/", f, ".tif", sep="")
  
  if (!file.exists(tiffilename)) {
    if (!file.exists(zipfilename)) {
      if (download) { 
        theurl <- paste("ftp://xftp.jrc.it/pub/srtmV4/tiff/", f, ".zip", sep="")
        test <- try (.download(theurl, zipfilename) , silent=TRUE)
        if (class(test) == 'try-error') {
          theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
          test <- try (.download(theurl, zipfilename) , silent=TRUE)
          if (class(test) == 'try-error') {
            theurl <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", f, ".ZIP", sep="")
            .download(theurl, zipfilename)
          }
        }
      } else {message('file not available locally, use download=TRUE') }	
    }
    if (file.exists(zipfilename)) { 
      utils::unzip(zipfilename, exdir=dirname(zipfilename))
      file.remove(zipfilename)
    }	
  }
  if (file.exists(tiffilename)) { 
    rs <- raster(tiffilename)
    projection(rs) <- "+proj=longlat +datum=WGS84"
    return(rs)
  } else {
    stop('file not found')
  }
}
srtm1 <- get_srtm(lon = bb[1,1],
                  lat = bb[2,1],
                  download = TRUE,
                  path = 'srtm')
srtm2 <- get_srtm(lon = bb[1,2],
                  lat = bb[2,1],
                  download = TRUE,
                  path = 'srtm')
srtm3 <- get_srtm(lon = bb[1,1],
                  lat = bb[2,2],
                  download = TRUE,
                  path = 'srtm')

# Merge the srtm files
srtm <- mosaic(srtm1, srtm2, srtm3, fun=mean)
plot(srtm)
plot(nep0, add = TRUE)
nepal0 <- spTransform(nep0, proj4string(srtm))
cr <- crop(srtm, 
           polygons(nepal0),
           # extent(nepal0), 
           snap="out")    
er <- mask(cr, nepal0)
plot(er)
# Make a smaller version of er
era <- raster::aggregate(er, fact = 50, fun = mean)

elevation_raster <- er
elevation_raster_small <- era
# Save both
devtools::use_data(elevation_raster,
                   overwrite = TRUE)
devtools::use_data(elevation_raster_small,
                   overwrite = TRUE)
# fr <- rasterize(nepal0, cr)   
# lr <- mask(x=cr, mask=fr)
# lr <- mask(x=cr, mask=polygons(nepal0))


# More elevation stuff
elevation <- getData("alt", country = "NPL")
elevation_terrain <- terrain(elevation, opt = c("slope", "aspect"), unit = "degrees")
elevation_slope <- terrain(elevation, opt = "slope")
elevation_aspect <- terrain(elevation, opt = "aspect")
elevation_hill <-  hillShade(elevation_slope, elevation_aspect, 40, 270)

devtools::use_data(elevation,
                   overwrite = TRUE)
devtools::use_data(elevation_terrain,
                   overwrite = TRUE)
devtools::use_data(elevation_slope,
                   overwrite = TRUE)
devtools::use_data(elevation_aspect,
                   overwrite = TRUE)
devtools::use_data(elevation_hill,
                   overwrite = TRUE)

# x <- terrain(elevation, opt = c("slope", "aspect"), unit = "degrees")
# plot(x)
# slope <- terrain(elevation, opt = "slope")
# aspect <- terrain(elevation, opt = "aspect")
# hill <- hillShade(slope, aspect, 40, 270)
# plot(hill, col = grey(0:100/100), legend = FALSE, main = "Nepal")
# plot(elevation, col = rainbow(25, alpha = 0.35), add = TRUE)
# library(rasterVis)
# persp(elevation, exp=0.1,phi=25, xlab="X", ylab="Y", zlab="Z", shade=0.45, col="green4", border = NA)
# plot3D(elevation)
# TIFF
# tf <- raster('data/SRTM v4.1 - Resampled large/SRTM v4.1 - Resampled/SRTM_W_250m_TIF/SRTM_W_250m.tif')



# LEAFLET
# library(leaflet)
# pal <- colorNumeric(c('white', 'red'), values(era),
#                     na.color = "transparent")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(era, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(era),
#             title = "Elevation")
