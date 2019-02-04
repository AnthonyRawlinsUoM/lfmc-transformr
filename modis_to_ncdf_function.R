
modis_to_ncdf<-function(thefile, thedate)
{  
  
  require(raster)
  require(gdalUtils)
  require(ncdf4)
  
sds<-get_subdatasets(thefile)

for (i in c(1,3,4))
{  
gdal_translate(sds[i], dst_dataset = paste0("band", i, ".tif"))
}

band1<-raster("band1.tif")
band3<-raster("band3.tif")
band4<-raster("band4.tif")

step1<-((band4-band1)/(band4+band1-band3))

for (i in c(1,3,4))
{
file.remove( paste0("band", i, ".tif"))  
}

step2<-clamp(step1, lower=-1, upper=1)


###############################################################################
# Naive approach
# Get min max vari spatially from each granule. Calculate fmc based on deviation
# from granule min max.

vari_max <- max(getValues(step2), na.rm=T)
vari_min <- min(getValues(step2), na.rm=T)
vari_range = vari_max - vari_min
rvari = ((step2 - vari_min) / vari_range)

###############################################################################
# Actual Approach
# Sample temporally across 2000->2014 for each granule to derive min/max for
# each pixel. Save as a calibration set and then calculate fmc from this.
# Currently we only have NSW for 2000 - 2014

# List max and min files:
# MAX_VARI<-raster("./FuelModels/Live_FM/AVG_MAX_MIN_TO_2014_FIRE_MASKED/AVG_MAX_VARI.tif")
# MIN_VARI<-raster("./FuelModels/Live_FM/AVG_MAX_MIN_TO_2014_FIRE_MASKED/AVG_MIN_VARI.tif")
# MAX_MINUS_MIN_VARI<-MAX_VARI - MIN_VARI
# rvari = ((step2 - MIN_VARI) / MAX_MINUS_MIN_VARI)
###############################################################################



rvari<-clamp(rvari, lower=0, upper=1)

lfmc = 52.51^(1.36 * rvari)
crs(lfmc)<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
outfile<-projectRaster(from=lfmc, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )

londim <- ncdim_def("lon", "degrees_east",  seq(xmin(outfile),xmax(outfile)-res(outfile)[1], by=res(outfile)[1]))
latdim <- ncdim_def("lat", "degrees_north", rev(seq(ymin(outfile),ymax(outfile)-res(outfile)[2], by=res(outfile)[2])))
timedim<- ncdim_def("time", "days since 1900-01-01", thedate)
# define variables
varname="LFMC"
units="z-scores"
dlname <- "Nolan live fuel moisture content"
fillvalue <- 1e20
tmp.def <- ncvar_def(varname, units, list(londim, latdim, timedim), fillvalue, 
                     dlname, prec = "single")

ncout<-nc_create(gsub(".hdf", ".nc", thefile), vars=tmp.def, force_v4 = T)
ncvar_put(ncout, tmp.def, values(outfile))
nc_close(ncout)
}

