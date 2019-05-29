#!/usr/bin/env r
#### HTTP RESOURCES
test <- function(n, wait = 0.5, ...) {
    Sys.sleep(wait)
    list(value = n)
}

#### FUNCTIONS FOR API if(!exists('modis_to_ncdf', mode='function'))
#### source('./modis_to_ncdf_function.R')




modis_to_ncdf <- function(thefile) {
    
    require(rgeos)
    require(raster)
    require(rgdal)
    require(snowfall)
    require(jsonlite)
    require(httr)
    require(geojsonio)
    require(xml2)
    require(maptools)
    require(stringr)
    require(ncdf4)
    
    classify <- c(16001, Inf, NA, -Inf, 0, NA)
    rcl <-matrix(classify, ncol=3, byrow=TRUE)
    
    in_path <- "/home/docker/rserve/data/in/"
    shp_path <- "/home/docker/rserve/data/static"
    out_path <- "/home/docker/rserve/data/out/"
    
    ramdisk <- "/media/ramdisk/"
    
    thefilepath <- paste0(in_path, thefile)
    
    out_file_name <- gsub(".hdf", ".nc", thefile)
    out_file_path <- paste0(out_path, out_file_name)
    
    result <- list(name = out_file_name, status_code = 500)
    
    if (!file.exists(out_file_path)) {
        get_sds <- function(thefilepath) {
            sds <- tryCatch({
                gdalUtils::get_subdatasets(thefilepath)
            }, warning = function(w) {
                result <- list(name = out_file_name, status_code = 503)
                return(result)
            }, error = function(e) {
                file.remove(thefilepath)
                result <- list(name = out_file_name, status_code = 503, c("[Error] :", 
                  thefilepath, " (problem file was deleted.)"))
                return(result)
            }, finally = {
                
            })
            return(sds)
        }
        
        sds <- get_sds(thefilepath)
        
        if (!is.null(sds)) {
            
            for (i in c(1, 3, 4)) {
                gdalUtils::gdal_translate(sds[i], dst_dataset = paste0(ramdisk, out_file_name, 
                  "-band", i, ".tif"), r = "cubic")
            }
            
            band1 <- reclassify(raster(paste0(ramdisk, out_file_name, "-band1.tif")), 
                rcl)
            band3 <- reclassify(raster(paste0(ramdisk, out_file_name, "-band3.tif")), 
                rcl)
            band4 <- reclassify(raster(paste0(ramdisk, out_file_name, "-band4.tif")), 
                rcl)
            
            # Match thefile's name to the modis granule forest mask
            #  eg., thefile<-"/nfs/pyromancer/Project/Landscape_Fuel_Moisture_Project/data/geoserver/data/FuelModels/Live_FM/MODIS/MOD09A1.A2017297.h30v12.006.2017310191152.hdf"
            # 
            mask <- paste0(shp_path, "/", strsplit(thefile, "[.]")[[1]][3], ".shp")
            
            mask_shp <- shapefile(mask)
            mask_shp <- spTransform(mask, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
            
            vari <- band1  # Placeholder to keep object properties intact
            
            vari@data@values <- ((band4@data@values - band1@data@values)/(band4@data@values + 
                band1@data@values - band3@data@values))
            
            # FROM MODIS
            crs(vari) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
            
            # Anything outside the modis granule forest mask is NODATA / NULL (masked)
            
            # crs(mask_shp) # ??
            vari_masked <- crop(vari, mask_shp)
            
            
            # TO MERCATOR
            outfile <- projectRaster(from = vari_masked, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
            
            londim <- ncdim_def("lon", "degrees_east", seq(xmin(outfile), xmax(outfile) - 
                res(outfile)[1], by = res(outfile)[1]))
            latdim <- ncdim_def("lat", "degrees_north", rev(seq(ymin(outfile), ymax(outfile) - 
                res(outfile)[2], by = res(outfile)[2])))
            
            gdalinfo_raw <- gdalUtils::gdalinfo(thefilepath)
            dates <- gdalinfo_raw[grep(glob2rx("*GRANULEBEGINNINGDATETIME*"), gdalinfo_raw)]
            date_parts <- strsplit(dates, "=")
            first_date <- strsplit(as.character(date_parts[[1]][2]), ",")[[1]][1]
            
            
            days <- as.numeric(as.Date(first_date) - as.Date("1970-01-01"))
            timedim <- ncdim_def("time", "days since 1970-01-01", days)
            
            # define variables
            varname = "vari"
            units = "z-scores"
            dlname <- "Nolan live fuel moisture variation"
            fillvalue <- 1e+20
            tmp.def <- ncdf4::ncvar_def(varname, units, list(londim, latdim, timedim), 
                fillvalue, dlname, prec = "double")
            
            ncout <- ncdf4::nc_create(out_file_path, vars = tmp.def, force_v4 = T)
            ncdf4::ncvar_put(ncout, tmp.def, values(outfile))
            ncdf4::nc_close(ncout)
            
            file.remove(paste0(ramdisk, out_file_name, "-band1.tif"))
            file.remove(paste0(ramdisk, out_file_name, "-band3.tif"))
            file.remove(paste0(ramdisk, out_file_name, "-band4.tif"))
            
            result <- list(name = out_file_name, status_code = 200)
            
            # msg <- paste('[Wrote file] :', out_file_name)
        }
    }
    
    return(result)
}








#### PROCESS REQUEST
process_request <- function(url, query, body, headers) {
    #### building request object not strictly necessary as in FastRWeb, just to make
    #### clear of request related variables
    request <- list(uri = url, method = "POST", query = query, body = body)
    
    ## parse headers
    request$headers <- parse_headers(headers)
    if ("request-method" %in% names(request$headers)) 
        request$method <- c(request$headers["request-method"])
    
    ## parse parameters (function arguments) POST accept only 2 content types -
    ## application/x-www-form-urlencoded by built-in server - application/json used
    ## below as do.call(function_name, request$pars)
    request$pars <- list()
    if (request$method == "POST") {
        if (!is.null(body)) {
            if (is.raw(body)) 
                body <- rawToChar(body)
            if (any(grepl("application/json", request$headers))) 
                body <- jsonlite::fromJSON(body)
            request$pars <- as.list(body)
        }
    } else {
        if (!is.null(query)) {
            request$pars <- as.list(query)
        }
    }
    
    #### building output object list(payload, content-type, headers, status_code)
    #### https://github.com/s-u/Rserve/blob/master/src/http.c#L358
    payload <- NULL
    content_type <- "application/json; charset=utf-8"
    headers <- character(0)
    status_code <- 200
    
    ## generate payload (function output) function name must match to resource path
    ## for now
    matched_fun <- gsub("^/", "", request$uri)
    
    ## no resource path means no matching function
    if (matched_fun == "") {
        payload <- list(api_version = "1.0")
        if (grepl("application/json", content_type)) 
            payload <- jsonlite::toJSON(payload, auto_unbox = TRUE)
        return(list(payload, content_type, headers))  # default status 200
    }
    
    ## check if all defined arguments are supplied
    defined_args <- formalArgs(matched_fun)[formalArgs(matched_fun) != "..."]
    args_exist <- defined_args %in% names(request$pars)
    if (!all(args_exist)) {
        missing_args <- defined_args[!args_exist]
        payload <- list(message = paste("Missing parameter -", paste(missing_args, 
            collapse = ", ")))
        status_code <- 400
    }
    
    if (is.null(payload)) {
        payload <- tryCatch({
            do.call(matched_fun, request$pars)
        }, error = function(err) {
            list(message = paste0("Internal Server Error: ", err))
        })
        
        if ("message" %in% names(payload)) 
            status_code <- 500
    }
    
    if (grepl("application/json", content_type)) 
        payload <- jsonlite::toJSON(payload, auto_unbox = TRUE)
    
    return(list(payload, content_type, headers, status_code))
}

# parse headers in process_request()
# https://github.com/s-u/FastRWeb/blob/master/R/run.R#L65
parse_headers <- function(headers) {
    ## process headers to pull out request method (if supplied) and cookies
    if (is.raw(headers)) 
        headers <- rawToChar(headers)
    if (is.character(headers)) {
        ## parse the headers into key/value pairs, collapsing multi-line values
        h.lines <- unlist(strsplit(gsub("[\r\n]+[ \t]+", " ", headers), "[\r\n]+"))
        h.keys <- tolower(gsub(":.*", "", h.lines))
        h.vals <- gsub("^[^:]*:[[:space:]]*", "", h.lines)
        names(h.vals) <- h.keys
        h.vals <- h.vals[grep("^[^:]+:", h.lines)]
        return(h.vals)
    } else {
        return(NULL)
    }
}

## Rserve requires .http.request function for handling HTTP request
.http.request <- process_request
