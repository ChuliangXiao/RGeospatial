xlib <- c("ncdf4", "ncdf4.helpers", "PCICt", "raster", "ggmap", 
          "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
lapply(xlib, library, character.only = TRUE) # load the required packages


# read in gridded NetCDF file
fname   <- paste0("Precip/", "201401010000.PRECIP_FORCING.nc")
geoFile <- "geo_em.d02.nc"
fnc     <- nc_open(fname)

# get Coordinate 
lon <- ncvar_get(fnc, varid = "lon")
lat <- ncvar_get(fnc, varid = "lat")
# get varaible precipitation rate
PR  <- ncvar_get(fnc, varid = "precip_rate")
# get Projection
library(rwrfhydro)
proj4 <- GetProj(geoFile) 

# 
spPR  <- data.frame(lon=as.vector(lon), lat=as.vector(lat), conc=as.vector(PR))
coordinates(spPR) <- ~ lon+lat
proj4string(spPR) = CRS(proj4)

#raster
r             <- raster(ncols=ncol(lat),nrows=nrow(lat))
extent(r)     <- extent(spPR)
rPR           <- rasterize(coordinates(spPR), r, spPR$conc)

r <- projectRaster(rPR, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

rtp <- rasterToPolygons(r)

bm <- ggmap(get_map(location = bbox(rtp)))
bm +  geom_polygon(data = rtp, 
                   aes(x = long, y = lat, group = group, 
                       fill = rep(rtp$layer*1e4, each = 5)), 
                   size = 0, 
                   alpha = 0.5)  + 
  scale_fill_gradientn(expression("PR\n"~10^{-4}), 
                       colors = topo.colors(255)) 
