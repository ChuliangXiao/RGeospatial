xlib <- c("ncdf4", "ncdf4.helpers", "PCICt", "raster", "ggmap", 
          "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
lapply(xlib, library, character.only = TRUE) # load the required packages


# read in gridded NetCDF file
fname   <- paste0("Precip/", "PRECIP_FORCING.2014010100-2014120100.nc")
geoFile <- "geo_em.d02.nc"
fnc     <- nc_open(fname)

# get Coordinate 
lon <- ncvar_get(fnc, varid = "lon")
lat <- ncvar_get(fnc, varid = "lat")
# get varaible precipitation rate
PR1  <- ncvar_get(fnc, varid = "precip_rate")
PR   <- rowMeans(PR1, dims = 2)


# get Projection
library(rwrfhydro)
proj4 <- GetProj(geoFile) 

# 
spPR  <- data.frame(lon=as.vector(lon), lat=as.vector(lat), conc=as.vector(PR))
coordinates(spPR) <- ~ lon+lat
proj4string(spPR) = CRS(proj4)

# read Maumee River Shapfile
rMaumee       <- readOGR(dsn = "MaumeeRiverShapefile", layer = "Maumee_Output")
#rMaumee.proj <- spTransform(rMaumee, CRSobj=CRS(proj4))
#spPR.proj    <- spTransform(spPR, CRSobj=CRS(proj4string(rMaumee)))


r             <- raster(ncols=ncol(lat),nrows=nrow(lat))
extent(r)     <- extent(spPR)
rPR           <- rasterize(coordinates(spPR), r, spPR$conc)
rMaumee.proj  <- spTransform(rMaumee, CRSobj=CRS(proj4string(rPR)))


plot(rPR*1e5)
plot(rMaumee.proj, add = T)

# ggmap
spPR  <- data.frame(lon=as.vector(lon), lat=as.vector(lat), conc=as.vector(PR))
coordinates(spPR) <- ~ lon+lat
proj4string(spPR) = CRS(proj4)

r <- projectRaster(rPR, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
rtp <- rasterToPolygons(r)

bm <- ggmap(get_map(location = bbox(rtp)))
bm +  geom_polygon(data = rtp, 
                   aes(x = long, y = lat, group = group, 
                       fill = rep(rtp$layer*1e5, each = 5)), 
                   size = 0, 
                   alpha = 0.5) +
   geom_polygon(data = rMaumee.proj,
                  aes(x=long, y=lat,group=group),
                  fill = NA,  colour = "black") +
   scale_fill_gradientn(expression("PR\n"~10^{-5}), 
                       colors = topo.colors(255)) 


plot(rMaumee.proj, add = T)