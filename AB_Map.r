  ##################################################################################################
  ###################                   Spatial Data in R                     ######################
  #################################################################################################

  
  
  

  ################################### Bunch o' packages I need #####################################
  
  setwd("C:/Sarah B/Thesis POM/Arc_Layers")
  library(raster)
  library(sp)
  library(rgdal)
  library(maptools)
  library(ggplot2)
  library(rgeos)
  library(gdal)
  library(gdalUtils)
  
  #install.packages("gpclib", type="source")
  #gpclibPermit()
  
  
  
  
  #################################     Genetic Data      #######################################
  #########################          Non shapefile data          ################################
  
  
  ### Function: Read in, clean, and transform all genetic data
  
  morph_gen <- function(file_name, plotit = F){
    #  To make this general spatially, I might consider a from and to projection argument
    #  In one line it does what
    #  Takes
    #  Returns
    
    #  Check that csv extension is on file name and append if needed
    if(!grepl("csv", file_name)){
      file_name <- paste0(file_name, ".csv")
    }
    
    #  Read is csv file
    raw <- try(read.csv(file_name, head = T, as.is = T))
    
    #  Select columns of interest
    skinny <- subset(raw, select=c("Wolf_ID", "Final_Age", "Sample_Site", "UTM_E", "UTM_N"))
    
    #  Define projection - this assumes that the data were collected using this projection--- need to fix this
    sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    
    #  Create spatial object
    sp_obj <- SpatialPointsDataFrame(coordinates(skinny[,c("UTM_E", "UTM_N")]), 
                                        proj4string = sa.geo, 
                                        data = skinny[,1:3])  
    
    # Reproject to match study area projection
    sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    wolf_geno <- spTransform(sp_obj, CRS=sa.geo)
    
#    if(plotit){
#      plot(sp_obj, main = file_name, col = (sp_obj$Final_Age == "A") + 1, pch = 19)    # +1 b/c logical == is "A" F/T => 0/1 numeric equivilant need to add 1 so it matches up with vector c(1,2)... (0,1) +1 = (1,2)
#      legend("topright", legend = c("Pup", "Adult"), col = c(1, 2), pch = 19)
#    }
    
#  return(sp_obj)  
    
    if(plotit){
      plot(wolf_geno, main = file_name, col = (wolf_geno$Final_Age == "A") + 1, pch = 19)    # +1 b/c logical == is "A" F/T => 0/1 numeric equivilant need to add 1 so it matches up with vector c(1,2)... (0,1) +1 = (1,2)
      legend("topright", legend = c("Pup", "Adult"), col = c(1, 2), pch = 19)
    }
    
    return(wolf_geno)
  }
  
  #  Define file names - consider list.files?
  fnames <- c("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2012.csv",
              "C:/Sarah B/Thesis POM/Data/Genetic/DNA_2013.csv",
              "C:/Sarah B/Thesis POM/Data/Genetic/DNA_2014.csv")

  sp_dat <- lapply(fnames, morph_gen, T)
  
#   
#   # Read in DNA location data
#   Geno.2012 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2012.csv", head=TRUE)
#   Geno.2013 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2013.csv", head=TRUE)
#   Geno.2014 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2014.csv", head=TRUE)
#   
#   # Subset the full genetic data so it's just ID and location data
#   locs12 <- subset(Geno.2012, select=c("Wolf_ID", "Final_Age", "Sample_Site", "UTM_E", "UTM_N"))
#   locs13 <- subset(Geno.2013, select=c("Wolf_ID", "Final_Age", "Sample_Site", "UTM_E", "UTM_N"))
#   locs14 <- subset(Geno.2014, select=c("Wolf_ID", "Final_Age", "Sample_Site", "UTM_E", "UTM_N"))
#   
#   head(locs12)    # Double check it looks right
# 
#   sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#   dna.geo <- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
# 
#  
#   sp_locs12 <- SpatialPointsDataFrame(coordinates(locs12[,c("UTM_E", "UTM_N")]), 
#                                       proj4string = dna.geo, 
#                                       data = locs12)
#   
#   # Making DNA data spatial
#   coordinates(locs12) <- c("UTM_E", "UTM_N")
#   coordinates(locs13) <- c("UTM_E", "UTM_N")
#   coordinates(locs14) <- c("UTM_E", "UTM_N")
#   
#   # Define spatial projection of DNA data
#   proj4string(locs12) <- sa.geo
#   proj4string(locs13) <- sa.geo
#   proj4string(locs14) <- sa.geo
#   
#   print(proj4string(locs12))    # Double check it's in the right projection
  
  
  # Subset observations by age class (pup vs. adult samples)
  P12 <- locs12[which(locs12$Final_Age=="P"),]; A12 <- locs12[which(locs12$Final_Age=="A"),]
  P13 <- locs13[which(locs13$Final_Age=="P"),]; A13 <- locs13[which(locs13$Final_Age=="A"),]
  P14 <- locs14[which(locs14$Final_Age=="P"),]; A14 <- locs14[which(locs14$Final_Age=="A"),]
  
  
  
  
  #################################       Hunter Survey Data       ###############################
  ###################################     Non shapefile data     #################################
  
  # Read in hunter survey data
  source("C:/Sarah B/Thesis POM/Scripts/Hunter_Surveys.R")
  # trying to spCbind LSD locations to section layer but FML I can't figure this out and I don't think it's gonna work
  hs12.match <- match(section$SEC, HS.12$Section)
  hs12.over <- over(HS.12$Section, section$SEC, returnList=TRUE)
  
  ## fuck it, I'll use the location data I already have...
  Hunt12 <- read.csv("C:/Sarah B/Thesis POM/Data/Hunter_Surveys/HS_2012_latlong.csv")
  coordinates(Hunt12) <- c("Lat", "Long")
  proj4string(Hunt12) <- sa.geo
  # the projection is definitely off- I need to figure out how to go from geographic to projected in R before I reproject in projected....
 
  
  # township <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/AESRD Original Layers/Hydro_ATS_ProvBoundary/ATS_v4.1_Geo_shp/ATS41_NewFormat/geographic shapefile", layer="ATSv41PolygonsTownshipIndex")
  # section <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Hunter Surveys", layer="ATS_PolygonSection_Clip")
  
  LSD <- 
  
   
  ################################################################################################
  ###########################         Spatial Data Manipulation       ############################
  ################################################################################################
  
  ################################################################################################
  ###############################           Shapefile data            ############################
  ################################################################################################
  
  
  ##########################        Study Area & Grid Cells          #############################
  
  # Read in study area, AB provincial boundary, & major rivers shapefiles
  sa <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Study Area", layer="2014_Study_Area")
  AB.prov <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Provincial Boundary", layer="BF_PROVINCIAL_POLYGON")
  maj.rivers <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/GeoBase_Roads&Water", layer="Expanded_StudyArea_Rivers_Clip")

  # Check out the original shapefile data- feature type, fields, projection, etc.
  ogrInfo(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_LandCover_2010_Diss")
  
  
  # Reproject to match study area's projection
  ab <- spTransform(AB.prov, projection(sa))
  river <- spTransform(maj.rivers, projection(sa))
  
  
  
  # Create 1000km2 grid across study area (L.GRID)
  sa_rat <- raster(extent(sa), crs = projection(sa), res = 31622.7766)  # res is meters- hight & width of cell
  
  # Transfer spatial data from vector data to raster cells (from study area polygon to raster)
  sa_mask <- rasterize(sa, sa_rat)

  # Making x & y coordinates based on study area raster (L.GRID)
  xs <- seq(xmin(sa_mask), xmax(sa_mask), by = res(sa_mask)[1])
  ys <- seq(ymin(sa_mask), ymax(sa_mask), by = res(sa_mask)[2])
  
  # Create gridlines across raster based on x & y min & maxes above-- if I want to visualize the grid cell boundaries
  sapply(xs, function(x) abline(v = xs))   #but they don't match up with the sa_mask cells...?
  sapply(ys, function(x) abline(h = ys))  
  

  # Create 100m2 grid across study area (S.GRID)
  sa_smrat <- raster(extent(sa), crs = projection(sa), res = 100)  # res is meters- hight & width of cell
  
  # Transfer spatial data from vector data to raster cells (from study area polygon to raster)
  sa_smmask <- rasterize(sa, sa_smrat)
  
  
  # Plotting & viewing L.GRID and S.GRID
  dev.new()  # New plotting window
  
  plot(sa_mask)
  zoom(sa_mask) # Zoom in on study area raster
  
  dev.new()  # New plotting window
  
  plot(sa_smmask)
  zoom(sa_smmask) # Zoom in on study area raster
  
  
  
  
  #################################        Forest Cover        ###################################
  
  # Read in ABMI forest cover shapefile- this has already been altered to 0 = open, 1 = forest cover
  abmi.fc <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_LandCover_2010_Diss")
  #abmi.cover <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_Land_Cover_2010_Clip")
  
  # Reproject ABMI forest cover to match study area
  fc <- spTransform(abmi.fc, projection(sa))
  
  
  # Trying to disolve all 0 & 1 polygons into single 0 & 1 polygon but not working...
  #IDs <- abmi$Forest_Cov
  #fc.diss <- unionSpatialPolygons(abmi, IDs, threshold=NULL, avoidGEOS=FALSE, avoidUnaryUnion=FALSE)
  #fc.diss <- unionSpatialPolygons(abmi, IDs)

  fc.raster <- raster(extent(sa), crs= projection(sa), res= 100)  # make a 100 m2 FC raster
  
  #  This probably won't work in parallel as written, see raster parallel processing for more info
  beginCluster(n=3)	   ### I can't tell if this is working....  # wanted to use beginCluster(sys.getenvi()["NUMBER_OF_PROCESSORS"]-1) but snow package gave an error
    fc.raz <- rasterize(fc, fc.raster)  # rasterize the FC raster with data from FC shapefile
  endCluster()
  
  beginCluster(as.numeric(Sys.getenv()["NUMBER_OF_PROCESSORS"])-1)	
    fc.mask <- mask(x=fc.raster, mask=fc.raz)  # mask rasterized FC raster with the study area
      ## still unclear what I did here... ###
  endCluster()
  
  #  Aggregate to larger cell size by sum(x)/length(x)
  #  Finally extract by cell
  
  
  ############################         Wolf Harvest        ######################################
  
  # Read in Wildlife Management and Registered Fur Management Areas shapefiles
  WMU <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Wildlife Management Units", layer="BF_WMU_POLYGON")
  RFMA <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/RFMA- Trapping Districts", layer="RFMAs")
  
  # Reproject to match study area projection
  wmu <- spTransform(WMU, projection(sa))
  rfma <- spTransform(RFMA, projection(sa))
  
  
  
  
  
  ############################       Cattle Grazing        ######################################
  
  # Read in grazing allotment, grazing leases, and LSC shapefiles 
  graze.allots <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Grazing_Allotments", layer="GrazAllots_in_WMUrequested_2015May14")
  
  
  allot <- spTransform(graze.allots, projection(sa))
  
  
  
  
  
  
  ############################       DEM- Elevation & Slope       ###############################
  
  
  library(rgdal)
  
  dem <- readGDAL("C:/Sarah B/Thesis POM/Arc_Layers/dem25/DEM_25")                         # read a grid (format is automatically detected (.img))
  str(dem)        # not sure this will work?
  
  image(dem, col=grey(1:99/100), axes=T)
  summary(dem)
  
  # OR? #
  
  dem <- raster("DEM_25.img")
  plot(dem, col=grey(1:99/100), main="DEM 25m")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(parallel)
  library(snow)
    
  # Read in DEM .img file
  DEM <- stack("C:/Sarah B/Thesis POM/Arc_Layers/dem25/dem_25.img")   # stack() part of raster package to read in .img files
  #  crop to study area
  #  transform
  
  
  
  
  
  
  
  
  

  #  Project all the layers
  saproj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  
  
  
  #tsp <- spTransform(township, projection(sa))
  #sec <- spTransform(section, projection(sa))   # FML
  #dem <- projectRaster(DEM, crs=saproj)   # this one takes awhile cuz it's a giant ass raster
  
  # Reprojecting DEM
  dems <- list.files("C:/Sarah B/Thesis POM/Arc_Layers/dem25", pattern="img")
  gtiffs <- sub("img", "tif", dems)
  
  # Define projections
  from_proj <- "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  to_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Convert img to tif
  gdal_translate(dems, gtiffs)  # gdal_translate doens't work b/c gdal not installed- which one do I install?
  
  # Project tiffs with gdalwarp
  
  
  
  projection(rfma)  # Double check that it's in the right projection now
  
  
  
  
  
  
  ######################################################################
  ###############           Start Plotting!           ###################
  #####################################################################
  
  
  # Plot layers together
  plot(ab, border="black")
  plot(DEM)
  plot(wmu, border="gray47", add = T)
  plot(fc, col=c("lemonchiffon", "olivedrab4"), border= FALSE, add=T)
  plot(rfma, border="gray51", add=T)
  plot(allot, border="gray73", lwd=1, add=T)
  #plot(tsp, border="gray62", add=T)
  #plot(sec, border="gray62", add=T)
  lines(river, col="dodgerblue3", lwd = 1)
  lines(sa, col = "black", lwd = 2)
  points(hs, pch = 17, col = "blue")
  points(locs12, pch = 8, col = "steelblue") # WHY can't I get this to plot over the study area
  plot(locs12, pch=8, col = "steelblue")  # Plots fine alone but not with other shapefiles
  

  
  
  
  #################################################################
  ## Grid cells ##
  r <- raster(ncol=40, nrow=30)
  projection(r) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  
  
  
  ## cropping everything because that's a function I actually got to work ##
  wmu.sa <- crop(wmu, sa)
  fc.sa <- crop(fc,sa)
  rfma.sa <- crop(rfma, sa)
  allot.sa <- crop(allot, sa)
  dem.sa <- crop(DEM, sa)    # doen'st work b/c the extents don't overlap- need to reproject DEM
  
  
  

  ### trying to mask or extract or whatever the fuck I'm doing here  ###
  mask(sa_mask, wmu)   # overlay grid cells (x) across wmu (y) so that so that wmu within grid takeon grid values and wmu outside grid are NA... why do I want to do this?
  
 
  
  
  
  raz <- rasterize(sa, DEM)   # can't try this until I figure out how to reproject the DEM file
  lr <- mask(x=dem.sa, mask=raz)
  
  wmu.grid <- extract(sa_mask, wmu)    # what does this output mean?!
  extract(sa_mask, fc, getCover=T)   # again, what does this output mean?????
  
  beginCluster(n=3)
  FCov <- extract(sa_mask, fc, getCover=T)
  endCluster()