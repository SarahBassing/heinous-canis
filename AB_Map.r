  ##################################################################################################
  ###################                   Spatial Data in R                     ######################
  #################################################################################################


  setwd("C:/Sarah B/Thesis POM/Arc_Layers")
  library(raster)
  library(rgdal)
  library(maptools)
  library(ggplot2)
  library(rgeos)
  
  #install.packages("gpclib", type="source")
  #gpclibPermit()
  
  
  #################################################################
  ##############           Shapefile data            ##############
  #################################################################
  
  # Read in all the shapefiles
  
  sa <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Study Area", layer="2014_Study_Area")
  WMU <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Wildlife Management Units", layer="BF_WMU_POLYGON")
  AB.prov <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Provincial Boundary", layer="BF_PROVINCIAL_POLYGON")
  aprx.hs <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Detected_Homesites", layer="Approx_AB_Homesites_2012-2014")
  abmi.fc <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_LandCover_2010_Diss")
  #abmi.cover <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_Land_Cover_2010_Clip")
  RFMA <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/RFMA- Trapping Districts", layer="RFMAs")
  maj.rivers <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/GeoBase_Roads&Water", layer="Expanded_StudyArea_Rivers_Clip")
  graze.allots <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Grazing_Allotments", layer="GrazAllots_in_WMUrequested_2015May14")
  township <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/AESRD Original Layers/Hydro_ATS_ProvBoundary/ATS_v4.1_Geo_shp/ATS41_NewFormat/geographic shapefile", layer="ATSv41PolygonsTownshipIndex")
  section <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Hunter Surveys", layer="ATS_PolygonSection_Clip")
  
  
  # Check out the original shapefile data- feature type, fields, projection, etc.
  ogrInfo(dsn="C:/Sarah B/Thesis POM/Arc_Layers/ABMI_Land_Cover_2010", layer="ABMI_LandCover_2010_Diss")
  
  
  #################################################################
  ##############        Non shapefile data          ##############
  ################################################################
  
  ## Non shapefile data ##
  
  # Read in DNA location data
  Geno.2012 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2012.csv", head=TRUE)
  Geno.2013 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2013.csv", head=TRUE)
  Geno.2014 <- read.csv("C:/Sarah B/Thesis POM/Data/Genetic/DNA_2014.csv", head=TRUE)
  
  # Subset the full genetic data so it's just ID and location data
  locs12 <- subset(Geno.2012, select=c("Wolf_ID", "Final_Age", "Sample_Site", "UTM_E", "UTM_N"))
  head(locs12)
  
  # Making DNA data spatial
  coordinates(locs12) <- c("UTM_E", "UTM_N")
  
  # Define spatial projection of DNA data
  sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  proj4string(locs12) <- sa.geo
  print(proj4string(locs12))    # Double check it's in the right projection
  
  
  # Subset observations by age class (pup vs. adult samples)
  P12 <- locs12[which(locs12$Final_Age=="P"),]; A12 <- locs12[which(locs12$Final_Age=="A"),]

  
  # Read in hunter survey data
  source("C:/Sarah B/Thesis POM/Scripts/Hunter_Surveys.R")
  # trying to spCbind LSD locations to section layer but FML I can't figure this out and I don't think it's gonna work
  #hs12.match <- match(sec$SEC, HS.12$Section)
  
  
  Hunt12 <- read.csv("C:/Sarah B/Thesis POM/Data/Hunter_Surveys/HS_2012_latlong.csv")
  coordinates(Hunt12) <- c("Lat", "Long")
  proj4string(Hunt12) <- sa.geo
  # the projection is definitely off- I need to figure out how to go from geographic to projected in R before I reproject in projected....
  
  
  
  
  
  
  
  ###############################################################
  ##########         Spatial Data Manipulation        ###########
  ###############################################################
  

  #  Project all the layers
  wmu <- spTransform(WMU, projection(sa))
  ab <- spTransform(AB.prov, projection(sa))
  hs <- spTransform(aprx.hs, projection(sa))
  fc <- spTransform(abmi.fc, projection(sa))
  rfma <- spTransform(RFMA, projection(sa))
  river <- spTransform(maj.rivers, projection(sa))
  allot <- spTransform(graze.allots, projection(sa))
  tsp <- spTransform(township, projection(sa))
  sec <- spTransform(section, projection(sa))
  
  projection(rfma)  # Double check that it's in the right projection now
  
  
  # Trying to disolve all 0 & 1 polygons into single 0 & 1 polygon but not working...
  #IDs <- abmi$Forest_Cov
  #fc.diss <- unionSpatialPolygons(abmi, IDs, threshold=NULL, avoidGEOS=FALSE, avoidUnaryUnion=FALSE)
  #fc.diss <- unionSpatialPolygons(abmi, IDs)
  
  
  
  ######################################################################
  ###############           Start Plotting!           ###################
  #####################################################################
  
  
  # Plot layers together
  plot(ab, border="black")
  plot(wmu, border="gray47", add = T)
  plot(fc, col=c("lemonchiffon", "olivedrab4"), border= FALSE, add=T)
  plot(rfma, border="gray51", add=T)
  plot(allot, border="gray73", lwd=1, add=T)
  plot(tsp, border="gray62", add=T)
  plot(sec, border="gray62", add=T)
  lines(river, col="dodgerblue3", lwd = 1)
  lines(sa, col = "black", lwd = 2)
  points(hs, pch = 17, col = "blue")
  points(locs12, pch = 8, col = "steelblue") # WHY can't I get this to plot over the study area
  plot(locs12, pch=8, col = "steelblue")  # Plots fine alone but not with other shapefiles

  # Create 1000km2 grid across study area
  sa_rat <- raster(extent(sa), crs = projection(sa), res = 1000)  # is the res in 1000 m or km?
  
  # Transfer spatial data from vector data to raster cells (from study area polygon to raster)
  sa_mask <- rasterize(sa, sa_rat)
  
  dev.new()  # New plotting window
  
  plot(sa_mask)
  zoom(sa_mask) # Zoom in on study area raster
  
  xs <- seq(xmin(sa_mask), xmax(sa_mask), by = res(sa_mask)[1])
  ys <- seq(ymin(sa_mask), ymax(sa_mask), by = res(sa_mask)[2])
  
  # Create gridlines across raster based on min and max above
  sapply(xs, function(x) abline(v = xs))
  sapply(ys, function(x) abline(h = ys))  
  