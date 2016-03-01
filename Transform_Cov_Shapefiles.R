


##################     FUNCTION:  Transform shapefiles to correct projection and extent     #########################


setwd("C:/Sarah B/Thesis POM/Arc_Layers/Spatial_R_Cov")

library(rgdal)
library(raster)
library(sp)


trans_shps <- function(file_name, plotit = F){
  #  Function: Transform all shapefiles to correct projection and crop to match study area extent
  #  Takes: All covariate data related shapefiles
  #  Returns:  Transformed and cropped shapefiles matching study area
  
  #  Read in shapefiles
  shps <- try(readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Spatial_R_Cov", layer=file_name))
  
  #  Reproject to match study area projection
  #  Desired projection : Albers Equal Area Conic
  sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  trans <- spTransform(shps, CRS=sa.geo)
  
  #if(plotit){                         
   # plot(trans, main = file_name)    
  #}
  
  return(trans)   # returns shapefiles
}

list_shapes <- c("2014_Study_Area", "BF_PROVINCIAL_POLYGON","RFMA_Expanded_Clip")

#list_shapes <- c("2014_Study_Area", "ABMI_LandCover_2010_Diss", "BF_NATIONAL_PARK_POLYGON", "BF_PROVINCIAL_POLYGON", 
                #"Expanded_WMU_Clip", "RFMA_Expanded_Clip", "Expanded_StudyArea_Rivers_Clip", "ATS_PolygonSection_Clip")

cov_dat <- lapply(list_shapes, trans_shps, T)


  ### add this after the reprojecting part of the function if I want to change the extent of shapefiles but not working and doesn't make sense for most shapefiles  
  #  Crop shapefiles to match study area
  #  sa <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Spatial_R_Cov", layer="2014_Study_Area")
  #  match_sa <- crop(trans, sa)
  

# Run indvidual shapefiles through trans_shps

#sa <- trans_shps("2014_Study_Area")

#rivers <- trans_shps("Expanded_StudyArea_Rivers_Clip")

#wmu <- trans_shps("Expanded_WMU_Clip")

#lsd <- trans_shps("ATS_PolygonSection_Clip")

# let's check out the lsd attributes
#summary(lsd)
#head(lsd)
#head(lsd$TWP)


# Plot together for a pretty map

#plot(sa)
#plot(wmu, add=T)
#plot(rivers, col="blue", add=T)