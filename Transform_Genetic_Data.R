
##################     Function: Read in, clean, and transform all genetic data     #########################

setwd("C:/Sarah B/Thesis POM/Data/Genetic")

library(rgdal)
library(raster)
library(sp)

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
  
  #  Define projection - this assumes that the data were collected using this projection
  #sa.geo <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  dna.geo <- CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")    # NAD83(CSRS) / Alberta 10-TM (Resource), NAD 1983 10TM AEP Forest
  
  #  Create spatial object
  sp_obj <- SpatialPointsDataFrame(coordinates(skinny[,c("UTM_E", "UTM_N")]), 
                                   proj4string = dna.geo, 
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



### Make sure projections are correct and DNA locations line up with Study Area

sa <- readOGR(dsn="C:/Sarah B/Thesis POM/Arc_Layers/Spatial_R_Cov", layer="2014_Study_Area")
plot(sa)

spplot(sp_dat[1])
# this gives me an error

