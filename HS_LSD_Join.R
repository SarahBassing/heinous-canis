
#################################################
######   Let's Plot Hunter Surveys Data!   ######
#################################################

source("C:/Sarah B/Thesis POM/Scripts/Hunter_Surveys.R")
source("C:/Sarah B/Thesis POM/Scripts/Transform_Cov_Shapefiles.R")

sa <- trans_shps("2014_Study_Area")
lsd <- trans_shps("ATS_PolygonSection_Clip")

# Check out the lsd attributes
summary(lsd)
head(lsd)
head(lsd$TWP)

# Pull out center coordinates from each section polygon
centroids <- coordinates(lsd)             # centroid point coordinates
x <- centroids[,1]                        # x coordinate
y <- centroids[,2]                        # y coordinate

lsd.df <- as.data.frame(lsd)              # SpatialPolygonDataFrame to DataFrame
lsd.df <- as_data_frame(lsd.df)           # Data Frame with only atomic vectors and lits (whatever that means)

lsd.df[,"centroids"] <- centroids         # Add centroid coordinates to LSD data frame

lsd.hs14 <- left_join(lsd.df, HS.14, by=c("RGE", "TWP", "SEC"))  # Join full LSD and HS data frames
# Warning messages:
#   1: In format.data.frame(df, justify = "left") :
#   corrupt data frame: columns will be truncated or padded with NAs
#   2: In format.data.frame(df[, !too_wide, drop = FALSE]) :
#   corrupt data frame: columns will be truncated or padded with NAs

inner.lsdhs <- inner_join(lsd.df, HS.14, by=c("RGE", "TWP", "SEC"))  # Join only LSD data that matches HS data
# Warning messages:
#   1: In format.data.frame(df, justify = "left") :
#   corrupt data frame: columns will be truncated or padded with NAs
#   2: In format.data.frame(df[, !too_wide, drop = FALSE]) :
#   corrupt data frame: columns will be truncated or padded with NAs




#over(lsd, centroids)   # over() combines two Spatial dataframes--- this is not correct use of over

#tmp <- slot(lsd, 'polygons')
#sub.tmp <- slot(tmp[[1]], 'Polygons')
#sub.tmp[[1]]@labpt                       # another way to get a matrix of the centroid coordinates


