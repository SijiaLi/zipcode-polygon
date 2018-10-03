setwd("~/Downloads/zipcode-polygon")

library(rgdal)
library(raster)

# Get a list of LA County Zipcodes from the shapefile
zip.map <- readOGR('./shapefile data/geo_export_04047602-3e3d-40b5-8318-2dca2dc5cb78.shp')
ziplist = as.vector(zip.map$zipcode)

# Get a list of LA County Zipcodes and their pin location from the csv file
zip.full = read.csv("us_postal_codes.csv")
zip.la = zip.full[zip.full$Zip.Code %in% ziplist,]

##############################
##############################

# Read in SPA shapefile
spa.map <- readOGR('./shapefile data/geo_export_8907f597-4d25-4039-ab5e-21698163a119.shp')

# Construct the pin locations for projection
dat <- data.frame(Longitude = zip.la$Longitude, Latitude = zip.la$Latitude, names = zip.la$Zip.Code)

# Assignment modified according
coordinates(dat) <- ~ Longitude + Latitude

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(dat) <- proj4string(spa.map)

# Project dat over spa.map and generate a final list
result = data.frame(zipcode = dat$names, over(dat, spa.map))
result = result[c("zipcode", "spa_2012", "spa_name")]

# Deal with a few missing values
problem = result[is.na(result$spa_2012), ]

maps_union <- union(zip.map, spa.map)
df <- data.frame(zipcode = maps_union$zipcode, spa_2012 = maps_union$spa_2012, spa_name = maps_union$spa_name)
subset = df[df$zipcode %in% problem$zipcode, ]
subset = subset[!is.na(subset$spa_2012), ] 

# from this "subset" dataset we can see only 4 zipcodes still have unidentified spa: 90630, 90631, 92397, 93243. 
# and we mannually checked map to assign the values as
# 90630, spa 7; 90631, spa 7; 92397, spa3; 93243, spa1

rows.to.remove <- with(subset, (zipcode == 90630 & spa_2012 != 7) | (zipcode == 90631 & spa_2012 != 7) | (zipcode == 92397 & spa_2012 != 3) | (zipcode == 93243 & spa_2012 != 1))
subset = subset[!rows.to.remove, ]
result = result[!is.na(result$spa_2012), ]
final.spa <- rbind(result, subset)


##############################
##############################

# Project points in dat to Supervisorial District polygons
sd.map <- readOGR('./shapefile data/geo_export_8b57dca9-7f07-4362-85e6-13e7829b3c81.shp')
proj4string(dat) <- proj4string(sd.map)
result = data.frame(zipcode = dat$names, over(dat, sd.map))
result = result[c("zipcode", "district", "label")]

# Deal with a few missing values
problem = result[is.na(result$district), ]

# Using maps_union will give an error because too few points near an area
# In this case using intersect works
maps_intersect <- intersect(zip.map, sd.map)
df <- data.frame(zipcode = maps_intersect$zipcode, district = maps_intersect$district, label = maps_intersect$label)
subset = df[df$zipcode %in% problem$zipcode, ]

# from this "subset" dataset we can see all zipcode has one and only solution so just append it to the result
result = result[!is.na(result$district), ]
final.sd <- rbind(result, subset)

##############################
##############################

final.df <- merge(final.spa, final.sd)
write.csv(final.df, file = "jurisdiction_mapping_table.csv")




