library(plyr)
library(dplyr)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)

# Read lpr_00b16a_e.shp (Canada)
can<-readOGR(dsn="data/canada-map", layer="lpr_000b16a_e")
can@data$id<-rownames(can@data)
can2<-can[can@data$PRENAME %in% c("Nova Scotia","Prince Edward Island","Newfoundland and Labrador","New Brunswick","Quebec"),]
rm(can)

# This keeps everything but fills in with zeros.
# To fix this, write out shapefile, read back in, then write working shapefile
writeOGR(can2,".","data/atlanticcanada-map/atlanticcanada-map",driver="ESRI Shapefile")
rm(can2)
ac<-readOGR(dsn="data/atlanticcanada-map","atlanticcanada-map")
writeOGR(ac,".","atlanticcanada-map",driver="ESRI Shapefile")

# Save as .csv to save time in the future...
ac.points<-fortify(ac, region="id")
write.csv(ac.points, "data/atlanticcanada-map/acpoints.csv", row.names=FALSE)

# Save as .csv to save time in the future...
ac.df<-join(ac.points, ac@data, by="id")
write.csv(ac.df, "data/atlanticcanada-map/acdf.csv", row.names=FALSE)
