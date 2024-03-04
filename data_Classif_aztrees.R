library(data.table)
library(ggplot2)
aztrees <- fread(
  "data_Classif_aztrees.csv"
)[, `:=`(
  region4 = fcase(
    xcoord < -111.75 & ycoord > 35.2, "NW",
    xcoord < -111.6 & ycoord < 35.1, "SW",
    xcoord > -111.625 & ycoord < 35.175, "SE",
    default="NE"),
  polygon = Joint_id,
  y = ifelse(type_id==17, "Tree", "Not tree")
)][
, region3 := ifelse(grepl("S", region4), "S", region4)
]
aztrees[, table(region4, y)]
## The variable "Joint_type" is for reference only. Please, consider the variable "type_id" as a primary-key variable for Land use/Land cover classes. Find below the description of each class:
## type_id     Description
## 1           Open water
## 4           Infrastructure (mostly white: roofs, some buildings, sidewalks, etc.) 
## 5           Infrastructure (mostly black: streets, routes, parking places, pavements, etc.)
## 7           Planted grasses (intense green, golf fields, football/soccer fields, etc.)
## 8           Natural grass
## 10          Bare grounds
## 17          Trees/shrubs
## type_id = Class code
## Joint_type = Class name
## Jount_id = Sampling polygon ID
## xcoord = X geographic coordinates WGS84
## ycoord = Y geographic coordinates WGS84
## SAMPLE_1 =  SENTINEL-2, band 2
## SAMPLE_2 = SENTINEL-2, band 3
## SAMPLE_3 = SENTINEL-2, band 4
## SAMPLE_4 = SENTINEL-2, band 5
## SAMPLE_5 = SENTINEL-2, band 6
## SAMPLE_6 = SENTINEL-2, band 7
## SAMPLE_7 = SENTINEL-2, band 8
## SAMPLE_8 = SENTINEL-2, band 8A
## SAMPLE_9 = SENTINEL-2, band 11
## SAMPLE_10 = SENTINEL-2, band 12 
## pSAMPLE_11 = SENTINEL-2, NDVI index
## SAMPLE_12 = SENTINEL-2, NDDI index
## SAMPLE_13 = SENTINEL-2, NDWI index
## SAMPLE_14 = SENTINEL-1, VV mean (mean of VV polarization)
## SAMPLE_15 = SENTINEL-1, VH mean (mean of VH polarization)
## SAMPLE_16 = SENTINEL-1, VV std (standard deviation of VV polarization)
## SAMPLE_17 =SENTINEL-1, VH std ((standard deviation of VV polarization)
## SAMPLE_18 = TPI_10 (Topographic position index at 10 radius resolution)
## SAMPLE_19 = TPI_15 (Topographic position index at 15 radius resolution)
## SAMPLE_20 = TPI_20 (Topographic position index at 20 radius resolution)
## SAMPLE_21 = Slope (terrain slope in degrees)
aztrees[, .(
  polygons=length(unique(polygon)),
  rows=.N
), keyby=.(region4,y)]
aztrees[, .(
  polygons=length(unique(polygon)),
  rows=.N
), keyby=.(region3,y)]
ggplot()+
  geom_point(aes(
    xcoord, ycoord, color=region3),
    data=aztrees)
ggplot()+
  geom_point(aes(
    xcoord, ycoord, color=region4),
    data=aztrees)
ggplot()+
  geom_point(aes(
    xcoord, ycoord, color=y),
    data=aztrees)
feature.names <- grep("SAMPLE",names(aztrees),value=TRUE)
for(n.regions in 3:4){
  region.col <- paste0("region",n.regions)
  fwrite(
    aztrees[
    , c(region.col,"y",feature.names)
    , with=FALSE],
    sprintf("data_Classif/aztrees%d.csv", n.regions))
}
fwrite(
  aztrees[, c("region3","region4","polygon","y",feature.names), with=FALSE],
  "data_Classif_aztrees_regions.csv")
