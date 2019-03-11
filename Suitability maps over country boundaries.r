library(maps)
library(mapdata)
library(maptools)
require(rasterVis)
require(gridExtra)
sk <- stack()
for (s in seq_along(ls(pattern="H_sapiens_forecast_suit_")))
{
Hsap_suit <- get(ls(pattern="H_sapiens_forecast_suit_")[s])
ext <- as.vector(extent(Hsap_suit))
### Get the wolrd map bnoundaries of our raster 
boundaries <- map("worldHires", fill=TRUE, xlim=ext[1:2], ylim=ext[3:4], plot=FALSE) 
####return the country names for the specific boundaries
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1]) 
### convert map to spatialpoygondataframe with the countries specified above and our raster pprojection ###
bPols <- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=CRS(projection(Hsap_suit))) 
p<- levelplot(Hsap_suit, margin=FALSE, main = list(paste0("H.sapiens_", sdm_bins[s], "ka", sep=""), 
cex=0.85), col.regions = colorRampPalette(c("grey90", "orange", "dark red"))(100)) + layer(sp.polygons(bPols, alpha=0.9))
assign(paste("p_",sdm_bins[s], "k", sep=""), p)
}
