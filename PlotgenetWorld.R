library(raster)
library(rgdal)
library(shapefiles)
library(gam)
library(poppr)
library(scales)

setwd("~/Uniandes/Tesis/Global Phylogeo")

#Making the RGB table
population <- read.genalex("Test-poppr.csv",ploidy = 3)
dapc.test <- dapc(population, var.contrib = TRUE, scale = FALSE, n.pca = NULL, n.da = nPop(population) - 1)
scatter(dapc.test, cell = 0, pch = 18:23, cstar = 0, mstree = TRUE, lwd = 2, lty = 2)
location <-read.csv("Local.csv")

table <- data.frame(Id=rownames(population@tab),x=location$Lat,y=location$Long,R=as.numeric(rescale(dapc.test$ind.coord[,1],c(0,255))) , G=as.numeric(rescale(dapc.test$ind.coord[,2],c(0,255))), B=as.numeric(rescale(dapc.test$ind.coord[,3],c(0,255))))

ddShapefile <- convert.to.shapefile(table[,c("Id","x","y")],table[,c("Id","R","G","B")],"Id",1)
#write.shapefile(ddShapefile, "ejemplo", arcgis=T)
#ejemplo <-readOGR("ejemplo.shp")
#plot(ejemplo)

world <- raster("DEM_img/alwdgg.img")
coordWorld=as.data.frame(coordinates(world))
                       
result_R <- gam(R~x+y,family = "gaussian",data = table)
predict.Gam(result_R)
result_G <- gam(G~x+y,family = "gaussian",data = table)
predict.Gam(result_G)
result_B <- gam(B~x+y,family = "gaussian",data = table)
predict.Gam(result_B)

R_Worldpredicted <- world 
values(R_Worldpredicted) <-predict.Gam(result_R,newdata = coordWorld)
G_Worldpredicted <- world 
values(G_Worldpredicted) <- predict.Gam(result_G,newdata = coordWorld)
B_Worldpredicted <- world 
values(B_Worldpredicted) <- predict.Gam(result_B,newdata = coordWorld)

predictWorld <- stack(R_Worldpredicted,G_Worldpredicted,B_Worldpredicted)

writeRaster(predictWorld, "predictWorld.tiff", format = "GTiff",overwrite=T)

#worldMap <- readOGR("world_adm0.shp")
