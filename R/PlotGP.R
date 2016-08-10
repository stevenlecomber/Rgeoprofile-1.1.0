PlotGP <-
function(Window="DEFAULT") {

if (class(Window)=="list") {
	plotWindow = Window
} else {
	plotWindow = list(c(xmin,xmax),c(ymin,ymax))
}

par(mfrow=c(1,1))
close.screen(all=T)

fig.mat<-matrix(c(0.0,0.7,0.0,1,0.7,1,0.0,1),nrow=2,byrow=T)

split.screen(fig.mat)

screen(1)

levels=seq(0,1,length.out=nring+1)

zoomlevel <- MaxZoom(latrange=plotWindow[[2]],lonrange=plotWindow[[1]],size=c(gridsize,gridsize))
MyMap2 <- GetMap(center=c((plotWindow[[2]][1]+plotWindow[[2]][2])/2,(plotWindow[[1]][1]+plotWindow[[1]][2])/2),size=c(gridsize,gridsize),zoom=zoomlevel,maptype=MapType,destfile=paste(Location,"basemap2.png",sep=""))

plot(1,type="n",xlim=plotWindow[[1]],ylim=plotWindow[[2]],xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")
rasterImage(MyMap2$myTile,MyMap2$BBOX$ll[2],MyMap2$BBOX$ll[1],MyMap2$BBOX$ur[2],MyMap2$BBOX$ur[1])
hitscoreraster = CreateRaster(1-hitscoremat,levels,transp)
rasterImage(hitscoreraster,xmin,ymin,xmax,ymax)
Contours(xvec,yvec,matrix=1-hitscoremat,levels)
points(datax,datay,pch=16,cex=0.8,col="red")
if(length(sources) >1 ){points(sourcex,sourcey,pch=15,cex=0.9,col="blue")}

if (class(Window)=="character") {
	if (Window=="ZOOM") {
cat("\nSelect zoom window...\n")
flush.console()

chosenWindow = locator(n=2)
plotWindow = list(sort(chosenWindow[[1]]),sort(chosenWindow[[2]]))

abline(v=plotWindow[[1]],h=plotWindow[[2]])

levels=seq(0,1,length.out=nring+1)

zoomlevel <- MaxZoom(latrange=plotWindow[[2]],lonrange=plotWindow[[1]],size=c(gridsize,gridsize))
MyMap2 <- GetMap(center=c((plotWindow[[2]][1]+plotWindow[[2]][2])/2,(plotWindow[[1]][1]+plotWindow[[1]][2])/2),size=c(gridsize,gridsize),zoom=zoomlevel,maptype=MapType,destfile=paste(Location,"basemap2.png",sep=""))

plot(1,type="n",xlim=plotWindow[[1]],ylim=plotWindow[[2]],xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")
rasterImage(MyMap2$myTile,MyMap2$BBOX$ll[2],MyMap2$BBOX$ll[1],MyMap2$BBOX$ur[2],MyMap2$BBOX$ur[1])
hitscoreraster = CreateRaster(1-hitscoremat,levels,transp)
rasterImage(hitscoreraster,xmin,ymin,xmax,ymax)
Contours(xvec,yvec,matrix=1-hitscoremat,levels)
points(datax,datay,pch=16,cex=0.8,col="red")
if(length(sources) >1 ){points(sourcex,sourcey,pch=15,cex=0.9,col="blue")}

	}	#if zoom
}

screen(2)

par(mar=c(0.1,0,0,0))
plot(1:10,1:10, type="n", axes=F, xlab="", ylab="")
leg.text<- c("Sigma = ", "Tau = ")
legend(1,7.5,c("Cases","Sources"),pch=c(20,15),bty="n",cex=c(0.6),col=c(1,4)) 

lengthbox<-4/(nring+1)

for (i in 1:(nring+1)) {

polygon(c(1,1,3,3),c(6-(lengthbox*i),6-(lengthbox*(i+1)),6-(lengthbox*(i+1)),6-(lengthbox*i)),col=heat.colors(nring)[i],cex=0.6)

}

text(3.3,6.2,"Hit Score Percentage",cex=0.6)

levels2<- rev(levels)

for (i in 1:(nring+1)) {

text(4,(6-(lengthbox*i)),levels2[i],cex=0.6)

}

close.screen(all=T)

}
