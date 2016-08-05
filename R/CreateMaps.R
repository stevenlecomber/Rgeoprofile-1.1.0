CreateMaps <-
function(PlotPrior=T){
  
  xmin <<- (max(datax)+min(datax))/2 - (0.5+Guardrail)*(max(datax)-min(datax))
  xmax <<- (max(datax)+min(datax))/2 + (0.5+Guardrail)*(max(datax)-min(datax))
  ymin <<- (max(datay)+min(datay))/2 - (0.5+Guardrail)*(max(datay)-min(datay))
  ymax <<- (max(datay)+min(datay))/2 + (0.5+Guardrail)*(max(datay)-min(datay))

  zoomlevel <- MaxZoom(latrange=c(ymin,ymax),lonrange=c(xmin,xmax),size=c(gridsize,gridsize))
  MyMap <- GetMap(center=c((ymin+ymax)/2,(xmin+xmax)/2),size=c(gridsize,gridsize),zoom=zoomlevel,maptype=MapType,destfile=paste(Location,"basemap.png",sep=""))

  priorx <<- (xmin+xmax)/2
  priory <<- (ymin+ymax)/2
  
  if (tau=="DEFAULT") {
    xdiff <<- max(datax)-min(datax)
    ydiff <<- max(datay)-min(datay)
    tau <<- max(c(xdiff,ydiff))
  }
  
  xvec <<- seq(xmin,xmax,length.out=gridsize2)
  yvec <<- seq(ymin,ymax,length.out=gridsize2)
  xmat <<- outer(rep(1,gridsize2),xvec)
  ymat <<- outer(yvec[gridsize2:1],rep(1,gridsize2))
  priormat <<- dnorm(xmat,mean=priorx,sd=tau)*dnorm(ymat,mean=priory,sd=tau)

  levels=seq(0,1,length.out=nring+1)
  if (PlotPrior==TRUE){
    par(mfrow=c(1,1))
    plot(1,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")
    rasterImage(MyMap$myTile,MyMap$BBOX$ll[2],MyMap$BBOX$ll[1],MyMap$BBOX$ur[2],MyMap$BBOX$ur[1])
    priorraster = CreateRaster(priormat,levels,transp)
    rasterImage(priorraster,xmin,ymin,xmax,ymax)
    par(new=T); contour(xvec,yvec,t(priormat/max(priormat)),xaxs="i",yaxs="i",levels=levels,axes=F,drawlabels=F,col="dark grey")
    points(datax,datay,pch=20,cex=0.8,col=pointcol)
  }
  else (cat("Prior map not plotted"))
  
}
