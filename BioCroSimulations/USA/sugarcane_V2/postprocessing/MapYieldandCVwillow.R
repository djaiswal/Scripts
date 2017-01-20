 library(maps)
library(sp)
library(maptools)
gpclibPermit()
library(RColorBrewer)
library(classInt)
library(doBy)

#############################
ReadBioCroOutput<- function(ResultFolder,first.year,last.year) {
       resUS<-NULL
       for(i in first.year:last.year){
           tmp <- try(read.table(paste(ResultFolder,"/res",i,".txt",sep="")),silent=TRUE)
              if(class(tmp) != "try-error"){
              tmp<-summaryBy(V2 +V3 +V4+V5+V6+V7+V8+V9+V10+V11+V12 +V13 +V14+ V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25~ V1,data=tmp,FUN=mean,keep.names=TRUE)
              resUS <- rbind(resUS,tmp)
                                          }
                                    }
              resUS
     }

###########################


EvaluateMeanAndCV<-function (resUS,varname) {
    if(varname=="StemDB1"){
         ttemp2<-summaryBy(Lat + Lon + StemDB1 ~ ID,data=resUS,FUN=sd)
         ttemp2<-na.omit(ttemp2)
         ttemp<-summaryBy(Lat + Lon + StemDB1 ~ ID,data=resUS,FUN=mean)
         res.dat <- data.frame(Lat=ttemp$Lat.mean,Lon=ttemp$Lon.mean,harvDB=ttemp$StemDB1.mean)
         res.dat <- na.omit(res.dat)
         harvDB1.sd<-ttemp2$StemDB1.sd
         ttemp2<-cbind(ttemp2,harvDB1.sd)
         res.dat<-cbind(res.dat,ttemp2$harvDB1.sd)
         cv<-res.dat[,4]/res.dat[,3]
         res.dat<-cbind(res.dat,cv) 
                           }
       if(varname=="avgstem"){
         ttemp2<-summaryBy(Lat + Lon + avgstem ~ LOC,data=resUS,FUN=sd)
         ttemp2<-na.omit(ttemp2)
         ttemp<-summaryBy(Lat + Lon + avgstem ~ LOC,data=resUS,FUN=mean)
         res.dat <- data.frame(Lat=ttemp$Lat.mean,Lon=ttemp$Lon.mean,harvDB=ttemp$avgstem.mean)
         res.dat <- na.omit(res.dat)
         harvDB1.sd<-ttemp2$avgstem.sd
         ttemp2<-cbind(ttemp2,harvDB1.sd)
         res.dat<-cbind(res.dat,ttemp2$harvDB1.sd)
         cv<-res.dat[,4]/res.dat[,3]
         res.dat<-cbind(res.dat,cv) 
                           }
         res.dat
  }
DefineMapScheme<-function(datatoplot,breakpoints=NULL,nclr=NULL){

        if(!is.numeric(breakpoints)){
        if(!is.numeric(nclr)) nclr=10
            plotclr <- brewer.pal(nclr,"Spectral")
            plotclr <- plotclr[length(plotclr):1]
            class <- classIntervals(datatoplot, nclr, style="pretty")
            colcode <- findColours(class, plotclr)
             } else {
                nclr<-length(breakpoints)-1
                plotclr <- brewer.pal(nclr,"Spectral")
                plotclr <- plotclr[length(plotclr):1]
                class <- classIntervals(datatoplot, nclr, style="pretty")
                class$brks <-breakpoints
                colcode <- findColours(class, plotclr)
              }
           colcode
       }

##################################
plotcolorpoints<-function(shapefilelocation,datatoplot,mapfolder,outfile,colcode){
  shapefile<-readShapePoly(shapefilelocation)
  pts <- SpatialPoints(cbind(datatoplot$Lon,datatoplot$Lat))
  sze<-1000
  png(paste(mapfolder,outfile,sep=""), height = sze, width = sze, res = 120)
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(shapefile)
  plot(pts,col=colcode,cex=0.76,pch=8,add=TRUE)
  plot(shapefile,add=TRUE)
  legend("bottomright", legend=names(attr(colcode, "table")),
  fill=attr(colcode, "palette"), cex=0.9, bty="n")
  text(-100,53,labels=outfile)
  dev.off()
  return
}


########################
MapYieldandCV<-function(first.year,last.year,ResultFolder,mapfolder,shapefilelocation,yieldbreakpoints=NULL,cvbreakpoints=NULL,nclr=NULL,cvmax=NULL) {

resUS<-ReadBioCroOutput(ResultFolder,first.year,last.year)
colnames(resUS) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem", "year")

YearlyYield <- resUS[,c("Lat","Lon","year","avgstem")]
write.table(YearlyYield,file=paste(mapfolder,"yearlyyield.txt"))

# I should make harvest Index as an Factor in BioCro to avoid this calculation here
# This is species dependent

#Create data framr of mean and CV of harvestable  stem Dry biomass only
res.dat<-EvaluateMeanAndCV(resUS,"avgstem")

write.table(res.dat,file=paste(mapfolder,"summary.txt"))
#Plotting Mean Yields
colorforyield<-DefineMapScheme(datatoplot=res.dat$harvDB,breakpoints=yieldbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="yield.png",colcode=colorforyield)

# Plotting CV of Yields
if(!is.null(cvmax))res.dat[res.dat$cv>cvmax,]$cv=cvmax
colorforyield<-DefineMapScheme(datatoplot=res.dat$cv,breakpoints=cvbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="cv.png",colcode=colorforyield)

return

}

MapYieldandCVDifference1<-function(first.year,last.year,ResultFolder1,ResultFolder2,mapfolder,shapefilelocation,yieldbreakpoints=NULL,cvbreakpoints=NULL,nclr=NULL,cvmax=NULL) {

resUS<-ReadBioCroOutput(ResultFolder1,first.year,last.year)
colnames(resUS) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem","year")

# I should make harvest Index as an Factor in BioCro to avoid this calculation here
# This is species dependent

#Create data framr of mean and CV of harvestable  stem Dry biomass only
res.dat<-EvaluateMeanAndCV(resUS,"avgstem")



resUS<-NULL
resUS<-ReadBioCroOutput(ResultFolder2,first.year,last.year)
colnames(resUS) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem","year")

# I should make harvest Index as an Factor in BioCro to avoid this calculation here
# This is species dependent

#Create data framr of mean and CV of harvestable  stem Dry biomass only
res.dat2<-EvaluateMeanAndCV(resUS,"avgstem")

# Get the difference

res.dat$harvDB=(res.dat$harvDB-res.dat2$harvDB)*100/(res.dat$harvDB)
write.csv(res.dat,file=paste(mapfolder,"willowsummary.csv"), row.names=FALSE)
#Plotting Mean Yields
colorforyield<-DefineMapScheme(datatoplot=res.dat$harvDB,breakpoints=yieldbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="yield.png",colcode=colorforyield)

# Plotting CV of Yields
if(!is.null(cvmax))res.dat[res.dat$cv>cvmax,]$cv=cvmax
colorforyield<-DefineMapScheme(datatoplot=res.dat$cv,breakpoints=cvbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="cv.png",colcode=colorforyield)

return

}

MapYieldandCVCoppicing<-function(first.year,last.year,ResultFolder,mapfolder,shapefilelocation,yieldbreakpoints=NULL,cvbreakpoints=NULL,nclr=NULL,cvmax=NULL) {


resUS<-ReadBioCroOutput(ResultFolder,first.year,last.year)
colnames(resUS) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem", "avgTemp","ToalPrecip","avgRad","awc","mdep","year")

# I should make harvest Index as an Factor in BioCro to avoid this calculation here
# This is species dependent

#Create data framr of mean and CV of harvestable  stem Dry biomass only

resUS$avgstem=resUS$Stem1+resUS$Stem2+resUS$Stem3+resUS$Stem4+resUS$Stem5
resUS$avgstem=resUS$avgstem
res.dat<-EvaluateMeanAndCV(resUS,"avgstem")
write.csv(res.dat,file=paste(mapfolder,"willowsummary.csv"), row.names=FALSE)
#Plotting Mean Yields
colorforyield<-DefineMapScheme(datatoplot=res.dat$harvDB,breakpoints=yieldbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="yield.png",colcode=colorforyield)

# Plotting CV of Yields
if(!is.null(cvmax))res.dat[res.dat$cv>cvmax,]$cv=cvmax
colorforyield<-DefineMapScheme(datatoplot=res.dat$cv,breakpoints=cvbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="cv.png",colcode=colorforyield)

return

}

MapYieldandCVContinuous<-function(first.year,last.year,ResultFolder,mapfolder,shapefilelocation,yieldbreakpoints=NULL,cvbreakpoints=NULL,nclr=NULL,cvmax=NULL) {
resUS<-ReadBioCroOutput(ResultFolder,first.year,last.year)
colnames(resUS) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem", "avgTemp","ToalPrecip","avgRad","awc","mdep","year")

# I should make harvest Index as an Factor in BioCro to avoid this calculation here
# This is species dependent

resUS$avgstem=resUS$Stem5
#Create data framr of mean and CV of harvestable  stem Dry biomass only
res.dat<-EvaluateMeanAndCV(resUS,"avgstem")
write.csv(res.dat,file=paste(mapfolder,"willowsummary.csv"), row.names=FALSE)
#Plotting Mean Yields
colorforyield<-DefineMapScheme(datatoplot=res.dat$harvDB,breakpoints=yieldbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="yield.png",colcode=colorforyield)

# Plotting CV of Yields
if(!is.null(cvmax))res.dat[res.dat$cv>cvmax,]$cv=cvmax
colorforyield<-DefineMapScheme(datatoplot=res.dat$cv,breakpoints=cvbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="cv.png",colcode=colorforyield)

return

}

MapYieldandCVDifferenceCoppiceVersusForest<-function(first.year=first.year,last.year=last.year,ResultFolder1=ResultFolder1,ResultFolder2=ResultFolder2,shapefilelocation=shapefilelocation,mapfolder=mapfolder,cvbreakpoints=NULL,yieldbreakpoints=NULL,cvmax=0.5){
resUS1<-ReadBioCroOutput(ResultFolder1,first.year,last.year)
colnames(resUS1) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem", "avgTemp","ToalPrecip","avgRad","awc","mdep","year")
resUS1$avgstem=resUS1$Stem5

resUS2<-ReadBioCroOutput(ResultFolder2,first.year,last.year)
colnames(resUS2) <- c("LOC","Lat","Lon",
                     "Stem1", "Leaf1", "Root1","Rhiz1",
                      "Stem2", "Leaf2", "Root2","Rhiz2",
                      "Stem3", "Leaf3", "Root3","Rhiz3",
                      "Stem4", "Leaf4", "Root4","Rhiz4",
                      "Stem5", "Leaf5", "Root5","Rhiz5",
                       "avgstem", "avgTemp","ToalPrecip","avgRad","awc","mdep","year")
resUS2$avgstem=resUS2$Stem1+resUS2$Stem2+resUS2$Stem3+resUS2$Stem4+resUS2$Stem5
resUS2$avgstem=resUS2$avgstem ## to convert average yearly yield to four year cycle
resUS1$avgstem=(resUS1$avgstem-resUS2$avgstem)
res.dat<-EvaluateMeanAndCV(resUS1,"avgstem")


write.csv(res.dat,file=paste(mapfolder,"willowsummary.csv"), row.names=FALSE)
#Plotting Mean Yields
colorforyield<-DefineMapScheme(datatoplot=res.dat$harvDB,breakpoints=yieldbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="yield.png",colcode=colorforyield)

# Plotting CV of Yields
if(!is.null(cvmax))res.dat[res.dat$cv>cvmax,]$cv=cvmax
colorforyield<-DefineMapScheme(datatoplot=res.dat$cv,breakpoints=cvbreakpoints)
plotcolorpoints(shapefilelocation,res.dat,mapfolder,outfile="cv.png",colcode=colorforyield)

return


}

