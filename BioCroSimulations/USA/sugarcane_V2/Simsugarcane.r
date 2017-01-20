## I'm trying ftionigure out ways of manipulating netCDF data

args <- commandArgs(TRUE)

if(length(args) == 0){
  warning("args is missing")
}else{
  k <- as.numeric(args)
}
simulationID <- k
iindexmax <- 139
jindexmax <- 96
timestart <- Sys.time()
library(BioCro, lib.loc="/home/a-m/djaiswal/R/library")
library(BioCoRegional)
source("/home/a-m/djaiswal/Scripts/ClimateProcessing/NARR/getNARRforBioCro.R")


coefi <- c(250,350,900,1200,3939,7000)
coefiN <- coefi/7000

setwd("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane")
uslay <- read.table("USlayer.txt",header=TRUE)

## Loading the soil data
ussoil <- read.table("./soil/ussoil-comp-pp.txt", header=TRUE)
names(ussoil) <- c("mukey","cokey","Loc","Lat","Lon",
                   "dist","centX","centY","cont","awc",
                   "mdep","hrb","elev","corn_yield","corn_yield_high","comppct")



setwd("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/")

NARRindex <- read.table("/home/a-m/djaiswal/Scripts/ClimateProcessing/NARR/NARRindex.txt")
N <- dim(NARRindex)[1]
startindex <- (simulationID-1)*100+1
endindex <- simulationID*100
if(endindex>13344){
  endindex<-13344
}
for(year in 1979:2010)
{
for(index in startindex:endindex)
{
    lati <- NARRindex$Latt[index]
    long <- NARRindex$Lonn[index]
    i<- NARRindex$Iindex[index]
    j<-NARRindex$Jindex[index]
    loc<-i*100 + j    
    csoil <- ussoil[ussoil$Loc == loc,]

    if(dim(csoil)[1]==0){
      resS <- data.frame(loc=loc,Lat=lati,Lon=long,
                         Stem1=NA, Leaf1=NA,Root1=NA,Rhiz1=NA,
                         Stem2=NA, Leaf2=NA,Root2=NA,Rhiz2=NA,
                         Stem3=NA, Leaf3=NA,Root3=NA,Rhiz3=NA,
                         Stem4=NA, Leaf4=NA,Root4=NA,Rhiz4=NA,
                         Stem5=NA, Leaf5=NA, Root5=NA, Rhiz5=NA,
                         avgStem=NA,year = NA)
    }else{
      dat <- getNARRforBioCro(lati,long,year)
   
       day1=15 # jan 15
       dayn=288# Oct15
       indes1 <- (day1 - 1) * 24
       indesn <- dayn * 24
       AirTemp <- dat[indes1:indesn, 5]
      mTT <- sum(AirTemp/24,na.rm=TRUE)
      seneL <- mTT * 0.6
      seneS <- mTT * 0.8
      cfs <- coefiN * 7000

      ## Growing season 1
      Stem <- numeric(nrow(csoil))
      Leaf <- numeric(nrow(csoil))
      Rhizome <- numeric(nrow(csoil))
      Root <- numeric(nrow(csoil))
      qfinal <- max(1, nrow(csoil))
      for(q in 1:qfinal){
        if(nrow(csoil) == 0){
          fieldc <- 0.15 + 0.17
          wiltp <- 0.15
          mdep <- 1.5
          elev <- NA
          hrb <- NA
          cornyd <- NA
          cornyd_h <- NA
        }else{
          awc <- csoil$awc[q]
          if(is.na(awc)) awc <- 0.174
          if(awc > 0.35) awc <- 0.35
          mdep <- csoil$mdep[q]/100
          if(is.na(mdep)) mdep <- 1.5
          if(mdep > 2.5) mdep <- 2.5
          if(mdep < 1e-2) mdep <- 1.5
          wiltp <- 0.15
          if(awc < 1e-2) awc <- 0.01
          fieldc <- wiltp + awc
        }
        sSoilm <- getNARRsoil(lati,long,year)
        soil.ll <- soilParms(iWatCont=sSoilm[day1],
                               FieldC=fieldc,WiltP=wiltp,
                               soilDepth=mdep, wsFun="linear", phi2=2, soilLayers=1)
       iRhizome <- 6
       res1 <- caneGro(dat,lat=lati,day1=day1,dayn=dayn,soilControl=soil.ll,photoControl=list(Catm=380),iRhizome=iRhizome,irtl=1e-2,frostControl = list(leafT100=-2))
       Leaf[q]<-res1$Leaf[length(res1$Leaf)]
       Stem[q]<-res1$Stem[length(res1$Stem)]
       Root[q]<-res1$Root[length(res1$Root)]
       Rhizome[q]<-res1$Seedcane[length(res1$Seedcane)]
      }
      weights <- csoil$comppct/100
      stem1<-sum(Stem*weights,na.rm=TRUE)
      stem1 <- stem1*(1-0.11)
      leaf1<-sum(Leaf*weights,na.rm=TRUE)
      root1<-sum(Root*weights,na.rm=TRUE)
      rhiz1<-sum(Rhizome*weights,na.rm=TRUE)
      
      ## Growing season 2
      Stem <- numeric(nrow(csoil))
      Leaf <- numeric(nrow(csoil))
      Rhizome <- numeric(nrow(csoil))
      Root <- numeric(nrow(csoil))
      qfinal <- max(1, nrow(csoil))
      for(q in 1:qfinal){
        if(nrow(csoil) == 0){
          fieldc <- 0.15 + 0.17
          wiltp <- 0.15
          mdep <- 1.5
          elev <- NA
          hrb <- NA
          cornyd <- NA
          cornyd_h <- NA
        }else{
          awc <- csoil$awc[q]
          if(is.na(awc)) awc <- 0.174
          if(awc > 0.35) awc <- 0.35
          mdep <- csoil$mdep[q]/100
          if(is.na(mdep)) mdep <- 1.5
          if(mdep > 2.5) mdep <- 2.5
          if(mdep < 1e-2) mdep <- 1.5
          wiltp <- 0.15
          if(awc < 1e-2) awc <- 0.01
          fieldc <- wiltp + awc
        }
        
        soil.ll <- soilParms(iWatCont=sSoilm[day1],
                             FieldC=fieldc,WiltP=wiltp,
                             soilDepth=mdep, wsFun="linear", phi2=2, soilLayers=1)
        
        iRhizome <- (stem1/(1.0-0.11))*0.11 #11% of the stem1 before multiplying 1-0.11
        res2 <- caneGro(dat,lat=lati,day1=day1,dayn=dayn,soilControl=soil.ll,photoControl=list(Catm=380),iRhizome=iRhizome,irtl=1e-2,frostControl = list(leafT100=-2))
        Leaf[q]<-res2$Leaf[length(res2$Leaf)]
        Stem[q]<-res2$Stem[length(res2$Stem)]
        Root[q]<-res2$Root[length(res2$Root)]
        Rhizome[q]<-res2$Seedcane[length(res2$Seedcane)]
        
      }
      weights <- csoil$comppct/100
      stem2<-sum(Stem*weights,na.rm=TRUE)
      stem2 <- stem2*(1-0.11)
      leaf2<-sum(Leaf*weights,na.rm=TRUE)
      root2<-sum(Root*weights,na.rm=TRUE)
      rhiz2<-sum(Rhizome*weights,na.rm=TRUE)
      
      

       ## Growing season 3
      Stem <- numeric(nrow(csoil))
      Leaf <- numeric(nrow(csoil))
      Rhizome <- numeric(nrow(csoil))
      Root <- numeric(nrow(csoil))
      qfinal <- max(1, nrow(csoil))
      for(q in 1:qfinal){
        if(nrow(csoil) == 0){
          fieldc <- 0.15 + 0.17
          wiltp <- 0.15
          mdep <- 1.5
          elev <- NA
          hrb <- NA
          cornyd <- NA
          cornyd_h <- NA
        }else{
          awc <- csoil$awc[q]
          if(is.na(awc)) awc <- 0.174
          if(awc > 0.35) awc <- 0.35
          mdep <- csoil$mdep[q]/100
          if(is.na(mdep)) mdep <- 1.5
          if(mdep > 2.5) mdep <- 2.5
          if(mdep < 1e-2) mdep <- 1.5
          wiltp <- 0.15
          if(awc < 1e-2) awc <- 0.01
          fieldc <- wiltp + awc
        }

        soil.ll <- soilParms(iWatCont=sSoilm[day1],
                               FieldC=fieldc,WiltP=wiltp,
                               soilDepth=mdep, wsFun="linear", phi2=2, soilLayers=1)
       
        iRhizome <- (stem2/(1.0-0.11))*0.11 #11% of the stem1 before multiplying 1-0.11
        res3 <- caneGro(dat,lat=lati,day1=day1,dayn=dayn,soilControl=soil.ll,photoControl=list(Catm=380),iRhizome=iRhizome,irtl=1e-2,frostControl = list(leafT100=-2))
        Leaf[q]<-res3$Leaf[length(res3$Leaf)]
        Stem[q]<-res3$Stem[length(res3$Stem)]
        Root[q]<-res3$Root[length(res3$Root)]
        Rhizome[q]<-res1$Seedcane[length(res3$Seedcane)]

      }
      weights <- csoil$comppct/100
      stem3<-sum(Stem*weights,na.rm=TRUE)
      stem3 <- stem3*(1-0.11)
      leaf3<-sum(Leaf*weights,na.rm=TRUE)
      root3<-sum(Root*weights,na.rm=TRUE)
      rhiz3<-sum(Rhizome*weights,na.rm=TRUE)

       ## Growing season 4
      Stem <- numeric(nrow(csoil))
      Leaf <- numeric(nrow(csoil))
      Rhizome <- numeric(nrow(csoil))
      Root <- numeric(nrow(csoil))
      qfinal <- max(1, nrow(csoil))
      for(q in 1:qfinal){
        if(nrow(csoil) == 0){
          fieldc <- 0.15 + 0.17
          wiltp <- 0.15
          mdep <- 1.5
          elev <- NA
          hrb <- NA
          cornyd <- NA
          cornyd_h <- NA
        }else{
          awc <- csoil$awc[q]
          if(is.na(awc)) awc <- 0.174
          if(awc > 0.35) awc <- 0.35
          mdep <- csoil$mdep[q]/100
          if(is.na(mdep)) mdep <- 1.5
          if(mdep > 2.5) mdep <- 2.5
          if(mdep < 1e-2) mdep <- 1.5
          wiltp <- 0.15
          if(awc < 1e-2) awc <- 0.01
          fieldc <- wiltp + awc
        }

        soil.ll <- soilParms(iWatCont=sSoilm[day1],
                               FieldC=fieldc,WiltP=wiltp,
                               soilDepth=mdep, wsFun="linear", phi2=2, soilLayers=1)
        iRhizome <- (stem3/(1.0-0.11))*0.11 #11% of the stem1 before multiplying 1-0.11
        res4 <- caneGro(dat,lat=lati,day1=day1,dayn=dayn,soilControl=soil.ll,photoControl=list(Catm=380),iRhizome=iRhizome,irtl=1e-2,frostControl = list(leafT100=-2))
        Leaf[q]<-res4$Leaf[length(res4$Leaf)]
        Stem[q]<-res4$Stem[length(res4$Stem)]
        Root[q]<-res4$Root[length(res4$Root)]
        Rhizome[q]<-res1$Seedcane[length(res4$Seedcane)]

      }
      weights <- csoil$comppct/100
      stem4<-sum(Stem*weights,na.rm=TRUE)
      stem4 <- stem4*(1-0.11)
      leaf4<-sum(Leaf*weights,na.rm=TRUE)
      root4<-sum(Root*weights,na.rm=TRUE)
      rhiz4<-sum(Rhizome*weights,na.rm=TRUE)

        ## Growing season 5
      Stem <- numeric(nrow(csoil))
      Leaf <- numeric(nrow(csoil))
      Rhizome <- numeric(nrow(csoil))
      Root <- numeric(nrow(csoil))
      qfinal <- max(1, nrow(csoil))
      for(q in 1:qfinal){
        if(nrow(csoil) == 0){
          fieldc <- 0.15 + 0.17
          wiltp <- 0.15
          mdep <- 1.5
          elev <- NA
          hrb <- NA
          cornyd <- NA
          cornyd_h <- NA
        }else{
          awc <- csoil$awc[q]
          if(is.na(awc)) awc <- 0.174
          if(awc > 0.35) awc <- 0.35
          mdep <- csoil$mdep[q]/100
          if(is.na(mdep)) mdep <- 1.5
          if(mdep > 2.5) mdep <- 2.5
          if(mdep < 1e-2) mdep <- 1.5
          wiltp <- 0.15
          if(awc < 1e-2) awc <- 0.01
          fieldc <- wiltp + awc
        }

        soil.ll <- soilParms(iWatCont=sSoilm[day1],
                               FieldC=fieldc,WiltP=wiltp,
                               soilDepth=mdep, wsFun="linear", phi2=5, soilLayers=1)
     
        iRhizome <- (stem4/(1.0-0.11))*0.11 #11% of the stem1 before multiplying 1-0.11
        res5 <- caneGro(dat,lat=lati,day1=day1,dayn=dayn,soilControl=soil.ll,photoControl=list(Catm=380),iRhizome=iRhizome,irtl=1e-2,frostControl = list(leafT100=-2))
        Leaf[q]<-res5$Leaf[length(res5$Leaf)]
        Stem[q]<-res5$Stem[length(res5$Stem)]
        Root[q]<-res5$Root[length(res5$Root)]
        Rhizome[q]<-res1$Seedcane[length(res5$Seedcane)]
      }
      weights <- csoil$comppct/100
      stem5<-sum(Stem*weights,na.rm=TRUE)
      stem5 <- stem5*(1-0.11)
      leaf5<-sum(Leaf*weights,na.rm=TRUE)
      root5<-sum(Root*weights,na.rm=TRUE)
      rhiz5<-sum(Rhizome*weights,na.rm=TRUE)

      ratoonfactor <- c(1,1.2,0.8,0.6,0.6)
      avgStem<-(stem1*ratoonfactor[1]+stem2*ratoonfactor[2]+stem3*ratoonfactor[3]+stem4*ratoonfactor[4]+stem5*ratoonfactor[5])*(1/5)  # Average Yield
          
      # Effect of ratoon cycling
      
      
      
      resS <- data.frame(loc=loc,Lat=lati,Lon=long,
                         Stem1=stem1, Leaf1=leaf1,Root1=root1,Rhiz1=rhiz1,
                         Stem2=stem2, Leaf2=leaf2,Root2=root2,Rhiz2=rhiz2,
                         Stem3=stem3, Leaf3=leaf3,Root3=root3,Rhiz3=rhiz3,
                         Stem4=stem4, Leaf4=leaf4,Root4=root4,Rhiz4=rhiz4,
                         Stem5=stem5, Leaf5=leaf5, Root5=root5, Rhiz5=rhiz5,
                         avgStem=avgStem,
                         year = year)
      
    }

    write.table(resS,paste("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/Results/res_",simulationID,"_",year,".txt",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE)
} #NARRindex end
} #year end

timeend <- Sys.time()

timestart
timeend
              

