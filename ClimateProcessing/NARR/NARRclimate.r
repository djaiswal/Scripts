## I'm trying ftionigure out ways of manipulating netCDF data

args <- commandArgs(TRUE)

if(length(args) == 0){
  warning("args is missing")
}else{
  k <- as.numeric(args)
}

year <- as.integer(k)

library(BioCro, lib.loc="/home/a-m/djaiswal/R/library")
library(ncdf4)





setwd("/home/groups/ebimodeling/met/old-narr")
## Loading radiation
shortR <- nc_open(paste("uUSshortR.",k,".nc",sep=""))## radiation will come form NLDAS
# shortR <- read.csv(paste("/home/groups/ebimodeling/met/NLDAS/nldas-proc-rad-",k,"-pp.csv",sep=""))

## Loading temperature
airT <- nc_open(paste("/home/groups/ebimodeling/met/old-narr/uUSairT.",k,".nc",sep=""))
## Loading relative humidity
rhum <- nc_open(paste("uUSrhum.",k,".nc",sep=""))
## Loading windspeed
winds <- nc_open(paste("uUSwind.",k,".nc",sep=""))
## Loading precip
precip <- nc_open(paste("uUSprecip.",k,".nc",sep=""))
## vegetation
veg <- nc_open(paste("uUSveg.",k,".nc",sep=""))
## Soil moisture
soilm <- nc_open(paste("uUSsoilm.",k,".nc",sep=""))


## Getting the variables
Rad <- ncvar_get(shortR,"dswrf")
Temp <- ncvar_get(airT,"air")
RH <- ncvar_get(rhum,"rhum")
windspeed <- ncvar_get(winds,"uwnd")
Precip <- ncvar_get(precip,"apcp")
Veg <- ncvar_get(veg,"veg")
Soilm <- ncvar_get(soilm,"soilm")

lat <- ncvar_get(airT,"lat")
lon <- ncvar_get(airT,"lon")
time <-ncvar_get(airT,"time")

day <- rep(1:365,each=8)
hr<-rep(c(0,3,6,9,12,15,18,21),365)
timet<-hr
setwd("/home/a-m/djaiswal/Scripts/ClimateProcessing/NARR")


for(i in 1:dim(lat)[1]){

  for(j in 1:dim(lat)[2]){

    lati <- lat[i,j]
    long <- lon[i,j]
    loc<-i*100 + j    
 

    # Fetching and unit conversion temperature
    # From Kelvin to Celsius
    sTemp <- Temp[i,j,] - 273.15
    # Fetcing and unit conversion relative humidity
    # From (%) to fraction
    sRH <- RH[i,j,] * 1e-2 - 0.001 
    # Fetcing and unit conversion wind speed
    # Just calculating absolute value
    sWindS <- abs(windspeed[i,j,])
    # Fetcing and unit conversion precipitation
    # The units of kg/m2 are approx. equivalent to mm
    sPrecip <- abs(Precip[i,j,])
    # Fetcing and unit conversion precipitation
    # The units of kg/m2 are approx. equivalent to mm
    sVeg <- Veg[i,j,]
    # Soil moisture
    # It represents the soil moisture of the first 2m
    # I can get what I need by dividing by 2000
    sSoilm <- Soilm[i,j,] / 2000

    ## Fetching and unit conversion radiation
     # From W/m^2 to MJ/m^2
     sRad <- Rad[i,j,] * 3*60*60 * 1e-6

    
    ## Fixing the missing value problem
    sRad <- ifelse(sRad > 32765 , 100,sRad)
    sTemp <- ifelse(sTemp > 32765 , 5,sTemp)
    sRH <- ifelse(sRH > 32765*1e-2 , 0.7,sRH)
    sWindS <- ifelse(sWindS > 32765 , 2,sWindS)
    sPrecip <- ifelse(sPrecip > 32765 , 0,sPrecip)
    sVeg <- ifelse(sVeg > 32765 , NA,sVeg)
    sSoilm <- ifelse(sSoilm > 30000,NA,sSoilm)

    ## Addressing the leap year problem
    if(length(sTemp) > 2920){
            sRad <- sRad[1:2920]
            sTemp <- sTemp[1:2920]
            sRH <- sRH[1:2920]
            sWindS <- sWindS[1:2920]
            sPrecip <- sPrecip[1:2920]
            hr <- hr[1:2920]
          }

      dat <- data.frame(year=year,doy=day,hour=hr,
                        Temp=sTemp,RH=sRH,WS=sWindS,precip=sPrecip,solarR=sRad)

      MaxTemp <- aggregate(dat$Temp,by=list(dat$doy),max)$x ## In Celsius
      MinTemp <- aggregate(dat$Temp,by=list(dat$doy),min)$x ## In Celsius
      MeanTemp <- aggregate(dat$Temp,by=list(dat$doy),mean)$x ## In Celsius

      MaxRH <- aggregate(dat$RH,by=list(dat$doy),max)$x  ## Fraction
      MinRH <- aggregate(dat$RH,by=list(dat$doy),min)$x  ## Fraction
      MeanRH <- aggregate(dat$RH,by=list(dat$doy),mean)$x ## Fraction

      MeanWS <- aggregate(dat$WS,by=list(dat$doy),mean)$x ## in m/s

      TotPrecip <- aggregate(dat$precip,by=list(dat$doy),sum)$x ## in mm

      TotRad <- aggregate(dat$solarR,by=list(dat$doy),sum)$x ## in MJ/m^2 

      dat <- weach(cbind(year,1:365,TotRad,MaxTemp,MinTemp,MeanTemp,
                         MaxRH,MinRH,MeanRH,MeanWS,TotPrecip),lati=lati,temp.units="Celsius",
                   rh.units="fraction",ws.units="mps",pp.units="mm")
      
      dat <- as.data.frame(dat)
 filename=paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/",k,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
 soilfilename= paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/soilm/",k,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
 save(dat,file=filename)
 save(sSoilm,file=soilfilename)

  
  }
}

nc_close(airT)
nc_close(rhum)
nc_close(winds)
nc_close(precip)
nc_close(soilm)


              

