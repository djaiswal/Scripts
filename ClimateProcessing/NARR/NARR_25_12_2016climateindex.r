k<- 2010
year <- as.integer(k)

library(BioCro, lib.loc="/home/a-m/djaiswal/R/library")
library(ncdf4)
setwd("/home/groups/ebimodeling/met/NARR_25_12_2016")

## Loading temperature
airT <- nc_open(paste("air.sfc.",k,".nc",sep=""))

lat <- ncvar_get(airT,"lat")
lon <- ncvar_get(airT,"lon")


NARR_25_12_2016 <- data.frame(Latt=numeric(0),Lonn=numeric(0),Iindex=numeric(0),Jindex=numeric(0))

for(i in 1:dim(lat)[1]){
  for(j in 1:dim(lat)[2]){
    lati <- lat[i,j]
    long <- lon[i,j]
    tmp <- data.frame(Latt=lati,Lonn=long,Iindex=i,Jindex=j)
    NARR_25_12_2016 <- rbind(NARR_25_12_2016,tmp)
  }
}
write.table(NARR_25_12_2016,file="/home/groups/ebimodeling/met/NARR_25_12_2016/ProcessedNARR/NARR_25_12_2016index.txt")
nc_close(airT)


              

