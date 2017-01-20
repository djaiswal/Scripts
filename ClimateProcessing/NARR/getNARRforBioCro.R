# To read Index for a given latitude and Longitude
getNARRforBioCro<-function(lat,lon,year,option="1979-2015"){
if(option =="1979-2015"){
USlayer<-read.table("/home/groups/ebimodeling/met/NARR_25_12_2016/ProcessedNARR/NARR_25_12_2016index.txt")
} else {
  if(option =="1979-2010"){
    USlayer<-read.table("/home/groups/ebimodeling/met/NARR/ProcessedNARR/NARRindex.txt")
  }else{
    stop("Error with option")
  }
}
index=which.min((lat-USlayer$Latt)^2+(lon-USlayer$Lonn)^2)
i=USlayer$Iindex[index]
j=USlayer$Jindex[index]
if(option =="1979-2015"){
  filename=paste("/home/groups/ebimodeling/met/NARR_25_12_2016/ProcessedNARR/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
} else {
  if(option =="1979-2010"){
    filename=paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
  }else{
    stop("Error with option")
  }
}
load(filename)
return(dat)
}

getNARRsoil<-function(lat,lon,year,option="1979-2015"){
  if(option =="1979-2015"){
    USlayer<-read.table("/home/groups/ebimodeling/met/NARR_25_12_2016/ProcessedNARR/NARR_25_12_2016index.txt")
  } else {
    if(option =="1979-2010"){
      USlayer<-read.table("/home/groups/ebimodeling/met/NARR/ProcessedNARR/NARRindex.txt")
    }else{
      stop("Error with option")
    }
  }
  index=which.min((lat-USlayer$Latt)^2+(lon-USlayer$Lonn)^2)
  i=USlayer$Iindex[index]
  j=USlayer$Jindex[index]
  if(option =="1979-2015"){
    filename=paste("/home/groups/ebimodeling/met/NARR_25_12_2016/ProcessedNARR/soilm/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
  } else {
    if(option =="1979-2010"){
      filename=paste("/home/groups/ebimodeling/met/NARR/ProcessedNARR/soilm/",year,formatC(i,width=3,flag=0),formatC(j,width=3,flag=0),".RData",sep="")
    }else{
      stop("Error with option")
    }
  }
  load(filename)
  return(sSoilm)
}




