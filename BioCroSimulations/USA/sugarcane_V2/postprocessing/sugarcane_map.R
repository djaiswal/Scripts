source("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing/MapYieldandCVwillow.R")

ResultFolder<-"/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing"
mapfolder<-"/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing/"
shapefilelocation<-"/home/a-m/djaiswal/mapping/states_21basic/states.shp"
last.year<-1979
first.year<-2010
for (year in first.year:last.year){
for (simulationID in 1:134){
  if(simulationID==1){
  output <- read.table(file=paste("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/Results/res_",simulationID,"_",year,".txt",sep=""))
  } else{
  tmp <- read.table(file=paste("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/Results/res_",simulationID,"_",year,".txt",sep=""))
  output <- rbind(output,tmp)
  }
}
write.table(output,file=paste("/home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing/res",year,".txt",sep=""))
}


MapYieldandCV(first.year=first.year,last.year=last.year,ResultFolder=ResultFolder,shapefilelocation=shapefilelocation,mapfolder=mapfolder,cvbreakpoints=NULL,yieldbreakpoints=NULL,cvmax=0.5)

