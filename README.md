# Scripts for  
## processing climate data.  
## running regional simulations.  
### US Simulations.  
  
  
climate NARR 1979-2015
Soil Data: SSURGO
Folder:https://github.com/djaiswal/Scripts/tree/master/BioCroSimulations/USA/sugarcane_V2  
R script:https://github.com/djaiswal/Scripts/blob/master/BioCroSimulations/USA/sugarcane_V2/Simsugarcane.r  
Bash Script:https://github.com/djaiswal/Scripts/blob/master/BioCroSimulations/USA/sugarcane_V2/SimALL.sh  
  
  
#### Postprocessing
Folder: https://github.com/djaiswal/Scripts/tree/master/BioCroSimulations/USA/sugarcane_V2/postprocessing  
bash script to summarize results:https://github.com/djaiswal/Scripts/blob/master/BioCroSimulations/USA/sugarcane_V2/postprocessing/Sim_map.sh  
R script to summarize and combine the results: https://github.com/djaiswal/Scripts/blob/master/BioCroSimulations/USA/sugarcane_V2/postprocessing/sugarcane_map.R  
to Map the results: https://github.com/djaiswal/Scripts/blob/master/BioCroSimulations/USA/sugarcane_V2/postprocessing/MapYieldandCVwillow.R 
  
#### To run new Simulation  
1. Change directory and file name in SimALL.sh for rscript and rout (2)  
2. change setwd in Simsugarcane.r (1)  
3. Change result folder in Simsugarcane.r (1)  

#### To postprocess new results  
1.  Change directory and file name in Sim_map.sh (2)  
2.  change Source,Resultfolder, mapfolder, output, tmp, and write.table (6)  





