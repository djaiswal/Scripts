#!/bin/bash
#PBS -S /bin/bash
#PBS -M deepakebimodelling@gmail.com
#PBS -m abe
#PBS -l mem=5GB

module load R/3.0.2

R CMD BATCH --no-save "--args $PBS_ARRAYID" /home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing/sugarcane_map.R /home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/postprocessing/sugarcane_map.Rout


