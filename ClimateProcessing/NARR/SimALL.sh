#!/bin/bash
#PBS -S /bin/bash
#PBS -M deepakebimodelling@gmail.com
#PBS -m abe
#PBS -t 1979-2015
#PBS -l mem=3GB

module load R/3.2.0
module load netcdf

R CMD BATCH --no-save "--args $PBS_ARRAYID" /home/a-m/djaiswal/Scripts/ClimateProcessing/NARR/NARR_25_12_2016climate.r /home/a-m/djaiswal/Scripts/ClimateProcessing/NARR/NARR_25_12_2016$PBS_ARRAYID.Rout


