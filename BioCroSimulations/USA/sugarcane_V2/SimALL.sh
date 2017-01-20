#!/bin/bash
#PBS -S /bin/bash
#PBS -M deepakebimodelling@gmail.com
#PBS -m abe
#PBS -t 1-134
#PBS -l mem=2GB
#PBS -l walltime=08:00:00
#PBS -N sugarcane_v2


module load R/3.2.0

R CMD BATCH --no-save "--args $PBS_ARRAYID" /home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/Simsugarcane.r /home/a-m/djaiswal/Scripts/BioCroSimulations/USA/sugarcane_V2/SimUS$PBS_ARRAYID.Rout


