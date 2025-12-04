#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=1000G
#SBATCH --job-name="train_algo_species"
#SBATCH --mail-user=tardif.sarah@courrier.uqam.ca
#SBATCH --mail-type=END
module load r/4.4.0
Rscript Etape3_RF.R
