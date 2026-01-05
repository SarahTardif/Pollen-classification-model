# script for executing model training on high-capacity server 

#!/bin/bash
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=400G
#SBATCH --job-name="train_algo_species"
module load r/4.4.0
Rscript Step3_RF.R
