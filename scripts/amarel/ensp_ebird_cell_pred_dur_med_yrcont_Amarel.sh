#!/bin/bash

#SBATCH --job-name=ensp_cell_predict ## Replace with your jobname 
#SBATCH --partition=p_deenr_1 # Partition (job queue)
#SBATCH --requeue
#SBATCH -N 1 # Number of nodes 
#SBATCH -n 1 # Numer of tasks
#SBATCH -c 10 # Number of cores per task
#SBATCH --mem=250000 # Real memory (RAM) per node required (MB) 
#SBATCH --time=96:00:00 # Total run time limit (HH:MM:SS) 
#SBATCH -o %N-%j.out
#SBATCH -e %N-%j.err


# This clears the decks of any leftovers 
module purge
# this activates the environment in which we installed R and our packages
source activate ren400

# You dont have to change anything from here to srun. Its just specifying which information
# will come out in aaa.out 
### Pre-run preparation
# Print the name of the node on which this script has started and the time.
echo "====================================================================="
echo "Starting on `hostname` at `date`"
STARTDATE=`date`
HN=`hostname`
echo "HERE WILL BE NODELIST $SLURM_JOB_NODELIST" >> aaa.out
echo "Current JOB_ID is $SLURM_JOB_ID"   >> aaa.out

# Display this jobs working directory
echo Working directory is $SLURM_SUBMIT_DIR  >> aaa.out

# This line is really important! It tells Amarel that you are running an R script, and it links this
# job file to the script that you want to run. Make sure that this file path matches the path to the
# script in your home directory. 
srun --mpi=pmi2 Rscript /scratch/mcallen/blra_pred_ycont/ensp_ebird_cell_pred_dur_med_yrcont_Amarel.R IFN:/scratch/mcallen/blra_pred_ycont/ensp_ebird_cell_pred_dur_med_yrcont.csv
    
    
