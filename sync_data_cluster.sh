#!/bin/bash

# Syncs raw audio files to SCRATCH directory on cluster
# takes a UCLA cluster id as first argument

# create some useful vars
echo "Enter password to get SCRATCH directory location from cluster"
user=$1
cluster_name=@hoffman2.idre.ucla.edu
SCRATCH=$(ssh $user$cluster_name 'echo $SCRATCH')
target_dir_cluster=$user$cluster_name:$SCRATCH/lena-pred
project_dir=.

# upload code and data directories
echo "Enter pwd to sync local project with server"
rsync -r --exclude='.git/' --exclude='.Rproj.user/' --exclude='/presentations' --progress --stats $project_dir $target_dir_cluster
