#!/usr/bin/env bash

# take user arguments (passed by position) for the dataset and speech register
dataset=$1
speech_type=$2
echo 'processing' $dataset $speech_type 'files'

# create the appropriate paths
raw_data_path=../../data/01_raw_data/pilot-segments
output_dir=../../../../02_processed_data/pilot-segments-norm/$dataset/$speech_type

# move into the raw data directory and normalize all the .wav files
cd $raw_data_path/$dataset/$speech_type
ffmpeg-normalize *.wav -ar 16000 -of $output_dir -ext wav
