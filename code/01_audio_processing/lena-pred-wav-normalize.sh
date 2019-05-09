#!/usr/bin/env bash

# take user arguments (passed by position) for the dataset --------------------------------------
# and whether pilot data should be used ---------------------------------------------------------
dataset=$1
is_pilot=$2
echo 'using' $dataset 'files'

# create the appropriate paths based on whether --------------------------------------
# user wants to use pilot segements or full dataset --------------------------------------
if [ "$is_pilot" = "pilot" ]; then
  echo 'normalizing pilot audio files'
  raw_data_path=../../data/01_raw_data/pilot-segments
  output_dir=../../../../02_processed_data/pilot-segments-norm/$dataset/$speech_type
  cd $raw_data_path/$dataset/$speech_type
  ffmpeg-normalize *.wav -ar 16000 -of $output_dir -ext wav
else 
  echo 'normalizing all audio files'
  raw_data_path=../../data/01_raw_data/$dataset
  cd $raw_data_path
fi

# loop over different speech types --------------------------------------
for speech_type in */
do
  cd $speech_type
  # loop over directories and normalize audio files in each dir --------------------------
  for label_type in */
  do
    output_dir=../../../../02_processed_data/$dataset-norm/$speech_type/$label_type
    cd "$label_type"
    ffmpeg-normalize *.wav -ar 16000 -of "$output_dir" -ext wav
    cd ..
  done
  cd ..
done






