#!/usr/bin/env bash
# ffmpeg-normaqlize tool taken from here: https://github.com/slhck/ffmpeg-normalize

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

if [ "$dataset" = "ManyBabies" ]
then 
  for speech_type in */
  do
    cd $speech_type
    # loop over directories and normalize audio files in each dir --------------------------
    for label_type in */
    do
      output_dir=../../../../02_processed_data/$dataset-norm/$speech_type$label_type
      cd "$label_type"
      ffmpeg-normalize *.wav -ar 16000 -of "$output_dir" -ext wav -nt ebu
      cd ..
    done
    cd ..
  done
else 
  for speech_type in */
  do
    if [ "$speech_type" = "IDS/" ] || [ "$speech_type" = "ADS/" ]
    then 
      cd $speech_type
      echo "Normalizing files for " $speech_type "register" 
      output_dir=../../../02_processed_data/$dataset-norm/$speech_type
      ffmpeg-normalize *.wav -ar 16000 -of "$output_dir" -ext wav -nt rms -t -18 -f
      cd ..
    else 
      echo "This is not a speech register dir, skipping"
    fi
  done
fi



