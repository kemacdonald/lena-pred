#!/usr/bin/env bash

# This script extracts the .wav files for each segement in the IDSLabel 
# dataset. The raw data comes as directories of wav files for each speaker
# and conversational block as coded by LENA
# We extract those individual .wav files and give it an informative/unique id

# TODO: fix bug where some files are not being named between 70-80 (not quite sure what's going on)

read -p "Press [Enter] key to start extracting all .wav files from zips"

cd ../../data/01_raw_data/IDSLabel/nested_dirs

# loop over speaker ids ------------------------------------

# for speaker_id in */
# do
#   echo $speaker_id
#   cd $speaker_id
#   # loop over zip files and uncompress ------------------------------------
#   for zip_file in *.zip
#   do 
#     conv_block=${zip_file%".zip"}
#     unzip $zip_file -d $conv_block
#     #mv $zip_file zip_files_dir/
#     
#     # make informative file names
#     cd $conv_block/
#     
#     # loop over individual .wav files. rename and move to top-level dir  ------------------------------------
#     for seg_id in *.wav
#     do
#       new_filename="$(basename $speaker_id)_$(basename $conv_block)_$(basename $seg_id)"
#       mv $seg_id $new_filename
#     done
#     
#     cd ..
#     # move all the files to unnested dir
#     find . -name '*.wav' -exec mv {} ../../unnested_files/ \;
#     # cleanup tmp directory created when we unzipped conv block
#     rm -r $conv_block/
#   done
#   cd ..
# done


# create dir with only files with speech using a whitelist created by 00_lena-pred-IDSlabel-meta-process.R

read -p "Press [Enter] key to start rsync of ADS whitelist"
cd ../

ads_whitelist=lena-pred-idslabel-whitelist-ads.txt
ads_path_to_whitelist=../../03_summaries/IDSLabel/$ads_whitelist
rsync -a --files-from=$ads_path_to_whitelist unnested_files/ ADS/

read -p "Press [Enter] key to start rsync of IDS whitelist"

ids_whitelist=lena-pred-idslabel-whitelist-ids.txt
ids_path_to_whitelist=../../03_summaries/IDSLabel/$ids_whitelist
rsync -a --files-from=$ids_path_to_whitelist unnested_files/ IDS/