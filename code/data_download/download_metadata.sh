# download metadata
# this will also prompt the user for a password
username=gordon
metadata_file="Warlaumont.zip"
metadata_get_path="https://homebank.talkbank.org/data/Password/"$metadata_file
metadata_save_path="../../data/00_metadata/"
wget -P $metadata_save_path --user=$username --ask-password $metadata_get_path

# unzip metadata files
unzip -a $metadata_save_path$metadata_file -d $metadata_save_path

# remove the original zip file
rm $metadata_save_path$metadata_file
