# bash script for downloading mp3 and wav audio files from
# homebank database

# -P specifies directory for saving files
# note that if the directory does not exist, it will get created

# wget will prompt for password, which changes every couple of months
# note that the username will also change every couple of months
username=gordon
dirpath=../../data/01_raw_data/

# download raw media files for one participant (this will take a long time)
media_url=https://media.talkbank.org/homebank/Password/Warlaumont/0204/
wget -P $dirpath -c --user=$username --ask-password -e robots=off -r -l inf --no-remove-listing -nH --no-parent -R 'index.html*' $media_url