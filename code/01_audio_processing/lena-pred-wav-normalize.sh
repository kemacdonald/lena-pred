#!/usr/bin/env bash

ffmpeg-normalize *.wav -ar 16000 -of ../../../02_processed_data/pilot-segments-norm/IDSLabel -ext wav
ffmpeg-normalize *.wav -ar 16000 -of ../../../02_processed_data/pilot-segments-norm/ManyBabies -ext wav
