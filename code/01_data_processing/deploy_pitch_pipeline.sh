#!/bin/bash

dataset=ManyBabies

sh ./01_lena-pred-wav-normalize.sh $dataset
Rscript 02_lena-pred-pitch-extraction.R
Rscript 03_lena-pred-poly-cluster.R
Rscript 04_lena-pred-dnn-dataset-gen.R