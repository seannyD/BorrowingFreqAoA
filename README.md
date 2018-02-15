# Cognitive influences in language evolution


Predicting whether a word is borrowed from psycholinguistic measures.  The file structure is as follows:

## data

-  `loanword8.csv`:  Raw data for English.
-  `loanwords_Dutch.csv`: Raw data for Dutch.
-  `loanwords_Dutch.Rdat`: Processed loanwords data for Dutch (with scaling and centering).

... and various other files from other languages and studies.  The data from other languages is either much smaller or less reliable.

## analysis

`loanwords_gam.Rmd` Main R script for analysing the English data.

`loanwords_gam_Dutch.Rmd` Main R script for analysing the Dutch data.

`loanwords_pagel.Rmd` A replication of the findings in Pagel et al. (2007) and Monaghan (2014) relating rate of change with frequency, length and age of acquisition.

... and various other analyses such as predicting the date of entry, and attempt to run the main analyses in other languages (mostly not reliable due to lack of data).  

## processing

Various files to combine and process the raw data.

## results

Graphs and other outputs from the R scripts.