library(openxlsx)
setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/")

d = read.csv("../data/loanword8.csv", stringsAsFactors = F)

newAoA = read.xlsx("../data/Brysbaert_Biemiller Master file with all values for test based AoA measures.xlsx")

newAoA$WORD = as.character(newAoA$WORD)
newAoA$WORD = gsub(" +$","", newAoA$WORD)
newAoA$WORD = gsub("^ +","", newAoA$WORD)

d$AoA_obj = newAoA[match(tolower(d$word), tolower(newAoA$WORD)),]$AoAtestbased

write.csv(d,file="../data/loanword8.csv", row.names = F)
