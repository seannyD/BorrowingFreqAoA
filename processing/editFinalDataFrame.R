# Merge final data with Monaghan's Old English data
setwd("~/Documents/MPI/MonaghanAoA/Stats 2/data/")

d9 = read.csv("loanword9.csv",stringsAsFactors = F)
d10 = read.csv("loanword10.csv",stringsAsFactors = F)

#d9 = d9[!is.na(d9$AoA),]
#d10 = d10[!is.na(d10$AoA),]

sum(!d9$word %in% d10$word)
sum(!d10$word %in% d9$word)

sapply(1:nrow(d9), function(i){
  all(d9[,i]==d10[,i])
})

setdiff(names(d10),names(d9))


idx9 = paste(d9$word,d9$cat,d9$phonology)
idx10 = paste(d10$word,d10$cat,d10$phonology)

matchx = match(idx9,idx10)

for(var in c("age","source","effect","ids.code","modern.english","middle.english","old.english","old.english.length")){
  d9[,var] = d10[matchx,var]
}

d9 = d9[,c(
  "word","borrowing",
  "phonology","phonlength",
  "AoA","AoA_obj",
  "subtlexzipf",
  "conc","cat",
  "source",
  "source.language","source.word",
  "source.language.mean.word.length",
  "source.language.word.freq",
  "source.word.length",
  "age_oldest",
  "age_youngest",
  "age_oldest_num",
  "age_youngest_num",
  "age",
  "effect","ids.code",
  "modern.english",
  "middle.english",
  "old.english","old.english.length"
)
]

## Add number of morphemes
#m = read.table("../data/Celex_EnglishMorphemes.tab",sep="\t",header=T,fileEncoding = "UTF-8",quote = "",comment.char="",stringsAsFactors = F)
#d9$numberOfMorphemes = m[
#  match(paste0(d9$word,substr(d9$cat,1,1)),
#        paste0(m$Head,m$Class)),]$CompCnt
  


write.csv(d9,"loanword11.csv",row.names = F)
