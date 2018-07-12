setwd("~/Documents/MPI/MonaghanAoA/Stats 2/analysis/")


dutch = read.csv("../data/WordData_Dutch2.csv", stringsAsFactors = F)

dutch.loan = read.csv("../data/wold-dataset.cldf/wold-vocabulary-12.csv", stringsAsFactors = F)

# remove brackets
dutch.loan$word = gsub(" *\\(.*?\\) *","",dutch.loan$Value)

dutch.loan$age.oldest = 
  sapply(dutch.loan$age, function(X){
  x = strsplit(X,'-')[[1]][1]
  if(is.na(as.numeric(x))){
    return(X)
  } else{
    return(as.numeric(x))
  }
  })

dutch.loan$age.oldest.num = 2000 - dutch.loan$age.oldest

dutch.loan$age.youngest = as.numeric(
  sapply(dutch.loan$age, function(X){
    x = strsplit(X,'-')[[1]]
    x[length(x)]
  })
)
dutch.loan$age.youngest.num = 2000 - dutch.loan$age.youngest

matchx =  match(dutch$spelling, dutch.loan$Value)

for(v in c("Borrowed",'age','age.youngest','age.oldest','age.youngest.num','age.oldest.num')){
  dutch[,v] = dutch.loan[matchx,v]
}

dutch$aoa = as.numeric(dutch$aoa)

write.csv(dutch,"../data/loanwords_Dutch.csv", row.names = F)

dutch = read.csv("../data/loanwords_Dutch.csv", stringsAsFactors = F)

# Make Rdat dataframe:

dutch$bor15 = ifelse(dutch$Borrowed=="1. clearly borrowed",1, ifelse(dutch$Borrowed=="5. no evidence for borrowing",0,NA))

dutch$cat = factor(dutch$subtlex.dominant.pos)

dutch = dutch[complete.cases(dutch[,c("bor15","length",'aoa','subtlex.dominant.pos.frequency','concreteness','cat')]),]

dutch$AoAscale <- scale(as.numeric(dutch$aoa))

dutch$subtlexzipfscale <- log10(as.numeric(dutch$subtlex.dominant.pos.frequency))
dutch$subtlexzipfscale = scale(dutch$subtlexzipfscale)

dutch$phonlengthscale <- scale(as.numeric(dutch$length))

dutch$concscale <- scale(as.numeric(dutch$concreteness))

dutch$bor15.cat = factor(dutch$bor15)

dutch$cat = relevel(dutch$cat,"N")

write.csv(dutch[,
                c('spelling',"Borrowed","age","age.youngest",'age.oldest',"bor15","length",'aoa',
                  'subtlex.dominant.pos.frequency','concreteness','cat',
                   'AoAscale','subtlexzipfscale','phonlengthscale','concscale',
                  'bor15.cat')],
          "../../Writeup/SI/loanwords_Dutch_Raw.csv", row.names = F, fileEncoding = 'utf-8')

save(dutch,file="../data/loanwords_Dutch.Rdat")

