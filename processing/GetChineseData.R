# Chinese data from http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0016505

# AOA_r is rated by adults
# AOA_o is actual child naming

setwd("~/Documents/MPI/MonaghanAoA/Stats 2/analysis/")

chinese = read.delim("../data/Mandarin_YouyiLiu_WithPinyin.tab", encoding = 'utf-8',fileEncoding = 'utf-8',stringsAsFactors = F)

chinese$Pinyin = gsub(" ",'',chinese$Pinyin)

chinese.loan = read.csv("../data/wold-dataset.cldf/wold-vocabulary-22.csv", stringsAsFactors = F)


chinese.loan$word = gsub(" *\\(.*?\\) *","",chinese.loan$Value)

chinese.loan$age.oldest = 
  sapply(chinese.loan$age, function(X){
    x = strsplit(X,'-')[[1]][1]
    if(is.na(as.numeric(x))){
      return(X)
    } else{
      return(as.numeric(x))
    }
  })

#chinese.loan$age.oldest.num = 2000 - chinese.loan$age.oldest

chinese.loan$age.youngest = as.numeric(
  sapply(chinese.loan$age, function(X){
    x = strsplit(X,'-')[[1]]
    x[length(x)]
  })
)

#chinese.loan$age.youngest.num = 2000 - chinese.loan$age.youngest

matchx =  match(chinese$Pinyin, chinese.loan$word)
for(v in c("Borrowed",'age','age.youngest','age.oldest','age.youngest.num','age.oldest.num')){
  chinese[,v] = chinese.loan[matchx,v]
}

write.csv(chinese, "../data/loanwords_Chinese.csv")

#s(phonlengthscale) + s(AoAscale) +   s(subtlexzipfscale) +  s(concscale) 

chinese$phonlengthscale = scale(chinese$LEN)
chinese$AoAscale = scale(chinese$AoA_o)
chinese$subtlexzipfscale = scale(chinese$FREQ)

# Note: only 1 "clearly borrowed word with ratings!