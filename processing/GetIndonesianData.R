setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/")
library(openxlsx)
d = read.xlsx("../data/otherLangs/Indonesian/Sianipar_2016_data sheet 1.xlsx")
d$`Words.(Indonesian)` = gsub(" $","",d$`Words.(Indonesian)`)
d$`Words.(Indonesian)` = gsub("^ ","",d$`Words.(Indonesian)`)

i.loan = read.csv("../data/wold-dataset.cldf/wold-vocabulary-27.csv", stringsAsFactors = F, encoding = 'utf-8')

write.csv(i.loan$original_script,
    file="../data/otherLangs/Indonesian/WOLD_indonesian_script.txt",
    fileEncoding = 'utf-8',
    row.names = F, quote = F)

i.loan$word = gsub(" *\\(.*?\\) *","",i.loan$Value)


i.loan$age.oldest = NA
i.loan$age.oldest.num = NA
i.loan$age.youngest = NA
i.loan$age.youngest.num = NA

matchx =  match(d$`Words.(Indonesian)`, i.loan$word)

for(v in c("Borrowed",'age','age.youngest','age.oldest','age.youngest.num','age.oldest.num','Parameter_name')){
  d[,v] = i.loan[matchx,v]
}
sum(!is.na(d$Borrowed))

d$bor15 = NA
d[!is.na(d$Borrowed) & d$Borrowed=="1. clearly borrowed",]$bor15 = 1
d[!is.na(d$Borrowed) & d$Borrowed=="2. probably borrowed",]$bor15 = 1
d[!is.na(d$Borrowed) & d$Borrowed=="4. very little evidence for borrowing",]$bor15 = 0
d[!is.na(d$Borrowed) & d$Borrowed=="5. no evidence for borrowing",]$bor15 = 0

sum(!is.na(d$bor15))

d$bor15.cat= factor(d$bor15)

d$cat = NA
d[grepl("^the ",d$Parameter_name),]$cat = "Noun"
d[grepl("^to ",d$Parameter_name),]$cat = "Verb"
d[!is.na(d$Parameter_name) & !d$cat %in% c("Noun","Verb"),]$cat = "Adj"

indonesian = d[!is.na(d$bor15),]
for(x in c("ALL_Valence_Mean","ALL_Frequency_Mean","ALL_Concreteness_Mean","ALL_Dominance_Mean","ALL_Arousal_Mean","ALLPredictability_Mean")){
  indonesian[,paste0(x,".scaled")] = scale(indonesian[,x])
}

write.csv(indonesian, file="../data/loanwords_Indonesian.csv")


save(indonesian, file="../data/loanwords_Indonesian.Rdat")