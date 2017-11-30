# data from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4030127/
setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/")

jap = read.delim("../data/Japanses_Allen_Conklin.txt", sep='\t', stringsAsFactors = F, encoding = "utf-8", fileEncoding = 'utf-8')

japB = read.delim("../data/Japanses_Allen_ConklinB.txt", sep='\t', stringsAsFactors = F, encoding = "utf-8", fileEncoding = 'utf-8')

jap$subtlexzipf = as.numeric(japB[match(jap$Japanese.name, japB$Japanese.name),]$logJP)

jap.loan = read.csv("../data/wold-dataset.cldf/wold-vocabulary-21.csv")

jap.loan$meaning = gsub("the ","",jap.loan$Parameter_name)
jap.loan$meaning = gsub("to ","",jap.loan$meaning)

# match by transcription
matchx = match(jap$Transcription, jap.loan$Value)
sum(!is.na(matchx))
# match by character
matchx2 = match(jap$Japanese.name, jap.loan$original_script)
# match by reading character
matchx3 = match(jap$Katakana.reading, jap.loan$original_script)
# match by meaning
matchx4 = match(jap$English.name, jap.loan$meaning)

matchx[is.na(matchx)] = matchx2[is.na(matchx)]
matchx[is.na(matchx)] = matchx3[is.na(matchx)]
matchx[is.na(matchx)] = matchx4[is.na(matchx)]
sum(!is.na(matchx))

for(v in c("Borrowed",'age','age.youngest','age.oldest','age.youngest.num','age.oldest.num')){
  jap[,v] = jap.loan[matchx,v]
}

write.csv(jap, "../data/loanwords_Japanese.csv")


#s(phonlengthscale) + s(AoAscale) +   s(subtlexzipfscale) +  s(concscale) 

jap$phonlength = nchar(jap$Transcription)
jap$phonlengthscale = scale(jap$phonlength)

jap$AoAscale = scale(jap$Japanese..L1..age.of.acquisition)

jap$subtlexzipfscale = scale(jap$subtlexzipf)
jap$concscale = scale(jap$Concreteness..L1.)

# By definition, cognates are borrowed
jap$bor15.cat = NA
jap$bor15.cat[jap$Cognate.status=="C"] = 1
jap$bor15.cat[jap$Cognate.status=="NC"] = 0
# But there may be borrowings from other languages
jap$bor15.cat[jap$Borrowed=="1. clearly borrowed"]= 1
jap$bor15.cat = factor(jap$bor15.cat)
  
save(jap,file="../data/loanwords_Japanese.Rdat")
