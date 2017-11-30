setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/")
library(stringr)
#read.delim("../data/otherLangs/German_AstridSchroder_13428_2011_164_MOESM1_ESM.tab",sep="\t")

d = read.delim("../data/otherLangs/Ferrand-BRM-2008/Ferrand(2008).txt",sep='\t', stringsAsFactors = F, encoding = 'utf-8', fileEncoding = 'utf-8')

d2 = read.delim("../data/otherLangs/Ferrand-BRM-2010/FLP-words.txt", sep='\t', stringsAsFactors = F, encoding = 'utf-8', fileEncoding = 'utf-8')

d$freq2 = d2[match(d$Stimuli,d2$item),]$c_freq_books

# Lexique database: http://www.lexique.org/telLexique.php
lx = read.delim("../data/otherLangs/Lexique381/Lexique381.txt", sep="\t", stringsAsFactors = F)

d$cat = lx[match(d$Stimuli,lx$X1_ortho),]$X4_cgram
d$cat = substr(d$cat,1,3)
d$cat = factor(d$cat)
d$cat = relevel(d$cat,"NOM")

d$phonlength = nchar(lx[match(d$Stimuli,lx$X1_ortho),]$X27_phonrenv)


filenames = c("../data/otherLangs/FrenchLoanwords/List of French words of Germanic origin (A-B) - Wikipedia.html",
      "../data/otherLangs/FrenchLoanwords/List of French words of Germanic origin (C-G) - Wikipedia.html",
      "../data/otherLangs/FrenchLoanwords/List of French words of Germanic origin (H–Z) - Wikipedia.html")

for(filename in filenames){
  f = readLines(filename, encoding = 'utf-8')
  res = str_match_all(f,'wikt:([^"]+)"')
  res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
  words = c(words,res)
}

f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine anglaise — Wiktionnaire.htm",encoding = 'utf-8')
res = str_match_all(f,'<i>([^<]+)</i>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res2 = str_match_all(f,'wiktionary.org/wiki/([^"]+)"')
res2 = unlist(sapply(res2,function(X){if(nrow(X)>0){X[1,2]}}))
res2 = res2[!grepl("[:%]",res2)]
words = c(words,res)

f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine hébraïque — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[1:79]
res = res[!grepl("w:",res)]
words = c(words,res)

f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine amérindienne — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[!grepl("w:",res)]
res = res[!grepl(":",res)]
res = res[!grepl(" ",res)]
res = res[!grepl("page",res)]
res = res[!res%in%c("français", "dictionnaire")]
words = c(words,res)


f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine indienne — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[!grepl("w:",res)]
res = res[!grepl(":",res)]
res = res[!grepl(" ",res)]
res = res[!grepl("page",res)]
words = c(words,res)
words = c(words,"calicot",'camphre','coolie','tank')


f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine scandinave — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[!grepl("w:",res)]
res = res[!grepl(":",res)]
res = res[!grepl(" ",res)]
res = res[!grepl("page",res)]
res = res[!res%in%c("français", "dictionnaire")]
words = c(words,res)

f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots en français d’origine russe — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[!grepl("w:",res)]
res = res[!grepl(":",res)]
res = res[!grepl(" ",res)]
res = res[!grepl("page",res)]
res = res[!res%in%c("français", "dictionnaire")]
words = c(words,res)

f = readLines("../data/otherLangs/FrenchLoanwords/Annexe_Mots français d’origine arabe — Wiktionnaire.htm", encoding = 'utf-8')
res =str_match_all(f,'title="([^"]+)">[^<]+</a>')
res = unlist(sapply(res,function(X){if(nrow(X)>0){X[1,2]}}))
res = res[!grepl("w:",res)]
res = res[!grepl(":",res)]
res = res[!grepl(" ",res)]
res = res[!grepl("page",res)]
res = res[!res%in%c("français", "dictionnaire")]
words = c(words,res)



gaul = read.delim("../data/otherLangs/FrenchLoanwords/FrenchFromGaulish.tab", stringsAsFactors = F, encoding = 'utf-8', fileEncoding = 'utf-8')

gaul.words = gsub( " .+","",gaul$French)
gaul.words = gsub("[,.]","",gaul.words)
gaul.words = gsub("\\([^\\)]+\\)","",gaul.words)

words = sort(unique(c(words,gaul.words)))

sum(d$Stimuli %in% words)

d$Borrowed = tolower(d$Stimuli) %in% tolower(words)
table(d$Borrowed)
prop.table(table(d$Borrowed))


write.csv(d,"../data/loanwords_French.csv", row.names = F)

#s(phonlengthscale) + s(AoAscale) +   s(subtlexzipfscale) +  s(concscale) 
d = d[!is.na(d$freq2),]

d$orthlengthscale = scale(nchar(d$Stimuli))
d$AoAscale = scale(d$AoA)
#d$subtlexzipfscale = scale(log(d$freq2))
d$subtlexzipfscale = log10(1+lx[match(d$Stimuli,lx$X1_ortho),]$X7_freqlemfilms2)
d$subtlexzipfscale = scale(d$subtlexzipfscale)
d$phonlengthscale = scale(d$phonlength)
d$bor15 = as.numeric(d$Borrowed)
d$bor15.cat = factor(d$bor15)
french = d
save(french,file="../data/loanwords_French.Rdat")

