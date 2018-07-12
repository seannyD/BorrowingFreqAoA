# Get data from the ASJP. 
#  - Work out the average word length for each language
#  - For each borrowed word, work out the frequency of having a word of that length in the source language
#  Missing values are filled in by regression

try(setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/"))

dataloan2 = read.csv("../data/loanword8.csv",stringsAsFactors = F, encoding = "UTF-8",fileEncoding = "UTF-8")

dataloan2[is.na(dataloan2$source.language),]$source.language = "English"


glottocodes = c(
"English" = "stan1293",
"French" = "stan1290",
"Latin" = "lati1261",
"Dharuk" = "sydn1236",
"Guugu Yimidhirr" = "gugu1255",
"Tahitian" = "tahi1242",
"Spanish" = "stan1288",
"Late Latin" = "lati1261", # Latin
"Narragansett" = "pequ1242",# Mohegan, which is part of the same dialect chain as Narragansett 'mohe1244',
"French (Anglo-Norman)" = "stan1290",# Substituting standard french (not much else in ASJP)
"Eastern Abenaki" = "east2544",
"Algonquian (Powhatan)" = "powh1243",
"Tupí" = "tupi1273",# Substituting Tupinamba instead of Tupi "tupi1274",
"Dutch" = "dutc1256",
"Italian" = "ital1282",
"Portuguese" = "port1283",
"Middle French" = "stan1290",
"Welsh" = "wels1247",
"Old Norse" = "oldn1244",
"Gaelic (Scottish)" = "scot1245",
"Middle Low German" = "nort2627",# Subsituting Eastern low german instead of Middle Low German "midd1318",
"Greek" = "anci1242", # Ancient greek
"Vulgar Latin" = "lati1261", # Latin
"Malay" = "mala1479",
"Middle North Germanic" = "oldh1241",# Old high german
"Middle High German"= "oldh1241",# Old high german,
"Middle Dutch" = "dutc1256" # Modern dutch
)

asjp.lang = read.csv("../data/asjp_dataset.cldf/languages.csv",stringsAsFactors = F, encoding = "UTF-8",fileEncoding = "UTF-8")

asjp.codes = data.frame(
  name = names(glottocodes),
  glottocode = glottocodes,
  asjp.ID = asjp.lang[match(glottocodes,asjp.lang$Glottocode),]$ID
)

asjp.codes = rbind(
  asjp.codes,
  data.frame(
    name = c("Celtic",'Old French',"French (Walloon)","Norwegian"),
    glottocode = c(NA,NA),
    asjp.ID = c("PROTO_CELTIC","WALLOON","WALLOON","NORWEGIAN_BOKMAAL")
  )
)
asjp.codes[is.na(asjp.codes$asjp.ID),]

asjp.words = read.csv("../data/asjp_dataset.cldf/forms.csv",stringsAsFactors = F, encoding = "UTF-8",fileEncoding = "UTF-8")

asjp.words = asjp.words[asjp.words$Language_ID %in% asjp.codes$asjp.ID,]

# Clean forms
asjp.words$Form = gsub("[\\*~ \\$]","",asjp.words$Form)


# Mean lengths

mean.lengths = tapply(asjp.words$Form, asjp.words$Language_ID, function(X){mean(nchar(X))})

dataloan2$source.language.mean.word.length = 
  mean.lengths[asjp.codes[match(dataloan2$source.language,asjp.codes$name),]$asjp.ID]

#dataloan2$source.language.mean.word.length[is.na(dataloan2$source.language)] = mean.lengths["ENGLISH"]

dataloan2$source.language.word.freq = NA
dataloan2$source.word.length = nchar(dataloan2$source.word)
dataloan2[is.na(dataloan2$source.word.length),]$source.word.length = dataloan2[is.na(dataloan2$source.word.length),]$phonlength 

langs= as.character(unique(dataloan2$source.language))
langs = langs[!is.na(langs)]
langs = langs[langs!="Unidentified"]

for(l in langs){
  sel = dataloan2$source.language==l & !is.na(dataloan2$source.language)
  lwords = asjp.words[asjp.words$Language_ID == 
               asjp.codes[match(l,asjp.codes$name),]$asjp.ID,]$Form
  lenfreq = table(nchar(lwords))
  wordLength = as.numeric(names(lenfreq))
  
  lengthsNeeded = sort(unique(dataloan2[sel,]$source.word.length))
  if(!all(lengthsNeeded %in% wordLength)){
    # fit a poisson regression to the data to 
    # fill in missing values
    missingVals = setdiff(lengthsNeeded,wordLength)
    print(paste("Pred",length(missingVals),"vals for",l))
    lenModel = glm(as.numeric(lenfreq) ~ 
                     wordLength +
                     I(wordLength^2), 
                   family = poisson)
    plot(exp(predict(lenModel)),as.numeric(lenfreq),
         main=l)
    abline(0,1)
    predictedLenFreq = 
      exp(predict(lenModel,
              newdata=data.frame(
                wordLength=missingVals)))
    names(predictedLenFreq) = missingVals
    lenfreq = c(lenfreq,predictedLenFreq)
  }
  
  lenfreq = lenfreq/sum(lenfreq)
  dataloan2[sel,]$source.language.word.freq = 
    lenfreq[match(dataloan2[sel,]$source.word.length,
                  as.numeric(names(lenfreq)))]
  
}



dataloan2 = dataloan2[,!names(dataloan2) %in%c("X","X.1","X.4","X.3","X.2","nounornot","verbornot","adjornot","advornot","pronounornot","functionornot","numberornot")]

write.csv(dataloan2, "../data/loanword9.csv", row.names = F)
  


  
  
  
  
  
  