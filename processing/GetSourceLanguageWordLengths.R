try(setwd("~/Documents/MPI/MonaghanAoA/Stats 2/processing/"))


glottocodes = c(
"English" = "stan1293",
"French" = "stan1290",
"Latin" = "lati1261",
"Dharuk" = "sydn1236",
"Guugu Yimidhirr" = "gugu1255",
"Tahitian" = "tahi1242",
"Spanish" = "stan1288",
"Late Latin" = "lati1261",
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
"Greek" = "anci1242",
"Vulgar Latin" = "lati1261"
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
    name = c("Celtic",'Old French'),
    glottocode = c(NA,NA),
    asjp.ID = c("PROTO_CELTIC","WALLOON")
  )
)
asjp.codes[is.na(asjp.codes$asjp.ID),]

asjp.words = read.csv("../data/asjp_dataset.cldf/forms.csv",stringsAsFactors = F, encoding = "UTF-8",fileEncoding = "UTF-8")

asjp.words = asjp.words[asjp.words$Language_ID %in% asjp.codes$asjp.ID,]

# Clean forms
asjp.words$Form = gsub("[\\*~ \\$]","",asjp.words$Form)

mean.lengths = tapply(asjp.words$Form, asjp.words$Language_ID, function(X){mean(nchar(X))})

dataloan2 = read.csv("../data/loanword8.csv",stringsAsFactors = F, encoding = "UTF-8",fileEncoding = "UTF-8")

dataloan2$source.language.mean.word.length = 
  mean.lengths[asjp.codes[match(dataloan2$source.language,asjp.codes$name),]$asjp.ID]

dataloan2$source.language.mean.word.length[is.na(dataloan2$source.language)] = mean.lengths["ENGLISH"]


  
  
  
  
  
  
  
  