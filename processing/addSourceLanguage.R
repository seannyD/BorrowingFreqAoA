dataloan <- read.csv("../analysis/loanword8.csv",stringsAsFactors = F)
dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))



dataloan$AoAscale <- scale(as.numeric(dataloan$AoA))
dataloan$subtlexzipfscale <- scale(as.numeric(dataloan$subtlexzipf))
dataloan$phonlengthscale <- scale(as.numeric(dataloan$phonlength))
dataloan$concscale <- scale(as.numeric(dataloan$conc))


engData = read.csv("../data/wold-dataset.cldf/wold-vocabulary-13.csv", stringsAsFactors = F)
engData$word = gsub(" *\\(.*?\\) *","",engData$Value)

sum(grepl("French",engData$contact_situation))
sum(grepl("German",engData$contact_situation))
sum(grepl("Dutch",engData$contact_situation))

contactWords = unlist(sapply(engData$contact_situation, function(X){
  unlist(strsplit(X," "))
}))

sort(table(contactWords))

engData$sourceLanguage = "Other"
for(l in c('Latin','Norse','French', 'German', 'Dutch','Spanish') ){
  engData[grepl(l,engData$contact_situation),]$sourceLanguage = l
}

engData[grepl("Germanic",engData$contact_situation),]$sourceLanguage = "German"

engData[grepl("Celtic",engData$contact_situation) |
          grepl("Gaelic",engData$contact_situation) |
          grepl("Welsh",engData$contact_situation) |
          grepl("Scottish",engData$contact_situation),]$sourceLanguage = "Celtic"

engData[grepl("None",engData$contact_situation),]$sourceLanguage = "English"

dataloan$sourceLanguage = engData[
  match(dataloan$word, engData$word),]$sourceLanguage


loanmodelscat <- glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot + advornot, data=dataloan, family = binomial)
summary(loanmodelscat)

library(lme4)
library(sjPlot)

dataloan$verbornot.cat = as.factor(dataloan$verbornot)
dataloan$nounornot.cat = as.factor(dataloan$nounornot)
dataloan$adjornot.cat = as.factor(dataloan$adjornot)
dataloan$advornot.cat = as.factor(dataloan$advornot)
dataloan$bor15.cat = as.factor(dataloan$bor15)

# linear model
lmmodel = lm(AoAscale ~ bor15.cat, data=dataloan)
summary(lmmodel)

# mixed effects model
lmermodel0 = lmer(AoAscale ~ 1 + (1 | sourceLanguage), data = dataloan[!is.na(dataloan$bor15.cat),])

lmermodel1 = lmer(AoAscale ~ 1 + (1 + bor15.cat| sourceLanguage), data = dataloan[!is.na(dataloan$bor15.cat),])

anova(lmermodel0,lmermodel1)

lmermodel2 = lmer(AoAscale ~ bor15.cat 
                  + (1 | sourceLanguage), data = dataloan[!is.na(dataloan$bor15.cat),])

anova(lmermodel0,lmermodel2)

summary(lmermodel)
sjp.lmer(lmermodel2, 're')
sjp.lmer(lmermodel2, 'fe')
