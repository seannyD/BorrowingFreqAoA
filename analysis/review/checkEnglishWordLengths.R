setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis")


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

setwd("~/Documents/MPI/MonaghanAoA/Stats 2/analysis/")

numConcepts = table(alldata$glotto)

alldata = alldata[numConcepts[alldata$glotto]>500,]



lengths = tapply(alldata$word.clean, alldata$glotto, function(d){
  lx = nchar(unlist(strsplit(d,";")))
  lx[lx<20]
})

plot(0,0,ylim=c(0,0.3),xlim=c(0,34), type='n',
     xlab = "Word length",
     ylab = "Density")
for(lang in unique(alldata$glotto)){
  dx = density(lengths[[lang]], bw = 1)
  lines(dx)
}
dx.eng = density(lengths[["stan1293"]], bw = 1)
lines(dx.eng,col=2)
dx.dut = density(lengths[["dutc1256"]], bw = 1)
lines(dx.dut,col=3)


meanLengths = sapply(lengths,mean,na.rm=T)
hist(meanLengths)
abline(v=mean(lengths[["stan1293"]]),col=2)
abline(v=mean(lengths[["dutc1256"]]),col=3)

quantile(meanLengths,0.05)


###########

dataloan <- read.csv("../data/loanword8.csv",stringsAsFactors = F, encoding = 'utf-8',fileEncoding = 'utf-8')
#dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))
#dataloan$bor15.cat <- factor(dataloan$bor15)

#dataloan = dataloan[!is.na(dataloan$bor15),]

sources = read.csv("../data/wold-dataset.cldf/loans.txt",sep="|", stringsAsFactors = F, encoding = 'utf-8',fileEncoding = 'utf-8')
names(sources) = c("original.word",'source.lang','relation','certain','dest.word','dest.lang')

sources[sources$dest.word=="tree trunk",]$dest.word = "trunk"
sources[sources$dest.word=="pile up",]$dest.word = "pile"


sources.eng = sources[sources$dest.lang=="English",]

sources.eng$dest.word = gsub("\\(.+\\)","",sources.eng$dest.word)
sources.eng$dest.word = gsub("^ ","",sources.eng$dest.word)
sources.eng$dest.word = gsub(" $","",sources.eng$dest.word)

sources.eng = sources.eng[order(sources.eng$relation,decreasing = T),]

dataloan$source.language = sources.eng[match(dataloan$word,sources.eng$dest.word),]$source.lang
dataloan$source.word = sources.eng[match(dataloan$word,sources.eng$dest.word),]$original.word

write.csv(dataloan,"../data/loanword8.csv")