dataloan <- read.csv("../pagel/loanword9.csv",stringsAsFactors = F)
dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))


dataloan$AoAscale <- scale(as.numeric(dataloan$AoA))
dataloan$subtlexzipfscale <- scale(as.numeric(dataloan$subtlexzipf))
dataloan$phonlengthscale <- scale(as.numeric(dataloan$phonlength))
dataloan$concscale <- scale(as.numeric(dataloan$conc))
dataloan$AoAlogscale <- scale(log(as.numeric(dataloan$AoA)))

pagelset <- subset(dataloan, as.numeric(pagel_rate, na.rm=TRUE)>0)

#find relation between rate of change in Pagel (2007) and borrowing in WOLD:
p0 <- glm(bor15 ~ as.numeric(pagel_rate), data=pagelset, family=binomial)
library(pscl)
pR2(p0)

#for pagel subset, is there a relation between borrowing and other psycholing variables?:
p1 <- glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot , data=pagelset)
summary(p1)
