try(setwd("~/Documents/MPI/MonaghanAoA/Stats 2/analysis/"))

dutch = read.csv("../data/loanwords_Dutch.csv", stringsAsFactors = F)

dutch$bor15 = ifelse(dutch$Borrowed=="1. clearly borrowed",1, ifelse(dutch$Borrowed=="5. no evidence for borrowing",0,NA))

dutch$nounornot = as.numeric(dutch$subtlex.dominant.pos == "N")
dutch$verbornot = as.numeric(dutch$subtlex.dominant.pos %in% c("WW"))
dutch$adjornot = as.numeric(dutch$subtlex.dominant.pos %in% c("ADJ"))
dutch$advornot = as.numeric(dutch$subtlex.dominant.pos %in% c("BW"))


dutch$AoAscale <- scale(as.numeric(dutch$aoa))
dutch$subtlexzipfscale <- log10(as.numeric(dutch$subtlex.dominant.pos.frequency))
dutch$phonlengthscale <- scale(as.numeric(dutch$length))
dutch$concscale <- scale(as.numeric(dutch$concreteness))

dutch2 = dutch[complete.cases(dutch[,c("bor15","phonlengthscale",'AoAscale','subtlexzipfscale','concscale')]),]

dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("LID"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("TSW"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("TW"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("VG"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("VNW"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("VZ"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("SPEC"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("N"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("ADJ"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("WW"))
dutch2$tmp = as.numeric(dutch2$subtlex.dominant.pos %in% c("BW"))
summary(dutch2$tmp)

# Note that there are many parts of speech other than nouns, verbs, adjectives and adverbs
#dutch2 = dutch2[dutch2$subtlex.dominant.pos %in% c("N",'WW','ADJ','BW'),]
# 630 nouns, 196 verbs, 104 adj, 23 adverbs, 

m0 =  glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m0)
m0a = glm(bor15 ~ subtlexzipfscale + concscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m0a)
m0b =  glm(bor15 ~ subtlexzipfscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m0b)
m0c =  glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m0c)


# Interaction between freq and aoa
m1.int = glm(bor15 ~ phonlengthscale + AoAscale*subtlexzipfscale + concscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m1.int)

# quadratic effect of freq
m1.qf = glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + I(subtlexzipfscale^2) + concscale + nounornot + verbornot +adjornot +advornot, data=dutch2, family = binomial)
summary(m1.qf)
# (no quadtratic effect)


# how long ago just for borrowings. 1 adverb borrowed, so not included as cat variable. 15 adjectives borrowed, 9 verbs, 174 nouns
dutchborrows <- subset(dutch2, bor15==1)
cor(dutchborrows$age.oldest.num,dutchborrows$age.youngest.num)

dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("LID")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("TSW")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("TW")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("VG")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("VNW")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("VZ")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("SPEC")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("N")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("ADJ")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("WW")); sum(dutchborrows$tmp)
dutchborrows$tmp = as.numeric(dutchborrows$subtlex.dominant.pos %in% c("BW")); sum(dutchborrows$tmp)


m1 = loanmodelhowlong <- lm(age.oldest.num ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot, data=dutchborrows)
summary(m1)


##
library(ggplot2)
ggplot(dutch, aes(subtlexzipfscale,bor15)) + stat_smooth()

#AoA.breaks = quantile(dutch$aoa,probs = seq(0,1,length.out = 4), na.rm = T)
AoA.breaks = c(2,4,6,18)
dutch$AoA.cut= cut(dutch$aoa, breaks=AoA.breaks , include.lowest = T)
table(dutch$AoA.cut)

# Plot freq x borrowing for each AoA stage
ggplot(dutch[!is.na(dutch$aoa),], aes(x =log10(subtlex.dominant.pos.frequency),y=bor15, colour=AoA.cut)) + stat_smooth(method='gam') + xlab("Frequency") + ylab("Probability of borrowing")

