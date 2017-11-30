library(ggplot2)



dataloan <- read.csv("../analysis/loanword8.csv",stringsAsFactors = F)
dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))

# Convert to numbers
dataloan$subtlexzipf = as.numeric(dataloan$subtlexzipf)
dataloan$AoA <- as.numeric(dataloan$AoA)

dataloan$AoAscale <- scale(as.numeric(dataloan$AoA))
dataloan$subtlexzipfscale <- scale(as.numeric(dataloan$subtlexzipf))
dataloan$phonlengthscale <- scale(as.numeric(dataloan$phonlength))
dataloan$concscale <- scale(as.numeric(dataloan$conc))

loanmodelscat <- glm(bor15 ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot + advornot, data=dataloan, family = binomial)
loanmodelscatnoaoa <- glm(bor15 ~ phonlengthscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot + advornot, data=dataloan, family = binomial)
loanmodelscatnoaoanolen <- glm(bor15 ~ subtlexzipfscale + concscale + nounornot + verbornot +adjornot + advornot, data=dataloan, family = binomial)


# The models suggest a positive relationship between frequency and borrowing.
# ... however, plotting the data suggests a negative relationship:

# Plot curve of freq vs borrowing:
ggplot(dataloan, aes(x =subtlexzipf,y=bor15)) + stat_smooth(method='loess') + xlab("Frequency") + ylab("Probability of borrowing")
# ... (note that the loess curve suggests a non-linear relationship)

# The explanation may be an interaction between AoA and borrowing

# Cut AoA into 3 quantiles:
# To get even numbers in each break
#AoA.breaks = quantile(dataloan$AoA,probs = seq(0,1,length.out = 4), na.rm = T)
# By theory motivated ages
AoA.breaks = c(2,4,6,18)
dataloan$AoA.cut= cut(dataloan$AoA, breaks=AoA.breaks , include.lowest = T)
table(dataloan$AoA.cut)

# Plot freq x borrowing for each AoA stage
ggplot(dataloan[!is.na(dataloan$AoA),], aes(x =subtlexzipf,y=bor15, colour=AoA.cut)) + stat_smooth(method='gam') + xlab("Frequency") + ylab("Probability of borrowing")
# ... we see that words learned later have a positive relationship


# Test for a statistical interaction between AoA and frequency:
loanmodelscat_int <- glm(bor15 ~ 
                              phonlengthscale + 
                              AoAscale * subtlexzipfscale + 
                              concscale + nounornot + verbornot +adjornot + advornot, 
                            data=dataloan, family = binomial)
summary(loanmodelscat_int)
# Yes. 

# However, the overall relationship between borrowing and freq is non-linear

#######
# test quadratic effect of frequency

loanmodelscat_qf <- glm(bor15 ~ 
                          phonlengthscale + 
                          AoAscale + 
                          subtlexzipfscale + 
                          I(subtlexzipfscale^2) + # Quadratic effect
                          concscale + nounornot + verbornot +adjornot + advornot, 
                        data=dataloan, family = binomial)
summary(loanmodelscat_qf)
anova(loanmodelscat,loanmodelscat_qf, test = 'Chisq')

# test cubic effect of frequency

loanmodelscat_qf2 <- glm(bor15 ~ 
                          phonlengthscale + 
                          AoAscale + 
                          subtlexzipfscale + 
                          I(subtlexzipfscale^2) +
                          I(subtlexzipfscale^3) + # Cubic effect
                          concscale + nounornot + verbornot +adjornot + advornot, 
                        data=dataloan, family = binomial)

summary(loanmodelscat_qf2)
anova(loanmodelscat_qf,loanmodelscat_qf2, test = 'Chisq')
# (not significant)

# test quadratic effect of aoa
loanmodelscat_qaoa <- glm(bor15 ~ 
                          phonlengthscale + 
                          AoAscale + 
                          I(AoAscale^2) +
                          subtlexzipfscale + 
                          I(subtlexzipfscale^2) +
                          concscale + nounornot + verbornot +adjornot + advornot, 
                        data=dataloan, family = binomial)
summary(loanmodelscat_qaoa)
# (not significant)

##
# test interaction between AoA and frequency, when also having quadratic effect of freq
loanmodelscat_qf_int <- glm(bor15 ~ 
                           phonlengthscale + 
                           AoAscale * subtlexzipfscale + 
                           I(subtlexzipfscale^2) +
                           concscale + nounornot + verbornot +adjornot + advornot, 
                         data=dataloan, family = binomial)
summary(loanmodelscat_qf_int)
# no significant interaction

# test interaction with quadratic term
loanmodelscat_qf_int2 <- glm(bor15 ~ 
                              phonlengthscale + 
                              AoAscale +
                              subtlexzipfscale + 
                              I(subtlexzipfscale^2)* AoAscale +
                              concscale + nounornot + verbornot +adjornot + advornot, 
                            data=dataloan, family = binomial)
summary(loanmodelscat_qf_int2)
# no significant interaction

# ... so the interaction between freq and aoa disappears when adding a "non-linear" effect for frequency.  This suggests that they're picking up on the same pattern.


########
# Maybe freq effects are different for different POS?
# Test interaction between freq and POS
loanmodelscat_poslength <- glm(bor15 ~ 
                           phonlengthscale + 
                           AoAscale + subtlexzipfscale + 
                          subtlexzipfscale:(concscale + nounornot + verbornot +adjornot + advornot), 
                         data=dataloan, family = binomial)
summary(loanmodelscat_poslength)
# (yes - adv is neg, others are positive, but different strengths)

###############
library("pscl", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
pR2(loanmodelscat)


#what about cumulative frequency?
dataloan$cumfreq <- (18-dataloan$AoA) * dataloan$subtlexzipf
dataloan$cumfreqscale <- scale(dataloan$cumfreq)
loanmodelscat <- glm(bor15 ~ phonlengthscale + AoAscale + cumfreqscale + concscale + nounornot + verbornot +adjornot + advornot, data=dataloan, family = binomial)


#get how long ago, just for certainly borrowed:
dataloanborrows <- subset(dataloan, bor15==1)
transform(dataloanborrows, age_oldest_num = as.numeric(age_oldest_num))
transform(dataloanborrows, age_youngest_num = as.numeric(age_youngest_num))

cor(dataloanborrows$age_oldest_num,dataloanborrows$age_youngest_num)

#this shows that correlation = .996 between youngest and oldest point of entry for borrowed words - so can use either.
dataloanborrows$AoAscale <- scale(dataloanborrows$AoA)
dataloanborrows$subtlexzipfscale <- scale(dataloanborrows$subtlexzipf)
dataloanborrows$phonlengthscale <- scale(dataloanborrows$phonlength)
dataloanborrows$concscale <- scale(dataloanborrows$conc)
dataloanborrows$cumfreqscale <- scale(dataloanborrows$cumfreq)

loanmodelhowlong <- lm( log(age_oldest_num) ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot, data=dataloanborrows)
loanmodelhowlong <- lm( log(age_oldest_num) ~ phonlengthscale + AoAscale + cumfreqscale  + concscale + nounornot + verbornot +adjornot, data=dataloanborrows)
loanmodelhowlongnofreq <- lm(log(age_oldest_num) ~ phonlengthscale + AoAscale + concscale + nounornot + verbornot +adjornot, data=dataloanborrows)

#just for recent borrowings - not the language-family ones:
dataloanborrowsrecent <- subset(dataloanborrows, age_oldest_num<1200)
# there are 385 nouns, 96 verbs, 45 adjectives (1 adverb, 1 pronoun, 1 determiner, 1 number, 1 name, 1 conjunction)
loanmodelhowlong <- glm(age_oldest_num ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + nounornot + verbornot +adjornot, data=dataloanborrowsrecent)

