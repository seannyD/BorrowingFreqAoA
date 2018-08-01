install.packages("sjPlot")
install.packages("party")
install.packages("lmtest")
install.packages("itsadug")
install.packages("ggfortify")
install.packages("factoextra")
install.packages("binom")
library(mgcv)
library(sjPlot)
library(lattice)
library(ggplot2)
library(dplyr)
library(party)
library(lmtest)
library(gridExtra)
library(scales)
library(itsadug)
library(ggfortify)
library(factoextra)
library(gridExtra)
library(reshape2)
library(binom)

logit2per = function(X){
  return(exp(X)/(1+exp(X)))
}

rescaleGam = function(px, n, xvar, xlab=""){
  y = logit2per(px[[n]]$fit)
  x = px[[n]]$x *attr(xvar,"scaled:scale") + attr(xvar,"scaled:center")
  se.upper = logit2per(px[[n]]$fit+px[[n]]$se)
  se.lower = logit2per(px[[n]]$fit-px[[n]]$se)
  dx = data.frame(x=x,y=y,ci.upper=se.upper,ci.lower=se.lower)
  plen = ggplot(dx, aes(x=x,y=y))+
    geom_ribbon(aes(ymin=ci.lower,ymax=ci.upper), alpha=0.3)+
    geom_line(size=1) +
    xlab(xlab)+
    ylab("Probability of borrowing")+
    coord_cartesian(ylim = c(0,1))
  return(plen)
}

setwd("~/Box Sync/papersonthego/LOAN_WORDS/VERYNEWANALYSES/")

dataloan <- read.csv("loanword10.csv",stringsAsFactors = F)
dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))
dataloan$bor15.cat <- factor(dataloan$bor15)

dataloan$subtlexzipf = as.numeric(dataloan$subtlexzipf)
dataloan$AoA = as.numeric(dataloan$AoA)
dataloan$conc = as.numeric(dataloan$conc)
dataloan$old.english.length = as.numeric(dataloan$old.english.length)
aoaSD = sd(dataloan$AoA,na.rm = T)
aoaMean = mean(dataloan$AoA/aoaSD,na.rm=T)
dataloan$cat = factor(dataloan$cat)
dataloan$effect = factor(dataloan$effect)

#Select only complete cases.

dataloan2 = dataloan[complete.cases(dataloan[,
               c("phonlength","AoA",
               "subtlexzipf", "cat",
               'conc','bor15')]),]

#Scale and center:

dataloan2$AoAscale <- scale(dataloan2$AoA)
dataloan2$subtlexzipfscale <- scale(dataloan2$subtlexzipf)
phonlength.center = median(dataloan2$phonlength)
dataloan2$phonlengthscale <-
  dataloan2$phonlength - phonlength.center
phonlength.scale = sd(dataloan2$phonlengthscale)
dataloan2$phonlengthscale = dataloan2$phonlengthscale/phonlength.scale
attr(dataloan2$phonlengthscale,"scaled:scale") = phonlength.scale
attr(dataloan2$phonlengthscale,"scaled:center") = phonlength.center
dataloan2$concscale <- scale(dataloan2$conc)
dataloan2$cat = relevel(dataloan2$cat,"Noun")
dataloan2$AoA_objscaled = scale(dataloan2$AoA_obj)

#GAM for Old English

# with old.english length as length:
# the idea of this analysis is to investigate whether length prior to borrowing affected whether the word
# was borrowed, rather than length of the borrowing.
# for that we need to go back to how the language was before the borrowings

#Select only complete cases with old english length also
dataloan3 = dataloan2[complete.cases(dataloan2[,
                                               c("phonlength","AoA",
                                                 "subtlexzipf", "cat",
                                                 'conc','bor15','old.english.length')]),]

old.english.length.center = median(dataloan3$old.english.length)
dataloan3$old.english.lengthscale <-
  dataloan3$old.english.length - old.english.length.center
old.english.length.scale = sd(dataloan3$old.english.lengthscale)
dataloan3$old.english.lengthscale = dataloan3$old.english.lengthscale/old.english.length.scale

attr(dataloan3$old.english.lengthscale,"scaled:scale") = old.english.length.scale
attr(dataloan3$old.english.lengthscale,"scaled:center") = old.english.length.center

m0oe = bam(bor15.cat ~
           s(old.english.lengthscale) + 
           s(AoAscale) + 
           s(subtlexzipfscale) +
           s(concscale) +
           s(cat,bs='re')+
           s(cat,old.english.lengthscale,bs='re')+
           s(cat,AoAscale,bs='re')+
           s(cat,subtlexzipfscale,bs='re')+
           s(cat,concscale,bs='re'),
         data = dataloan3,
         family='binomial')

# significant interaction between cat and old english length, but no significant effect of old english length on its own
# test effect of old english length without interactions
m1oe = bam(bor15.cat ~
             s(old.english.lengthscale) + 
             s(AoAscale) + 
             s(subtlexzipfscale) +
             s(concscale) +
             s(cat,bs='re'),
           data = dataloan3,
           family='binomial')

# now Old English length is significant and linear, as for Modern English phonlength. I'll be happy with that.
# But we are still including words that may have been borrowed BEFORE Old English, so select only those borrowed after Old English:


#classify as borrowed only those words borrowed after 900CE (so have changed since old english)
dataloan3$age_oldest_num <- as.numeric(dataloan3$age_oldest_num)
dataloan3$bor15oe <- ifelse(dataloan3$bor15==1 & dataloan3$age_oldest_num <=1100 ,1, ifelse(dataloan3$bor15==0,0,NA))
dataloan3$bor15oe.cat <- factor(dataloan3$bor15oe)


m2oe = bam(bor15oe.cat ~
             s(old.english.lengthscale) + 
             s(AoAscale) + 
             s(subtlexzipfscale) +
             s(concscale) +
             s(cat,bs='re')+
             s(cat,old.english.lengthscale,bs='re')+
             s(cat,AoAscale,bs='re')+
             s(cat,subtlexzipfscale,bs='re')+
             s(cat,concscale,bs='re'),
           data = dataloan3,
           family='binomial')

# again interaction between cat and length, so remove interactions to investigate main effects:
m3oe = bam(bor15oe.cat ~
             s(old.english.lengthscale) + 
             s(AoAscale) + 
             s(subtlexzipfscale) +
             s(concscale) +
             s(cat,bs='re'),
           data = dataloan3,
           family='binomial')

# it's significant, as before, linear effect of length

# This is evidence that length of the Old English form influenced the likelihood of that word subsequently being borrowed,
# and is not just a consequence of borrowed words coming from languages with longer word length than English.


#Model plots

#Plot the model estimates with Old English length, changing the dependent scale to probability and the independent variables to their original scales. The code uses the itsadug package and then rescales the variables back to the original units. This code is hidden, but you can view it in the Rmd file.

# plotting model estimates
px = plot.gam(m3oe,select=1, xlab="Old English Word length", ylab="Log odds of borrowing",shade = T)
px = plot.gam(m3oe,select=2, xlab="AoA", ylab="Log odds of borrowing",shade = T)
px = plot.gam(m3oe,select=3, xlab="Frequency", ylab="Log odds of borrowing",shade = T)

y = logit2per(px[[1]]$fit)
x = px[[1]]$x *old.english.length.scale + old.english.length.center
se.upper = logit2per(px[[1]]$fit+px[[1]]$se)
se.lower = logit2per(px[[1]]$fit-px[[1]]$se)
dx = data.frame(x=x,y=y,ci.upper=se.upper,ci.lower=se.lower)
poelen = ggplot(dx, aes(x=x,y=y))+
  geom_ribbon(aes(ymin=ci.lower,ymax=ci.upper), alpha=0.3)+
  geom_line(size=1) +
  xlab("Old English Word Length")+
  ylab("Probability of borrowing")+
  coord_cartesian(ylim = c(0,1))
paoa = rescaleGam(px,2,dataloan2$AoAscale, "Age of acquisition")
pfreq = rescaleGam(px,3,dataloan2$subtlexzipfscale, "Frequency")
grid.arrange(poelen,pfreq,paoa, nrow=1)
pdf(file='results/OldEnglish_ModelResults.pdf',
    height =3,width = 8)
grid.arrange(poelen,pfreq,paoa, nrow=1)
dev.off()



# Next analyses are for likelihood of borrowing just for the Swadesh words:

#Identify Swadesh words:

swd = read.csv("data/SwadeshConcepts.txt", header = F, stringsAsFactors = F)$V1
dataloan2$Swadesh = dataloan2$word %in% swd

m0sw = bam(bor15.cat ~
             s(phonlengthscale) + 
             s(AoAscale) + 
             s(subtlexzipfscale) +
             s(concscale) +
             s(cat,bs='re'),
           data = dataloan3,
           subset = Swadesh == TRUE,
           family='binomial')

# This gives the error:
#  Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#  A term has fewer unique covariate combinations than specified maximum degrees of freedom

#Run with just frequency (and category)?
m1sw = bam(bor15.cat ~
             s(subtlexzipfscale) +
             s(cat,bs='re'),
           data = dataloan3,
           subset = Swadesh == TRUE,
           family='binomial')
# this runs, with marginal (linear) effect of frequency - and shows high frequency less likely to be borrowed consistent with Pagel

px = plot.gam(m1sw,select=1, xlab="Frequency for Words in Swadesh list", ylab="Log odds of borrowing",shade = T)




# This works out influences on type of borrowing: coexistence, insertion, or replacement:

dataloan2$insertornot[dataloan2$effect == "Insertion"] = 1
dataloan2$insertornot[dataloan2$effect == "Coexistence"] = 0
dataloan2$insertornot[dataloan2$effect == "Replacement"] = 0
dataloan2$insertornot <- as.factor(dataloan2$insertornot)

dataloan2$coexistornot[dataloan2$effect == "Insertion"] = 0
dataloan2$coexistornot[dataloan2$effect == "Coexistence"] = 1
dataloan2$coexistornot[dataloan2$effect == "Replacement"] = 0
dataloan2$coexistornot <- as.factor(dataloan2$coexistornot)

dataloan2$replaceornot[dataloan2$effect == "Insertion"] = 0
dataloan2$replaceornot[dataloan2$effect == "Coexistence"] = 0
dataloan2$replaceornot[dataloan2$effect == "Replacement"] = 1
dataloan2$replaceornot <- as.factor(dataloan2$replaceornot)

m0insert = bam(insertornot ~
             s(phonlengthscale) + 
             s(AoAscale) + 
             s(subtlexzipfscale) +
             s(concscale) +
             s(cat,bs='re'),
           data = dataloan2,
           family='binomial')
# this shows that insertion borrowings are more likely to be concrete than other types of borrowing. No other significant effects
px = plot.gam(m0insert,select=4, xlab="Concreteness", ylab="Log odds of insertion borrowing",shade = T)

m0coexist = bam(coexistornot ~
                 s(phonlengthscale) + 
                 s(AoAscale) + 
                 s(subtlexzipfscale) +
                 s(concscale) +
                 s(cat,bs='re'),
               data = dataloan2,
               family='binomial')
# this shows that coexistence borrowings are more likely to be abstract than other types of borrowing, and affected by category.
px = plot.gam(m0coexist,select=4, xlab="Concreteness", ylab="Log odds of coexistence borrowing",shade = T)

m0replace = bam(replaceornot ~
                  s(phonlengthscale) + 
                  s(AoAscale) + 
                  s(subtlexzipfscale) +
                  s(concscale) +
                  s(cat,bs='re'),
                data = dataloan2,
                family='binomial')
# this shows that replacement borrowings are more likely to be abstract than other types of borrowing, and affected by category.
px = plot.gam(m0replace,select=4, xlab="Concreteness", ylab="Log odds of replacement borrowing",shade = T)
