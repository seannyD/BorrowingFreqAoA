
dataloan2 = dataloan2[complete.cases(dataloan2[,c("bor15.cat",'phonlengthscale','AoAscale','subtlexzipfscale','concscale')]),]
logit2per = function(X){
  return(exp(X)/(1+exp(X)))
}

m4 = glmer(bor15.cat ~ phonlengthscale +
             AoAscale*subtlexzipfscale  + 
             concscale +  
             (1 | cat) + 
             (0 + AoAscale|cat) + 
             (0 + subtlexzipfscale | cat) + 
             (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial,
           control=glmerControl(
             optCtrl=list(maxfun=50000),
             optimizer = 'bobyqa'))

m4a = glmer(bor15.cat ~ phonlengthscale +
             AoAscale*subtlexzipfscale  + 
              I(subtlexzipfscale^2) +
             concscale +  
             (1 | cat) + 
             (0 + AoAscale|cat) + 
             (0 + subtlexzipfscale | cat) + 
             (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)


summary(m4b)
anova(m3,m4,m4a)

library(mgcv)
gam0 = bam(bor15.cat ~ 
             s(phonlengthscale) +
             s(AoAscale) +
             s(subtlexzipfscale)  + 
             s(subtlexzipfscale, by=AoAscale)  + 
             s(subtlexzipfscale, by=concscale)  + 
             s(concscale),
           family=binomial(),
           data= dataloan2)

summary(gam0)

####

dx = data.frame(
  phonlengthscale = seq(
    range(dataloan2$phonlengthscale)[1],
    range(dataloan2$phonlengthscale)[2],
    length.out = 100
  ),
  AoAscale = 0,
  subtlexzipfscale = 0,
  concscale = 0
)

dxp = logit2per(predict(gam0, newdata=dx))

plot(dxp~dx$phonlengthscale, type='l')

#####

dx = data.frame(
  phonlengthscale = 0,
  AoAscale = seq(
    range(dataloan2$AoAscale)[1],
    range(dataloan2$AoAscale)[2],
    length.out = 100
  ),
  subtlexzipfscale = 0,
  concscale = 0
)

dxp = logit2per(predict(gam0, newdata=dx))

plot(dxp~dx$AoAscale, type='l')


####

dx = data.frame(
  phonlengthscale = 0,
  AoAscale = 0,
  subtlexzipfscale = seq(
    range(dataloan2$subtlexzipfscale)[1],
    range(dataloan2$subtlexzipfscale)[2],
    length.out = 100
  ),
  concscale = 0
)

dxp = logit2per(predict(gam0, newdata=dx))

plot(dxp~dx$subtlexzipfscale, type='l')
