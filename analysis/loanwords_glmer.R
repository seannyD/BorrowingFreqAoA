# This analysis treats POS as a random effect

library(lme4)
library(sjPlot)
library(lattice)

dataloan <- read.csv("../analysis/loanword8.csv",stringsAsFactors = F)
dataloan$bor15 <- ifelse(dataloan$borrowing==1,1, ifelse(dataloan$borrowing==5,0,NA))
dataloan$bor15.cat <- factor(dataloan$bor15)

# Convert to numbers
dataloan$subtlexzipf = as.numeric(dataloan$subtlexzipf)
dataloan$AoA <- as.numeric(dataloan$AoA)

dataloan$AoAscale <- scale(as.numeric(dataloan$AoA))
dataloan$subtlexzipfscale <- scale(as.numeric(dataloan$subtlexzipf))
dataloan$phonlengthscale <- scale(as.numeric(dataloan$phonlength))
dataloan$concscale <- scale(as.numeric(dataloan$conc))

dataloan2 = dataloan[complete.cases(dataloan[,c("phonlengthscale","AoAscale","subtlexzipfscale", "cat",'concscale')]),]


# Explore random effect structure

m0 = glmer(bor15.cat ~ 1 + (1 | cat),
           data=dataloan2, family = binomial)

# Random slope for AoA
m0.1 = glmer(bor15.cat ~ 1 + (1 | cat) + (0 + AoAscale|cat),
           data=dataloan2, family = binomial)
anova(m0,m0.1)

# Random slope for freq
m0.2 = glmer(bor15.cat ~ 1 + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat),
             data=dataloan2, family = binomial)
anova(m0.1,m0.2)

# Random slope for length
m0.3 = glmer(bor15.cat ~ 1 + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
             data=dataloan2, family = binomial)
anova(m0.2,m0.3)

# Random slope for concreteness?
m0.35 = glmer(bor15.cat ~ 1 + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat) + (0 + concscale | cat),
             data=dataloan2, family = binomial)
anova(m0.3,m0.35)
# No

# correlation parameters between random slopes
m0.4 = glmer(bor15.cat ~ 1 + (1 + AoAscale+ subtlexzipfscale + phonlengthscale| cat),
             data=dataloan2, family = binomial)
anova(m0.3,m0.4)

# All are significant, but correlation parameters cause convergence issue and add lots of degrees of freedom.  We'll leave them out.  
# Anyway, as we see in the later analyses, the random slopes actually account for very little variation in the final models.

# Main effect of length
m1 = glmer(bor15.cat ~ phonlengthscale + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
# Main effect of aoa
m2 = glmer(bor15.cat ~ phonlengthscale + AoAscale + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
# Main effect of freq
m3 = glmer(bor15.cat ~ phonlengthscale + AoAscale + subtlexzipfscale  + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m0.3,m1,m2,m3)
# ... all are significant

# Main effect of concreteness
m3.5 = glmer(bor15.cat ~ phonlengthscale + AoAscale + subtlexzipfscale + concscale + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m3,m3.5)
# ... not significant

# Interaction between AoA and freq
m4 = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale  + concscale +  (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m3,m4)
# .. significant

# Interaction between AoA and length
m5 = glmer(bor15.cat ~ phonlengthscale*AoAscale + AoAscale*subtlexzipfscale + concscale + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m4,m5)
# No, plus bad convergence

# Interaction between length and freq
m6 = glmer(bor15.cat ~ phonlengthscale+AoAscale + AoAscale*subtlexzipfscale +
             subtlexzipfscale*phonlengthscale + concscale + (1 | cat) + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m4,m6)
# No, plus bad convergence


# Best model is model 4 with all main effects and freq x aoa

# Summary of coefficients
summary(m4)

# plot fixed effects
sjp.glmer(m4, 'fe')
# plot random effects
dotplot(ranef(m4))

# Main random effects are for the general level of borrowing prob., 
#  very little difference for slopes

# Check the effect of random intercept with the full model:
m4.nointercept = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale  + concscale + (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
           data=dataloan2, family = binomial)
anova(m4,m4.nointercept)
# ... significant: different POS vary in their baseline probability of being borrowed


# Check the effects of random slopes with the full model:
#  (i.e. do the strength of the main effects vary by POS?)

m4.noAoASlope = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale  + concscale +  (1 | cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat),
                           data=dataloan2, family = binomial)
anova(m4, m4.noAoASlope )

m4.nofreqSlope = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale  + concscale +  (1 | cat) + (0 + AoAscale | cat) + (0 + phonlengthscale| cat),
                      data=dataloan2, family = binomial)
anova(m4, m4.nofreqSlope )

m4.nolengthSlope = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale  + concscale +  (1 | cat) + (0 + AoAscale | cat) + (0 + subtlexzipfscale| cat),
                       data=dataloan2, family = binomial)
anova(m4, m4.nolengthSlope )

# random slopes do not improve the fit of the model over the fixed effects.  
# So strength does not vary by POS



# Let's check random effect for interaction:

m4.1 = glmer(bor15.cat ~ phonlengthscale + AoAscale*subtlexzipfscale + concscale + (1 | cat) + 
               (0 + AoAscale|cat) + (0 + subtlexzipfscale | cat) + (0 + phonlengthscale| cat) + (0 + AoAscale:subtlexzipfscale|cat),
           data=dataloan2, family = binomial)
summary(m4.1)
dotplot(ranef(m4.1))
# Basically no difference