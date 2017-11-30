setwd("~/Documents/MPI/MonaghanAoA/Stats 2/analysis/")

library(gplots)
library(mgcv)

d = read.csv("../pagel/loanword9.csv")
d$pagel_rate[d$pagel_rate=="#N/A"] = NA
d$pagel_rate = as.numeric(d$pagel_rate)
d = d[d$borrowing %in% c(1,5) & !is.na(d$pagel_rate),]
d$AoA = scale(as.numeric(d$AoA))
d$subtlexzipf = scale(as.numeric(d$subtlexzipf))
d$phonlength = scale(as.numeric(d$phonlength))

d$borrowing = factor(d$borrowing,levels=c(5,1),labels = c("no","yes"))

plotmeans(d$pagel_rate~d$borrowing)
plot(d$pagel_rate~d$AoA)

d$borrowing.num = as.numeric(d$borrowing)-1

m0 = lm(pagel_rate ~
          phonlength + 
          subtlexzipf,
        data = d)

summary(m0)

m1 = lm(pagel_rate ~
           phonlength + 
           subtlexzipf +
           AoA +
           borrowing,
         data = d)

summary(m0)
