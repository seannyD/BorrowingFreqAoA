#AoA graphs:
a<-2:18

#english aoa prob:
y<- -2.62573 + 0.49680* (a*sd(as.numeric(dataloan$AoA), na.rm=TRUE)-mean(as.numeric(dataloan$AoA), na.rm=TRUE))
py = 1 - 1/(1+exp(y))
#dutch aoa prob:
z<- -3.79927 + 0.45550*(a*sd(as.numeric(dutch2$AoA), na.rm=TRUE)-mean(as.numeric(dutch2$AoA), na.rm=TRUE))
pz = 1 - 1/(1+exp(z))

tiff("probability_graph_aoa.tiff"); par(new=F)
plot(a,py, type="o",xlab="Age of Acquisition",ylab="Probability of Borrowing",xlim=c(2,18),xaxt="n"); 
par(new=T)
lines(a, pz, type="o", xlab="",ylab="",lty=2,xaxt="n",yaxt="n")
axis(1,at=a,labels=a,cex.axis=0.95)
legend(12,0.3, c("English","Dutch"), lty=1:2);
dev.off()


#Frequency graphs:
b<-seq(1,7.5,0.5)

#english frequency prob:
yy<- -2.62573 + 0.38433* (b*sd(as.numeric(dataloan$subtlexzipf), na.rm=TRUE)-mean(as.numeric(dataloan$subtlexzipf), na.rm=TRUE))
pyy = 1 - 1/(1+exp(yy))
#dutch frequency prob:
zz<- -3.79927 + 0.37078*(b*sd(log10(as.numeric(dutch2$subtlex.dominant.pos.frequency)), na.rm=TRUE)-mean(log10(as.numeric(dutch2$subtlexzipf)), na.rm=TRUE))
pzz = 1 - 1/(1+exp(zz))

tiff("probability_graph_freq.tiff"); par(new=F)
plot(b,pyy, type="o",xlab="Frequency",ylab="Probability of Borrowing",xlim=c(1,7.4),ylim=c(0,0.2),xaxt="n"); 
par(new=T)
lines(b, pzz, type="o", xlab="",ylab="",lty=2,xaxt="n",yaxt="n")
atx <- axTicks(1)
axis(1,at=atx,labels=10^atx)
legend(5,0.04, c("English","Dutch"), lty=1:2);
dev.off()


