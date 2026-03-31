# APARTAT 1
ann <- read.table("A14.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1
smm <- read.table("S12.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1

(lambda.hat <- length(ann)/sum(ann))
(mu.hat <- length(smm)/sum(smm))
(rho.hat <- lambda.hat/mu.hat)

summary(ann)
sd(ann)
(cv_ann <- sd(ann_vec) / mean(ann_vec))
summary(smm)
sd(smm)
(cv_smm <- sd(smm_vec) / mean(smm_vec))

par(mfrow=c(1,2)) 
hist(ann, main="Temps entre arribades", col="lightblue")
hist(smm, main="Temps de servei", col="lightgreen")

# APARTAT 2
n <- length(ann)
m <- length(smm)

(x_menys <- qchisq(0.025,2*n)/(2*n)
x_mes <- qchisq(1-0.025,2*n)/(2*n)
c(lambda.hat*x_menys, lambda.hat, lambda.hat*x_mes)

f_menys <- qf(0.025,2*n,2*n)
f_mes <- qf(1-0.025,2*n,2*n)
c(rho.hat * f_menys, rho.hat, rho.hat * f_mes)


(apartat2 <- data.frame( n, f_menys, f_mes,
                         ef = (f_mes-f_menys)/mean(c(f_mes, f_menys))*100,
                         x_menys , x_mes,
                         ex = (x_mes-x_menys)/mean(c(x_mes, x_menys))*100))
 
#APARTAT 3
#ann /sortides
sample <- ann               
samplerate <- lambda.hat     
n <- length(sample)

hist(sample, freq=FALSE, breaks=25, col="yellow",
     main="Histograma mostra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="green",
     main="Histograma per percentils")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Observed counts", col="green")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
#Com que pvalor>0.05, es pot acceptar que la mostra és compatible amb una exponencial

#smm /entrades
sample <- smm               
samplerate <- mu.hat     
n <- length(sample)

hist(sample, freq=FALSE, breaks=25, col="yellow",
     main="Histograma mostra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="green",
     main="Histograma per percentils")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Observed counts", col="green")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
#Com que pvalor>0.05, es pot acceptar que la mostra és compatible amb una exponencial

