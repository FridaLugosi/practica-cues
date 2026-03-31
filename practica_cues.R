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

sample <- ann
hist(sample,20)

summary(sample)                             # Es treuen estadístiques bàsiques
mm<-mean(sample)                            # mitjana
samplerate<-1/mm                            # com que la mostra prové d'una exponencial convé calcular el seu paràmetre
par(mfrow=c(1,2))                           # Es configura com serà el display. 
                                            # En aquest cas serà dues columnes 1 fila (hi podra cablre dos gràfics paral·lels)
#hist(sample,breaks=25,col="yellow",main="Equal interval length ... ")
hist(sample,freq=FALSE,breaks=25,col="yellow",main="Equal interval length ... ") # opcio freq=FALSE canvia l'escala y a densitats i no a freqüencies.
curve(dexp(x, rate = samplerate), col=2, add=TRUE) # dibuixa la corba de la distribució contra la que es fa el test
1/0.04
sequence<-seq(0,1,by=0.04)                  # Crea la sequència de punts de tall sobre el [0,1] amb distància entre punts de 0.04
sequence                                    # la mostra

# H0: Exponencial lambda = 1/16             
perdist <- qexp(sequence, rate = samplerate)           # Crea la sequència de punts de tall sobre el eix t, [0,infty] marcada per la Funció de distribció inversa de la dist. H0
perdist
perdist[length(perdist)] <- max(sample)                             # El darrer punt qe seria infinit es canvia a un valor raonable
perdist
hist(sample,freq=FALSE,breaks=perdist,col="green",main="Length defined by percentiles  ... ") # Exemple de histograma per percentils
curve(dexp(x, rate = samplerate), col=2, add=TRUE)              # S'hi solapa una corba
# Chi squared test
dsample<-cut(sample,breaks=perdist,include.lowest = T)    
dsample
table(dsample)
summary(dsample)
iobs<-as.vector(table(dsample))            # Obs in the groups defined by nb intervals-percentiles
pexp<-as.vector(rep(1/25,25))              # Expected probability for each groups: sample size/nb intervals

n <- length(sample)

iexp <- n * pexp
abline(h = n/25, col="red", lwd=2, lty=2)                           # VALOR ESPERAT D'OBSERVACIONS
iobs                                       # VALOR DE Nº D'OBSERVACIONS EN CADA INTERVAL
iexp
X2<-sum(((iobs-iexp)^2)/iexp);X2           # CÀCLCUL DE L'ESTADISTIC DE X2
1-pchisq(X2,23)                             # pvalue = P(H0)=P(Shiq(23)>X2)
chisq.test( iobs,p=pexp )                 # EXEMPLE QUE SI FUNCONA

barplot(table(dsample),main="Observed counts",col="green") # ES DIBUIXA UN DIAGRAMA DE BARRES QUE DONA IDEA DE LA REGULARITAT DEL Nº D'OBSERVACIONS DISTRIBUÏT SOBRE CADA INTERVAL
abline(h=500/25,col="red",lwd=2,lty=2)    # ES superposa una línia d'alçada Nº ESPERAT D'OBSERVACIONS PER INTERVAL
