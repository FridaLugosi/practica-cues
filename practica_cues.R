# APARTAT 1
ann <- read.table("A14.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1
smm <- read.table("S12.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1

(lambda.hat <- length(ann)/sum(ann))
(mu.hat <- length(smm)/sum(smm))
(rho.hat <- lambda.hat/mu.hat)

library(flextable)
library(dplyr)
library(officer)

# Calcular estadísticas
resumen_ann <- summary(ann)
sd_ann <- sd(ann)
cv_ann <- sd_ann / mean(ann)

# Convertir a data frame
stats_ann <- data.frame(
  Estadística = c(
    "Mínimo",
    "1er cuartil",
    "Mediana",
    "Media",
    "3er cuartil",
    "Máximo",
    "Desviación estándar",
    "Coeficiente de variación"
  ),
  Valor = c(
    resumen_ann["Min."],
    resumen_ann["1st Qu."],
    resumen_ann["Median"],
    resumen_ann["Mean"],
    resumen_ann["3rd Qu."],
    resumen_ann["Max."],
    sd_ann,
    cv_ann
  )
)

# Crear flextable
ft_ann <- flextable(stats_ann) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Estadístiques bàsiques de la mostra ann") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Taula: Resum descriptiu de la mostra ann.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadistiques_ann.png")

# Calcular estadísticas
resumen_smm <- summary(smm)
sd_smm <- sd(smm)
cv_smm <- sd_smm / mean(smm)

# Convertir a data frame
stats_smm <- data.frame(
  Estadística = c(
    "Mínimo",
    "1er cuartil",
    "Mediana",
    "Media",
    "3er cuartil",
    "Máximo",
    "Desviación estándar",
    "Coeficiente de variación"
  ),
  Valor = c(
    resumen_smm["Min."],
    resumen_smm["1st Qu."],
    resumen_smm["Median"],
    resumen_smm["Mean"],
    resumen_smm["3rd Qu."],
    resumen_smm["Max."],
    sd_smm,
    cv_smm
  )
)

# Crear flextable
ft_ann <- flextable(stats_smm) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Estadístiques bàsiques de la mostra smm") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Taula: Resum descriptiu de la mostra smm.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadistiques_smm.png")

#-------------------------------

par(mfrow=c(1,2)) 
hist(ann, main="Temps entre arribades", col="lightblue")
hist(smm, main="Temps de servei", col="lightgreen")

# APARTAT 2
n <- length(ann)
m <- length(smm)

(apartat2 <- data.frame( n = length(ann),
                         f_menys = qf(0.025,2*n,2*n),
                         f_mes = qf(1-0.025,2*n,2*n),
                         ef = (qf(1-0.025,2*n,2*n)-qf(0.025,2*n,2*n))/mean(c(qf(1-0.025,2*n,2*n), qf(0.025,2*n,2*n)))*100,
                         x_menys = qchisq(0.025,2*n)/(2*n),
                         x_mes = qchisq(1-0.025,2*n)/(2*n),
                         ex = (qchisq(1-0.025,2*n,2*n)-qchisq(0.025,2*n,2*n))/mean(c(qchisq(1-0.025,2*n,2*n), qchisq(0.025,2*n,2*n)))*100))

# Crear flextable
ft_ann <- flextable(apartat2) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Taula per obtenir IC del 95% d'amplitud") %>% 
  bold(part = "header", i = 1) %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "IC.png")


# Intervalos de confianza
IC_lambda <- c(lambda.hat * apartat2$x_menys,
               lambda.hat * apartat2$x_mes)

IC_mu <- c(mu.hat * apartat2$x_menys,
           mu.hat * apartat2$x_mes)

IC_rho <- c(rho.hat * apartat2$f_menys,
            rho.hat * apartat2$f_mes)

# Crear data frame
IC_df <- data.frame(
  Paràmetre = c("λ (lambda)", "μ (mu)", "ρ (rho)"),
  Límit_inferior = c(IC_lambda[1], IC_mu[1], IC_rho[1]),
  Límit_superior = c(IC_lambda[2], IC_mu[2], IC_rho[2])
)

# (Opcional) redondear
IC_df[,2:3] <- round(IC_df[,2:3], 4)

# Crear flextable
ft_IC <- flextable(IC_df) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Intervals de confiança al 95%") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Taula: Intervals de confiança per λ, μ i ρ.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_IC, path = "IC_parametres.png")

 
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

