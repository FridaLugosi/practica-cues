# ===================================================================================
# FICHERO A14 Y S12
# ===================================================================================
# APARTADO 1
ann <- read.table("A14.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1
smm <- read.table("S12.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1

n <- length(ann)
m <- length(smm)
# Se estima lambda (tasa de llegadas) y mu ( tasa de servicios) por sus estimadores de máxima verosimilitud
(lambda.hat <- n/sum(ann))
(mu.hat <- m/sum(smm))
(rho.hat <- lambda.hat/mu.hat)

library(flextable)
library(dplyr)
library(officer)

# Estadísticas basicas
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
  add_header_lines(values = "Estadísticas básicas de la muestra ann") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Resumen descriptivo de la muestra ann.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadisticas_ann.png")

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
  add_header_lines(values = "Estadísticas básicas de la muestra smm") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Resumen descriptivo de la muestra smm.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadisticas_smm.png")

par(mfrow=c(1,2)) 
hist(ann, main="Distribución del tiempo entre llegadas", col="lightblue")
hist(smm, main="Distribución del tiempo de servicio", col="lightgreen")

#-------------------------------
# APARTADO 2
(tabla_IC <- data.frame( n = length(ann),
                         f_menys = qf(0.025,2*n,2*n),
                         f_mes = qf(1-0.025,2*n,2*n),
                         ef = (qf(1-0.025,2*n,2*n)-qf(0.025,2*n,2*n))/mean(c(qf(1-0.025,2*n,2*n), qf(0.025,2*n,2*n)))*100,
                         x_menys = qchisq(0.025,2*m)/(2*m),
                         x_mes = qchisq(1-0.025,2*m)/(2*m),
                         ex = (qchisq(1-0.025,2*m,2*m)-qchisq(0.025,2*m,2*m))/mean(c(qchisq(1-0.025,2*m,2*m), qchisq(0.025,2*m,2*m)))*100))

# Crear flextable
ft_ann <- flextable(tabla_IC) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Tabla para obtener el IC al 95% de amplitud") %>% 
  bold(part = "header", i = 1) %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "IC.png")


# Intervalos de confianza
IC_lambda <- c(lambda.hat * tabla_IC$x_menys,
               lambda.hat * tabla_IC$x_mes)

IC_mu <- c(mu.hat * tabla_IC$x_menys,
           mu.hat * tabla_IC$x_mes)

IC_rho <- c(rho.hat * tabla_IC$f_menys,
            rho.hat * tabla_IC$f_mes)

# Crear data frame
IC_df <- data.frame(
  Paràmetre = c("λ (lambda)", "μ (mu)", "ρ (rho)"),
  Límit_inferior = c(IC_lambda[1], IC_mu[1], IC_rho[1]),
  Límit_superior = c(IC_lambda[2], IC_mu[2], IC_rho[2])
)

# Redondear
IC_df[,2:3] <- round(IC_df[,2:3], 4)

# Crear flextable
ft_IC <- flextable(IC_df) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Intervalos de confianza al 95%") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Intervalos de confianza para λ, μ i ρ.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_IC, path = "IC_parametros.png")

#-------------------------------
#APARTADO 3

# ann / llegadas
sample <- ann               
samplerate <- lambda.hat     
n <- length(sample)

par(mfrow = c(3,1))
hist(sample, freq=FALSE, breaks=25, col="lightyellow",
     main="Histograma muestra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="lightgreen",
     main="Histograma por percentiles")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Recuentos observados", col="lightblue")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
# Como el pvalor>0.05, se acpta que la muestra es compatible con una exponencial

# smm / servicio
sample <- smm               
samplerate <- mu.hat     
n <- length(sample)

hist(sample, freq=FALSE, breaks=25, col="lightyellow",
     main="Histograma muestra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="lightgreen",
     main="Histograma por percentiles")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Recuentos observados", col="lightblue")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
# Como el pvalor>0.05, se acpta que la muestra es compatible con una exponencial

# ===================================================================================
# FICHERO A9 Y S8
# ===================================================================================
# APARTADO 1
ann <- read.table("A9.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1
smm <- read.table("S8.DAT", header = FALSE, dec = ",", fileEncoding = "UTF-16LE")$V1

n <- length(ann)
m <- length(smm)
# Se estima lambda (tasa de llegadas) y mu ( tasa de servicios) por sus estimadores de máxima verosimilitud
(lambda.hat <- n/sum(ann))
(mu.hat <- m/sum(smm))
(rho.hat <- lambda.hat/mu.hat)

library(flextable)
library(dplyr)
library(officer)

# Estadísticas basicas
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
  add_header_lines(values = "Estadísticas básicas de la muestra ann") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Resumen descriptivo de la muestra ann.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadisticas_ann.png")

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
  add_header_lines(values = "Estadísticas básicas de la muestra smm") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Resumen descriptivo de la muestra smm.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "estadisticas_smm.png")

par(mfrow=c(1,2)) 
hist(ann, main="Distribución del tiempo entre llegadas", col="lightblue")
hist(smm, main="Distribución del tiempo de servicio", col="lightgreen")

#-------------------------------
# APARTADO 2
(tabla_IC <- data.frame( n = length(ann),
                         f_menys = qf(0.025,2*n,2*n),
                         f_mes = qf(1-0.025,2*n,2*n),
                         ef = (qf(1-0.025,2*n,2*n)-qf(0.025,2*n,2*n))/mean(c(qf(1-0.025,2*n,2*n), qf(0.025,2*n,2*n)))*100,
                         x_menys = qchisq(0.025,2*m)/(2*m),
                         x_mes = qchisq(1-0.025,2*m)/(2*m),
                         ex = (qchisq(1-0.025,2*m,2*m)-qchisq(0.025,2*m,2*m))/mean(c(qchisq(1-0.025,2*m,2*m), qchisq(0.025,2*m,2*m)))*100))

# Crear flextable
ft_ann <- flextable(tabla_IC) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Tabla para obtener el IC al 95% de amplitud") %>% 
  bold(part = "header", i = 1) %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_ann, path = "IC.png")


# Intervalos de confianza
IC_lambda <- c(lambda.hat * tabla_IC$x_menys,
               lambda.hat * tabla_IC$x_mes)

IC_mu <- c(mu.hat * tabla_IC$x_menys,
           mu.hat * tabla_IC$x_mes)

IC_rho <- c(rho.hat * tabla_IC$f_menys,
            rho.hat * tabla_IC$f_mes)

# Crear data frame
IC_df <- data.frame(
  Paràmetre = c("λ (lambda)", "μ (mu)", "ρ (rho)"),
  Límit_inferior = c(IC_lambda[1], IC_mu[1], IC_rho[1]),
  Límit_superior = c(IC_lambda[2], IC_mu[2], IC_rho[2])
)

# Redondear
IC_df[,2:3] <- round(IC_df[,2:3], 4)

# Crear flextable
ft_IC <- flextable(IC_df) %>% 
  theme_vanilla() %>% 
  add_header_lines(values = "Intervalos de confianza al 95%") %>% 
  bold(part = "header", i = 1) %>% 
  add_footer_lines(values = "Tabla: Intervalos de confianza para λ, μ i ρ.") %>% 
  italic(part = "footer") %>% 
  fontsize(size = 10, part = "footer") %>% 
  autofit()

# Exportar como imagen
save_as_image(ft_IC, path = "IC_parametros.png")

#-------------------------------
#APARTADO 3

# ann / llegadas
sample <- ann               
samplerate <- lambda.hat     
n <- length(sample)

par(mfrow = c(3,1))
hist(sample, freq=FALSE, breaks=25, col="lightyellow",
     main="Histograma muestra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="lightgreen",
     main="Histograma por percentiles")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Recuentos observados", col="lightblue")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
# Como el pvalor>0.05, se acpta que la muestra es compatible con una exponencial

# smm / servicio
sample <- smm               
samplerate <- mu.hat     
n <- length(sample)

hist(sample, freq=FALSE, breaks=25, col="lightyellow",
     main="Histograma muestra")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

sequence <- seq(0, 1, by=0.04)
perdist <- qexp(sequence, rate = samplerate)
perdist[length(perdist)] <- max(sample)

hist(sample, freq=FALSE, breaks=perdist, col="lightgreen",
     main="Histograma por percentiles")
curve(dexp(x, rate = samplerate), col=2, add=TRUE)

dsample <- cut(sample, breaks=perdist, include.lowest=TRUE)
iobs <- as.vector(table(dsample))

k <- 25
pexp <- rep(1/k, k)
iexp <- n * pexp

X2 <- sum((iobs - iexp)^2 / iexp)
gl <- k - 1 - 1
pvalor <- 1 - pchisq(X2, gl)

barplot(iobs, main="Recuentos observados", col="lightblue")
abline(h=n/k, col="red", lwd=2, lty=2)

pvalor
chisq.test(iobs, p=pexp)
# Como el pvalor>0.05, se acpta que la muestra es compatible con una exponencial
