##Ejercicio 1

library(TeachingDemos)


energia <- c(67, 67.3, 67.8, 66.4, 67.5, 67.5, 66.6, 67.1, 66.5, 66.9)
S = sd(energia) # desviación típica de la muestra
alpha = 0.05 
n = length(energia) 

SE = S/sqrt(n); SE 

E = qt(1-alpha/2,n-1)*SE; E 

xbar = mean(energia) # media muestral
IC = xbar + c(-E,E); IC # Intervalo estimado

t.test(energia)


##Ejercicio 2

peso <- c(7.96, 7.90, 7.98, 8.01, 7.97, 8.03, 8.02, 8.04, 8.02)
alpha = 0.05
n = length(peso) # Tamaño de la muestra
sd = sd(peso) # desviación estandard de la muestra

sigma.test(peso)

#CI pata la varianza
sigma.test(peso)$conf.int

#CI pata la desviación típica
sqrt(sigma.test(peso)$conf.int)

##Ejercicio 3

x <- c(23.3, 22.5, 21.9, 21.5, 19.9, 21.3, 21.7, 23.8, 22.6, 24.7)
sigma <- 1.4
mu_0 <- 23
alpha <- 0.05

library(TeachingDemos)
z.test(x, sigma , mu = mu_0, alternative = "less")

##Ejercicio 4

x <- c(7.3, 7.1, 7.9, 7.0, 7.2)
mu_0 <- 6.8
alpha <- 0.01

t.test(x,mu = mu_0, conf.level = 1-alpha, alternative = "greater")



