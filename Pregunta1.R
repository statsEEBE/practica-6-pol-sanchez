#Practica 6 
#Estimación por intervalos 
#Ex-24
# a)
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
n <- 16
sigma <- 5
#calculamos la media muestral x barra
xbar <- mean(x)
xbar
# alpha = significacion, 1-alpha = confianza = 0.9
z_alpha_medios <- qnorm(1-0.05)
IC <- c(xbar - z_alpha_medios*5/sqrt(16),xbar + z_alpha_medios*5/sqrt(16))
IC
install.packages("BSDA")
library(BSDA)
z.test(x, sigma.x = 5, conf.level = 0.9)
#Contraste de hipotesis ; SITUACIÓN 1: H0 mu=500 (hipot. nula) ; H1 mu>500 (hipot. alternativa a probar)
#En este caso mu0 no esta en el IC, por tanto rechazamos H0, y aceptamos H1
#SITUACIÓN 2: si mu0 estuviera en el IC, aceptamos la H0 y rechazamos la H1
#Zona Crítica; imaginamos que estamos en la situación H0 cierta ; para calcular el límite de rareza:
zc <- qnorm(0.90) 
zc
#Si Zobserv es mayor que zcritc, rechazamos H0, sino, aceptamos H0
mu0 <- 500
zobs <- (xbar-mu0)/(sigma/sqrt(n))
zobs
# Cola Inferior , si mu<mu0, "less"; Cola Superior , si mu>mu0, "greater"; 2-colas, si mu!=mu0, "two.sided"; En este caso es cola superior
z.test(x, conf.level = 0.9, alternative = "greater", sigma.x = sigma, mu=mu0)
pvalor

#Caso 2; no conozco la sigma ni la mu
s <- sd(x)
n <- length(x)
m <- qt(0.99+0.005, n-1)*s/sqrt(n)
t.test(x, conf.level = 0.99)
c(xbar-m, xbar+m)
#H0: mu = mu0 = 500; H1 mu > mu0 (cola sup.)
mu0 <- 500
t.test(x, mu=mu0, alternative = "greater", conf.level = 0.99)
#Por tanto, aceptamos la H0 ya que p-value >= alpha = 0.01
#Tcritico = qt(1-alpha, n-1)
tc <- qt(0.99,n-1)
tc
tobs <- (xbar-mu0)/(s/sqrt(n))
tobs
#Por tanto acepto la H0


###
install.packages("EnvStats")
library(EnvStats)
varTest(x, conf.level = 0.95)
IC = c(sqrt(20.99068),sqrt(92.14106));IC
varTest(x, conf.level = 0.95, sigma.squared = 25, alternative = "two.sided")
# Falta prop.test
prop.test(136,400,correct = FALSE, p=0.3 , alternative = "less")
