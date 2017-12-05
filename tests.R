library(copula)

C3 <- onacopula("A", C(0.2, 1, C(0.8, 2:3)))


library(HAC)

plot(nacopula2hac(C3))

theta <- copJoe@iTau(0.5)
C10 <- onacopula("J",C(theta,1:10))
C10. <- onacopulaL("J",list(theta,1:10))
stopifnot(identical(C10, C10.),
          identical(nac2list(C10), list(theta, 1:10)))
plot(nacopula2hac(C10))


theta0 <- copGumbel@iTau(.2)
theta1 <- copGumbel@iTau(.5)
C3.2 <- onacopula("G", C(theta0, 1, C(theta1, c(2,3))))
plot(nacopula2hac(C3.2))


str(NAlis <- list(theta0, 1, list(list(theta1, c(2,3)))))
C3. <- onacopulaL("Gumbel", NAlis)
stopifnot(identical(C3, C3.))
plot(nacopula2hac(C3.2))



# Type de famille de HAC homgènes dispo dans le package : 
#  "AMH" (or simply "A"), "Clayton" ("C"), "Frank" ("F"), "Gumbel" ("G"), or "Joe" ("J")


data("finData")
cor(finData,method="kendall")




# Pour estimer une cpula archimédienne hierarchique, on peut : 

# 1° Donner la matrice de tau de kendall, celle de rho de spearman et permetre a l'utilisateur de choisir entre les deux. 
# Donner la liste des familles disponibles et permettre a l'utilisateur de choisir. 
# plotter les dataset bivariés pour voir ce qu'il ce passe.
estimate.copula(finData,method=1)
estimate.copula(finData,method=2)
estimate.copula(finData,method=3)
estimate.copula(finData,method=4)



























library(copula)
library(dplyr)
library(psych)
library(VineCopula)
library(fitdistrplus)

normC <- gumbelCopula(param=5,dim = 2)

data <- rCopula(n=10000, copula = normC)

# check my copula : 
#BiCopSelect(data[,1],data[,2], presel=TRUE)
pairs.panels(data,method="kendall")

data[,1] <- qexp(data[,1])
data[,2] <- qnorm(data[,2])


# re-check my copula :
u <- pobs(data)[,1]
v <- pobs(data)[,2]
#BiCopSelect(u,v,presel=TRUE)
pairs.panels(data,method="kendall")




# Plot de la convergence de la dépendance des extremes a gauche : 
x <- seq(from = 0,to = 1,length=10000)
y <- x %>%
  lapply(function(x) pCopula(c(x,x),normC)/x) %>%
  unlist()

plot(x,y)







