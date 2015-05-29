# multinomiala konfidensintervall
# exempel

library(dplyr)
library(MultinomialCI)
library(CoinMinD)

# scenarios
s01 <- rep(20, 5) * 1.5
s02 <- c(10, 20, 30, 50)

# simultana konfindensintervall
multinomialCI(s01, 0.05) # Sison and Glaz (1995)
multinomialCI(s02, 0.05)

# CoinMinD
BMDU(s01,1) # Bayesian Multinomial Dirichlet model
BMDE(s01, 1) # Bayesian Multinomial Dirichlet model
FS(s01, 0.05) # Fitzpatrick and Scott (1987)
GM(s01, 0.05) # Goodman (1965)
WS(s01, 0.05) # Wilson (1927)

# fler exempel
f03 <- c(31, 32, 37)
f16 <- c(5, 11, 84)
multinomialCI(f03, 0.05)
multinomialCI(f16, 0.05)
FS(f03, 0.05)
FS(f16, 0.05)
