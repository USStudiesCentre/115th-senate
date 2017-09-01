################################################
## read roll call data for a US Senate
##
## simon jackman
## june 18 2015
################################################
library(pscl)

## load the 109th Senate roll calls
data(s109)

## fit 1 dimensional ideal point model
## 2 parameter IRT model
id1 <- ideal(s109,
             maxiter=15E3,
             thin=50,
             normalize=TRUE,
             verbose=TRUE)


summary(id1)

plot(id1)





