library(shiny)
library(tidyverse)
runGitHub(repo = 'r-cade-games', username = 'JacekPardyak', subdir = 'pong')


tab <- table(round(rnorm(100)), round(rnorm(100)))
plot(unclass(tab))



game <- ttt_game()

m <- game$state

position = 0
position = position + 1
game$play(position)
game$state


par(mar=c(5.1, 4.1, 4.1, 4.1))
# numeric matrix
x <- matrix(runif(50), nrow=10)
plot(x)
plot(x, key=NULL)
plot(x, key=list(cex.axis=0.5, tick=FALSE))
plot(x, digits=3)
plot(x, breaks=range(x), digits=3, cex=0.6)
# logical matrix
m <- matrix(runif(50)<0.5, nrow=10)
plot(m)
plot(m, col=c("red", "blue"))
plot(m, key=NULL, digits=1)
# character matrix
s <- matrix(sample(letters[1:10], 50, replace=TRUE), nrow=10)
plot(s)
plot(s, col=topo.colors)
plot(s, digits=10)
plot(s, digits=1, col=heat.colors(5), breaks=letters[1:5])
plot(s, digits=1, col=heat.colors(5), breaks=c('a', 'c', 'e', 'g', 'i'))
# contingency table
tab <- table(round(rnorm(100)), round(rnorm(100)))
plot(unclass(tab))
# chisquare test residuals
cst <- chisq.test(apply(HairEyeColor, 1:2, sum))
col <- colorRampPalette(c("blue", "white", "red"))
plot(cst$residuals, col=col, breaks=c(-7.5,7.5))
# triangular matrix
x[upper.tri(x)] <- NA
plot(x, digit=2)
plot(x, na.print=FALSE)
plot(x, na.cell=FALSE)
