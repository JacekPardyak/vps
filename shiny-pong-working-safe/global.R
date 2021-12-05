library(shiny)
library(beepr)
library(sets)
HEIGHT = 300
WIDTH = 300 
SIZE = 30 
DELAY = 1000

library(simecol)
ballode <- new("odeModel",
               main = function (t, y, parms) {
                 dy1 <- y[2]
                 dy2 <- -9.8
                 list(c(dy1, dy2))   },
               parms = 0,
               times = seq(0, 37.5, 0.25),
               init = c(height = 20, v = 0),
               solver = function(t, y, func, parms) {
                 root <- function(t, y, parms) y[1]
                 event <- function(t, y, parms) {
                   y[1] <- 0
                   y[2] <- -0.9 * y[2]
                   return(y)   }
                 lsodar(t, y, func, parms = NULL,events = list(func = event, 
                                                               root = TRUE), rootfun = root)}
)
ballode <- sim(ballode)
# Animation
o <- out(ballode)
