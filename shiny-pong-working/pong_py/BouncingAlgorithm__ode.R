#------------------------------------------------------------------
# Bouncing ball simulation using simecol
# Thomas Petzoldt - 04/09/2010
#------------------------------------------------------------------
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
#png("ball%04d.png", width=300, height=300)
for (i in 1:length(times(ballode))){
  Sys.sleep(0.25)
  plot(1, o[i, 2], main="", ylab="", xlab="", xaxt="n",
       col="red", cex=2, pch=16, ylim=c(0,20))
}
#graphics.off()
#Convert the *.png image sequence into an animate .gif file
#(Note: requires the ImageMagick command line programs)
#system("convert -delay 5 *.png bouncing_ball.gif")
