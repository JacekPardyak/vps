library(beepr)
# court definition
window.height = 480
window.width = 640

# ball definition
self.width = 20
self.height = 20

# start 

self.x = runif(1) * (window.width - self.width)
self.y = runif(1) * (window.height - self.height)

# time definition
i = 0
# plot just for tests
par(mar = rep(1, 4), bg = "black")
plot.new()
plot.window(xlim = c(0, window.width), ylim = c(0, window.height))

for(dt in seq(from = 0, to = 100, by = 1)){
  Sys.sleep(1/3)
  i = i + 1

# move definition
self.dx = (runif(1) - 0.5) 
self.dy = (runif(1) - 0.5) 

# collision 
if (self.x <= 0 | self.x + self.width >= window.width){
  self.dx = -1 * self.dx
 # beep(1)
}
  
if (self.y <= 0 | self.y + self.height >= window.height){
  self.dy = -1 * self.dy
 # beep(1)
}

self.x = self.x + self.dx * dt
self.y = self.y + self.dy * dt

self.x = min(max(self.x, 0), window.width - self.width)
self.y = min(max(self.y, 0), window.height - self.height)
             

# plot rendering

#par(mar = rep(1, 4), bg = "black")
#plot.new()
#plot.window(xlim = c(0, window.width), ylim = c(0, window.height))

# court
lines(
  x = c(0, window.width, window.width, 0, 0),
  y = c(0, 0, window.height, window.height, 0),
  type = "l",
  lwd = 5,
  col = "white"
)

# ball
points(
  x = self.x, 
  y = self.y,
  pch = 19, 
  col = "white", 
  cex = 3
)

text(
  x = self.x, 
  y = self.y,
  labels = i, 
  col = "red" #,  cex = 3
)

}



