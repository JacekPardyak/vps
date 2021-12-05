#Bouncing Algorithm - www.101computing.net/bouncing-algorithm/


res <-  data.frame(x = 30, y = 30)   
    
moveBall = function(df){    
  WIDTH=100
  HEIGHT=100
  vx = sample(c(0:10), 1)
  vy = sample(c(0:10), 1)
  
  # define field
  field <- data.frame(x0 = c(0, WIDTH, HEIGHT, 0),    # Create data frame with line-values
                      y0 = c(0, 0, WIDTH, HEIGHT),
                      x1 = c(WIDTH, HEIGHT, 0, 0),
                      y1 = c(0, WIDTH, HEIGHT, 0))
  
  #Bouncing Algorithm when the Ball hit the edge of the canvas
  x = df$x+vx
  y = df$y+vy
  if (x<0 | x>WIDTH){
    vx=-vx
    x=x+vx
  }
  if (y<0 | y>HEIGHT){
    vy=-vy
    y=y+vy
  }    
  #Draw ball  
  t <- data.frame(x = x, y = y)
  par(pty="s")
  plot(t, xlim=c(0, WIDTH), ylim=c(0, HEIGHT))
  segments(x0 = field$x0,                            # Draw multiple lines
           y0 = field$y0,
           x1 = field$x1,
           y1 = field$y1)
  return(t)
}

for(i in c(1:10)) {
  Sys.sleep(0.5)
  res = moveBall(res)}

