# pyglet 
from pyglet import image, window, clock, app

width = 500
height = 400
win = window.Window(width, height)
ballimg = image.load('ball.png')
backgroundimg = image.load('background.png')
#clock.set_fps_limit(60)
app.run()
#schedule_interval(60)

radius = ballimg.width/2

x = [width / 2, height / 2]
v = [5, 5]
f = [0, 0]
dt = 1

while not win.has_exit: 
  win.dispatch_events()
  v = v + dt * f
  x = x + dt * v
  
  # check collision
  if x[0] < 0 + radius or x[0] > width - radius:
    V[0] = -v[0]
  
  if x[1] < 0 + radius or x[1] > height - radius:
    V[1] = -v[1]
    
  # update 
  win.clear()
  backgroundimg.blit(0, 0)
  ballimg.blit(x[0] - radius, x[1] - radius)
  clock.tick()
  win.flip()
