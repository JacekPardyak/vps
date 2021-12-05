library(tidyverse)

set.seed(1)

n <- 100
x <- cumsum(sample(c(-1, 1), n, TRUE))
df <- data.frame(x = x, index = c(1:length(x)))

for (i in c(1:n)) {
  Sys.sleep(0.5)
  p <- df %>% filter(index == i) %>% ggplot() + 
    aes(x = index, 
        y = x,
        xmin = min(df['index']),
        xmax = max(df['index']),
        ymin = min(df['x']),
        ymax = max(df['x'])) +
    geom_point() + 
    theme_void()
  print(p)
}


# ============================================================================
HEIGHT = 300
WIDTH = 300
df <- data.frame(x = 150, y = 150)
i = 1 
while (i < 100) {
  Sys.sleep(0.5)
  i = i + 1
  print(i)
  df[i, 'x'] = df[i-1, 'x'] + 1
  df[i, 'y'] = df[i-1, 'y'] + 1
  
  p <- df %>% filter(row.names(df) == i) %>% ggplot() + 
    aes(x = df[i, 'x'], 
        y = df[i, 'y'],
        xmin = 0,
        xmax = WIDTH,
        ymin = 0,
        ymax = HEIGHT) +
    geom_point() + 
    theme_void()
  print(p)
}

# ============================================================================
HEIGHT = 300
WIDTH = 300
df <- data.frame(x = 150, y = 150)
i = 1 
while (i < 100) {
  Sys.sleep(0.5)
  i = i + 1
  print(i)
  df[i, 'x'] = df[i-1, 'x'] + 1
  df[i, 'y'] = df[i-1, 'y'] + 1
  
  par(mar = rep(1, 4), bg = "black")
  plot.new()
  plot.window(xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
  
  lines(
    x = c(WIDTH, WIDTH, 0, 0),
    y = c(0, HEIGHT, HEIGHT, 0),
    type = "l",
    lwd = 5,
    col = "white"
  )
  
  points(
    x = df[i, 'x'], 
    y = df[i, 'y'],
    pch = 15,
    cex = 5,
    col = "white"
  )
}

