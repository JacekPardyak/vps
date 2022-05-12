# retail-and-retailers-sales-time-series-collection
library(tidyverse)
library(feather)
paths = list.files(path = "./influxdb/retail-and-retailers-sales-time-series-collection/", pattern = ".csv", full.names = TRUE)
vars = list.files(path = "./influxdb/retail-and-retailers-sales-time-series-collection/", pattern = ".csv", full.names = FALSE) 
df <- tibble()
for (i in c(1:length(paths))){
  tbl <- read_csv(paths[i]) %>% select('date', 'value') %>% mutate(name = vars[i]) #%>%mutate(name = gsub(".csv", "", name))
  df <- df %>% bind_rows(tbl)
}

df <- df %>% pivot_wider(names_from = name, values_from = value) %>% fill(names(.)) %>% 
  pivot_longer(!date, names_to = "name", values_to = "value") %>% arrange(date)


path <- "./influxdb/retail-and-retailers-sales-time-series-collection.feather"
write_feather(df, path)


# -------------------------
data <- read_feather("./influxdb/retail-and-retailers-sales-time-series-collection.feather")
names <- data %>% select(name) %>% distinct() %>% pull()

nm <- names %>% nth(4)

df <- data %>% filter(name == nm)
df %>%
  ggplot() +
  aes(x = date, y = value) + 
  geom_line(alpha=0.5, show.legend = F)

#  Smoothing a Time Series
library(KernSmooth)
t <- df %>% select(date) %>% pull() %>% as.numeric()
y <- df %>% select(value) %>% pull()

gridsize <- length(y)
bw <- dpill(t, y, gridsize = gridsize)
lp <- locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize)
smt <- lp$y
df <- df %>% mutate(smooth = smt)

df %>%
  ggplot() +
  geom_line(aes(x = date, y = value), alpha=0.5) +
  geom_line(aes(x = date, y = smooth), colour = "red")



# ----------------------
#  Smoothing a Time Series with stl
t <- df %>% select(date) %>% pull() %>% as.numeric()
y <- df %>% select(value) %>% pull()
ts <- ts(y, frequency = 30)
model <- stl(ts,  s.window = 7, t.window = 50)
model$time.series %>% as_tibble() %>% select(trend) %>% pull()
plot(model)    
ts %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

plot(stl(nottem, "per"))
plot(stl(nottem, s.window = 7, t.window = 50, t.jump = 1))

?ses

library(forecast)
fcast <- holt(airmiles)
deaths.fcast <- hw(USAccDeaths,h=48)

model <- ses(ts)
model <- holt(ts)
ts <- ts(y, frequency = 3)
model <- hw(ts)
ses = model$fitted
df <- df %>% mutate(smooth = ses)
df %>%
  ggplot() +
  geom_line(aes(x = date, y = value), alpha=0.5) +
  geom_line(aes(x = date, y = smooth), colour = "red")
