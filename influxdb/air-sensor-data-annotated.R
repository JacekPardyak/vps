library(influxdbclient)
library(tidyverse)
# You can generate an API token from the "API Tokens Tab" in the UI
token = "O9QVVWu2xStTCZ79to7kCyqUIBYy9gozgP-g9iTQX2gT6dX83yqxVMjam-_s3r-38B2OQ1dfLQXsurT5-me2ow=="

client <- InfluxDBClient$new(url = "http://localhost:8086",
                             token = token,
                             org = "dom")

client

result <- client$query('from(bucket: "sample_data") |> range(start: -1y) |> drop(columns: ["_start", "_stop"])')
df <- result[[1]]
for (i in c(2:length(result))) {
  tmp <- result[[i]]
  df <- df %>% bind_rows(tmp)
}
df <- df %>% rename('field' = '_field') %>% rename('value' = '_value') %>%
  select(one_of('field', 'sensor_id', 'time', 'value'))
df %>% select(field) %>% table()

# plot co
df %>% filter(field == 'co') %>%
  ggplot() +
  aes(x = time, y = value, colour = sensor_id) +
  geom_line()
# plot humidity 
df %>% filter(field == 'humidity') %>%
  ggplot() +
  aes(x = time, y = value, colour = sensor_id) +
  geom_line()
# plot temperature 
df %>% filter(field == 'temperature') %>%
  ggplot() +
  aes(x = time, y = value, colour = sensor_id) +
  geom_line()

library(forecast)
library(lubridate)
df_ts <- df %>% 
  filter(field == 'temperature') %>% 
  filter(sensor_id == 'TLM0100')

## create time series representation of data
mts <- ts(df_ts$value, start = df_ts$time[1],
          frequency = 1)
## plotting the input data
plot(mts, xlab ="Time",
     ylab ="Temperature",
     main ="Temperature of the selected sensor",
     col.main ="darkgreen")

## fit model on the data
fit <- auto.arima(mts)
## make predictions for next 5 days
forecast(fit, 5)
## plot predictions
plot(forecast(fit, 5), xlab ="Weekly Data",
     ylab ="Temperature",
     main ="Temperature forecast", col.main ="darkgreen")

#bucket - wiadro
#haslo - OpenComPBX1
# Refrences:
# https://thenewstack.io/getting-started-with-r-and-influxdb/
# https://docs.influxdata.com/influxdb/v2.1/install/?t=Windows
# Data:
# https://github.com/influxdata/community-templates/blob/master/sample-data/sample-data.yml

# installationŁ
# - download
# - unzip and start .exe

