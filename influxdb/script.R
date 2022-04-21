library("influxdbclient")
library(tidyverse)
# You can generate an API token from the "API Tokens Tab" in the UI
token = "O9QVVWu2xStTCZ79to7kCyqUIBYy9gozgP-g9iTQX2gT6dX83yqxVMjam-_s3r-38B2OQ1dfLQXsurT5-me2ow=="

client <- InfluxDBClient$new(url = "http://localhost:8086",
                             token = token,
                             org = "dom")

client

df <- read_csv("https://github.com/gouravsinghbais/Getting-Started-with-R-and-InfluxDB/raw/master/covid_data.csv")
class(df)
df %>% glimpse()

## convert date column to POSIXct
df <- df %>% 
  mutate(Date = as.POSIXct(strptime(Date, format='%Y-%m-%d')))
df %>% glimpse()
df <- as.data.frame(df)
## write data in influxDB
response <- client$write(x = df, 
                         bucket = "wiadro",
                         precision = "us",
                         measurementCol = "Cases",
                         tagCols = c("Region", "Country"),
                         fieldCols = c("Cases"),
                         timeCol = "Date")



result <- client$query('from(bucket: "wiadro") |> range(start: -3y) |> drop(columns: ["_start", "_stop"])')

result[[1]][c("time", "_value")]

## create an empty dataframe to store all the results
df1 = data.frame()
## iterate over each entry and append it in created dataframe
for (r in result){
  sub_df = r[c("time", "_value")]
  print(sub_df)
  df1 = rbind(df1, sub_df)
}

## arrange dataframe on ascending order based on time
df1 = df1[order(df1$time),]
## change the date to YYYY-MM-DD format
df1[['time']] <- as.POSIXct(strptime(df1[['time']], format='%Y-%m-%d'))
## rename column _value to Cases
colnames(df1)[2] <- "Cases"
## convert double values to integer
df1$Cases <- as.integer(df1$Cases)

df1[1, "time"]

library(forecast)
library(lubridate)
## create time series representation of data
mts <- ts(df1$Cases, start = decimal_date(ymd(df1[1, "time"])),
          frequency = 365.25 / 7)
## plotting the input data
plot(mts, xlab ="Weekly Data",
     ylab ="Total Positive Cases",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

## fit model on the data
fit <- auto.arima(mts)
## make predictions for next 5 days
forecast(fit, 5)
## plot predictions
plot(forecast(fit, 5), xlab ="Weekly Data",
     ylab ="Total Positive Cases",
     main ="COVID-19 Pandemic", col.main ="darkgreen")

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

