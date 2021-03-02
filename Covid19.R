#Library
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

# Data storing

#we are fetching the data from library. cases which are confirmed.

data <- covid19.data(case = 'ts-confirmed')

# to view the data fetched

View(data)

# now fetching data according to country
# choosing US firstly storing in variable USA
USA <-data %>%filter(Country.Region=='US')

## to view the data fetched in USA

View(USA)

# to transpose the data from row to column 

USA<- data.frame(t(USA))

# changing the column names to date type using columnbind "cbind"

USA <- cbind(rownames(USA),data.frame(USA,row.names = NULL))

# after conversion we can see that there is seperate column for date aswell.

# Now changing the names of the column

colnames(USA)<- c('Date','Confirmedcases')

# Removing unwanted data 

USA <- USA[-c(1:4),]

# checking the date format or we can change the date format as well


USA$Date<- ymd(USA$Date)

# checking the structure of USA

str(USA)
# changing the data type of Confirmed cases from Character to Integer

USA$Confirmedcases <- as.numeric(USA$Confirmedcases)

# Ploting two columns

qplot(Date, Confirmedcases, data = USA,
      main = 'Cases in US')
ds<-USA$Date
y<-USA$Confirmedcases

# creating the dataframe

df<-data.frame(ds,y)

#Forecasting
f<-prophet(df)

# predicting future of next 28 days

future <- make_future_dataframe(f, periods = 28)
# storing both the data in forecast
forecast <- predict(f,future)
#ploting

plot(f,forecast)

dyplot.prophet(f,forecast)

#forecast components

prophet_plot_components(f,forecast)

# model perfomance

pred<- forecast$yhat[1:162]
actual <- f$history$y
plot(actual, pred)

# adding a line with colour red

abline(lm(pred~actual),col='red')

# summary

summary(lm(pred~actual))











