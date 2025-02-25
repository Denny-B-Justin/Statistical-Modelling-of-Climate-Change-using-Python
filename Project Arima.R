# SAN FRANCISCO

datas<-read.csv(file.choose(),header= T, sep =",")
datas
data<-select(datas,TAVG)
date<-select(datas,DATE)
data
summary(data)

plot(data$TAVG)
data1<-na.omit(data)
summary(data1)
print (nrow(data1))
plot(data1$TAVG)    # Precipitation Plot
number_of_zeros <- sum(data1 == 0) # Normal Days
print(number_of_zeros)

cumulativedata <- cumsum(data1$TAVG)
plot(cumulativedata)

TS<-ts(cumulativedata,start=1990,frequency=365)   #Time Series
plot(TS)

TS_data <- ts(data1,start=1990,frequency=365)
plot (TS_data)
#Auto Correlation
precipitation_time_series <- ts(data1$PRCP, frequency = 1)
acf_prcp <- acf(precipitation_time_series, main = "Autocorrelation Function for Precipitation")
plot(acf_prcp)

autocorrelation_table <- data.frame(
  Lag = acf_prcp$lag,
  Autocorrelation = acf_prcp$acf)
print (autocorrelation_table)

# Fitting in ARIMA - Data
fitarima1<-auto.arima(TS_data)
fitarima1
pred<-forecast(fitarima1,365*10)
plot(pred)

#Fitting in ARIMA - Cumulative
fitarima2<-auto.arima(TS)
fitarima2
pred<-forecast(fitarima2,365*10)
plot(pred)




D<-decompose(TS)
plot(D)

# Remove Seasonality
ds<-seasadj(D)
plot(ds)
dsdc<-decompose(ds)
plot(dsdc)
# Removing trend of DE seasonal data
dds<-diff.default(ds,lag=1,differences = 1)
plot(dds)
dds



# Define the intervals
interval_breaks <- seq(0, 6, by = 0.1)  # Define interval breaks from 0 to 1 at increments of 0.2
options(repr.plot.width = 10, repr.plot.height = 6) 
# Cut the 'PRCP' data into intervals
data$Interval <- cut(data$PRCP, breaks = interval_breaks, include.lowest = TRUE, right = FALSE)

# Calculate the exceedance probability in each interval
exceedance_probability <- data %>%
  group_by(Interval) %>%
  summarise(Probability = n() / nrow(data))

# Display the interval probabilities
print(exceedance_probability)
plot (exceedance_probability)
kable(exceedance_probability, caption = "Probability of Occurrence in Each Interval")
cumulative_prob <- cumsum(exceedance_probability$Probability)
plot(cumulative_prob)

#Return Period
reciprocal_probabilities <- interval_probabilities %>%
  mutate(Reciprocal_Probability = 1 / Probability)
kable(reciprocal_probabilities, caption = "Table")

  
  
  # THE END
