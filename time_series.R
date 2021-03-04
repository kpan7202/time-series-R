# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import packages
library(tidyverse)
library(stringr)
library(ggplot2)
library(forecast)

# Import datasets
data_train_feature = read_csv("dengue_features_train.csv")
data_train_label = read_csv("dengue_labels_train.csv")
data_pop_iq = read_csv("Iquitos_Population_Data.csv")
data_pop_sj = read_csv("San_Juan_Population_Data_imputed.csv")

# Drop any entries before 2000 since sj only has data after 2000
#data_train_feature = data_train_feature[data_train_feature$year >= 2001, ]
#data_train_feature = data_train_feature[data_train_feature$year <= 2007, ]

# Join population table into data_train_feature
data_pop_iq$city = 'iq'
data_pop_sj$city = 'sj'
data_train_feature = merge(x = data_train_feature, y = data_pop_iq, by.x = c("year", "city"), by.y = c("Year", "city"), all.x = TRUE)
data_train_feature = merge(x = data_train_feature, y = data_pop_sj, by.x = c("year", "city"), by.y = c("Year", "city"), all.x = TRUE)

#Combine to one column
data_train_feature$Estimated_population.x = replace_na(data_train_feature$Estimated_population.x, 0)
data_train_feature$Estimated_population.y = replace_na(data_train_feature$Estimated_population.y, 0)
data_train_feature = mutate(data_train_feature, population = Estimated_population.x + Estimated_population.y)
data_train_feature = subset(data_train_feature, select = -c(Estimated_population.x,Estimated_population.y) )

# Fill forward missing values
data_train_feature = arrange(data_train_feature, city, year, weekofyear)
data_train_feature = data_train_feature %>%  group_by(city) %>% 
  fill(
    ndvi_ne, 
    ndvi_nw, 
    ndvi_se, 
    ndvi_sw,
    precipitation_amt_mm,
    reanalysis_air_temp_k,
    reanalysis_avg_temp_k,
    reanalysis_dew_point_temp_k,
    reanalysis_max_air_temp_k,
    reanalysis_min_air_temp_k,
    reanalysis_precip_amt_kg_per_m2,
    reanalysis_relative_humidity_percent,
    reanalysis_sat_precip_amt_mm,
    reanalysis_specific_humidity_g_per_kg,
    reanalysis_tdtr_k,
    station_avg_temp_c,
    station_diur_temp_rng_c,
    station_max_temp_c,
    station_min_temp_c,
    station_precip_mm
    )

#Create lag variables, to do correlation analysis on lag variables
data_train_feature = data_train_feature %>%  group_by(city) %>% mutate(
  prev_ndvi_ne = lag(ndvi_ne, order_by = weekofyear),
  prev_ndvi_nw = lag(ndvi_nw, order_by = weekofyear),
  prev_ndvi_se = lag(ndvi_se, order_by = weekofyear),
  prev_ndvi_sw = lag(ndvi_sw, order_by = weekofyear),
  
  prev_precipitation_amt_mm = lag(precipitation_amt_mm, order_by = weekofyear),
  prev_reanalysis_air_temp_k = lag(reanalysis_air_temp_k, order_by = weekofyear),
  prev_reanalysis_avg_temp_k = lag(reanalysis_avg_temp_k, order_by = weekofyear),
  prev_reanalysis_dew_point_temp_k = lag(reanalysis_dew_point_temp_k, order_by = weekofyear),
  prev_reanalysis_max_air_temp_k = lag(reanalysis_max_air_temp_k, order_by = weekofyear),
  prev_reanalysis_min_air_temp_k = lag(reanalysis_min_air_temp_k, order_by = weekofyear),
  prev_reanalysis_precip_amt_kg_per_m2 = lag(reanalysis_precip_amt_kg_per_m2, order_by = weekofyear),
  prev_reanalysis_relative_humidity_percent = lag(reanalysis_relative_humidity_percent, order_by = weekofyear),
  prev_reanalysis_sat_precip_amt_mm = lag(reanalysis_sat_precip_amt_mm, order_by = weekofyear),
  prev_reanalysis_specific_humidity_g_per_kg = lag(reanalysis_specific_humidity_g_per_kg, order_by = weekofyear),
  prev_reanalysis_tdtr_k = lag(reanalysis_tdtr_k, order_by = weekofyear),
  
  prev_station_avg_temp_c = lag(station_avg_temp_c, order_by = weekofyear),
  prev_station_diur_temp_rng_c = lag(station_diur_temp_rng_c, order_by = weekofyear),
  prev_station_max_temp_c = lag(station_max_temp_c, order_by = weekofyear),
  prev_station_min_temp_c = lag(station_min_temp_c, order_by = weekofyear),
  prev_station_precip_mm = lag(station_precip_mm, order_by = weekofyear),
  )

#Start of Time Forecasting
data_train_feature <- as.data.frame(data_train_feature)
data_train_label <- as.data.frame(data_train_label)
data_train_total <- merge(data_train_feature, data_train_label, by = c("city","weekofyear","year"))
data_train_total <- data_train_total[order(as.Date(data_train_total$week_start_date)),]

#San Juan Forecasting
#Validation data for San Juan - 2007
data_train_sj <- data_train_total[data_train_total$city == "sj",]
data_validation_sj <- data_train_total[data_train_total$city == "sj",]
data_validation_sj <- data_validation_sj[data_validation_sj$year == 2007,]
data_validation_sj = na.omit(data_validation_sj)

#Plotting the total cases as a Time Series after sorting the dataframe based on date
data_validation_sj <- data_validation_sj[order(as.Date(data_validation_sj$week_start_date)),]
validation_casesTS_sj <- ts(data_validation_sj$total_cases, frequency=52, start=c(2007,1))

#Training data from 1990 to 2005
data_train_sj <- data_train_sj[data_train_sj$year < 2007,]

#Total cases over time for San Juan
ggplot(data_train_sj, aes(x=week_start_date, y=total_cases)) + geom_line()

#Plotting the total cases as a Time Series after sorting the dataframe based on date
data_train_sj <- data_train_sj[order(as.Date(data_train_sj$week_start_date)),]
train_casesTS_sj <- ts(data_train_sj$total_cases, frequency = 52, start = c(1990,18))

#decompose the San Juan total cases time series
casesTS_sj_decompose <- decompose(train_casesTS_sj)
plot(casesTS_sj_decompose)
seasonplot(train_casesTS_sj, col=terrain.colors(6))

layout(matrix(c(1,2,3,4),2,2,byrow=T))

#Seasonal Naive Prediction
naive_sj = snaive (train_casesTS_sj, h=length(data_validation_sj$total_cases))
plot(naive_sj)

#Forecasts of STL objects are obtained by applying a non-seasonal forecasting method to the seasonally adjusted data
#STLF - Seasonal and Trend decomposition using Loess Forecasting model
stlf_model_sj = stlf(train_casesTS_sj, allow.multiplicative.trend = TRUE, h=length(data_validation_sj$total_cases))
plot(stlf_model_sj)

#Triple exponential Holt Winters- models level, trend, and seasonal components
triple_fit <- HoltWinters(train_casesTS_sj)
holt_winters_sj <- (forecast(triple_fit,h=length(data_validation_sj$total_cases)))
plot(holt_winters_sj)

#TBATS
#Exponential Smoothing State Space Model with Box-Cox Transformation
tbats_model_sj = tbats(train_casesTS_sj,seasonal.periods=52)
tbats_forecast_sj = forecast(tbats_model_sj, h=length(data_validation_sj$total_cases))
plot(tbats_forecast_sj)

accuracy(naive_sj$mean, validation_casesTS_sj)
accuracy(stlf_model_sj$mean, validation_casesTS_sj)
accuracy(holt_winters_sj$mean, validation_casesTS_sj)
accuracy(tbats_forecast_sj$mean, validation_casesTS_sj)

#Appending all the forecast time series into one dataframe for plotting
Seasonal_Naive_forecast_sj_df = data.frame(Seasonal_Naive=as.matrix(naive_sj$mean), date=time(naive_sj$mean))
STLF_forecast_sj_df = data.frame(STLF=as.matrix(stlf_model_sj$mean), date=time(stlf_model_sj$mean))
HW_forecast_sj_df = data.frame(Holt_Winters=as.matrix(holt_winters_sj$mean), date=time(holt_winters_sj$mean))
TBATS_forecast_sj_df = data.frame(TBATS=as.matrix(tbats_forecast_sj$mean), date=time(tbats_forecast_sj$mean))
validation_casesTS_sj_df = data.frame(Validation=as.matrix(validation_casesTS_sj), date=time(validation_casesTS_sj))
collated_forecast_sj_df = merge(Seasonal_Naive_forecast_sj_df, STLF_forecast_sj_df)
collated_forecast_sj_df = merge(collated_forecast_sj_df, HW_forecast_sj_df)
collated_forecast_sj_df = merge(collated_forecast_sj_df,TBATS_forecast_sj_df)
collated_forecast_sj_df = merge(collated_forecast_sj_df,validation_casesTS_sj_df)


zero_values = function(x)
{
  if (x < 0) {0}
  else {x}
}

#Change negative cases to zero
collated_forecast_sj_df$Seasonal_Naive <- sapply(collated_forecast_sj_df$Seasonal_Naive,zero_values)
collated_forecast_sj_df$STLF <- sapply(collated_forecast_sj_df$STLF,zero_values)
collated_forecast_sj_df$Holt_Winters<- sapply(collated_forecast_sj_df$Holt_Winters,zero_values)
collated_forecast_sj_df$TBATS <- sapply(collated_forecast_sj_df$TBATS,zero_values)

colors <- c("Validation" = "black", "Seasonal Naive" = "red", "Holt Winters" = "blue", "Loess Forecasting" = "green", "Box-Cox Transformation" = "purple")
ggplot(collated_forecast_sj_df, aes(x=date)) + geom_line(aes(y=Validation,color="Validation")) + 
  geom_line(aes(y=Seasonal_Naive,color="Seasonal Naive")) +
  geom_line(aes(y=Holt_Winters,color="Holt Winters")) +
  geom_line(aes(y=STLF,color="Loess Forecasting")) +
  geom_line(aes(y=TBATS,color="Box-Cox Transformation")) +
  labs(title = "All forecast models with validation data", x = "Date", y = "Number of Cases") +
  scale_color_manual(values = colors)

#Plotting all time series in one plot using ts.plot
ts.plot(validation_casesTS_sj,naive_sj$mean,stlf_model_sj$mean,holt_winters_sj$mean,tbats_forecast_sj$mean, col=c("black","red","blue","cyan","green"))
legend("top", c("Validation","Seasonal Naive","Loess Forecasting","Holt Winters", "Box-Cos Transformation"), col=c("black","red","blue","cyan","green"), pch=15)

#Iquitos forecasting
data_train_iq <- data_train_total[data_train_total$city == "iq",]

#Training data from 2000 to 2008
data_train_iq <- data_train_iq[data_train_iq$year < 2009,]

#Validation data from 2009
data_validation_iq <- data_train_total[data_train_total$city == "iq",]
data_validation_iq <- data_validation_iq[data_validation_iq$year == 2009,]
data_validation_iq = na.omit(data_validation_iq)

#Plotting the total cases as a Time Series after sorting the dataframe based on date
data_validation_iq <- data_validation_iq[order(as.Date(data_validation_iq$week_start_date)),]
validation_casesTS_iq <- ts(data_validation_iq$total_cases, frequency=52, start=c(2009,1))

#Total cases over time for Iquitos
ggplot(data_train_iq, aes(x=week_start_date, y=total_cases)) + geom_line()

#Plotting the total cases as a Time Series after sorting the dataframe based on date
data_train_iq <- data_train_iq[order(as.Date(data_train_iq$week_start_date)),]
train_casesTS_iq <- ts(data_train_iq$total_cases, frequency = 52, start = c(2000,26))

#decompose the San Juan total cases time series
casesTS_iq_decompose <- decompose(train_casesTS_iq)
plot(casesTS_iq_decompose)
seasonplot(train_casesTS_iq, col=terrain.colors(6))

#Seasonal Naive Prediction
naive_iq = snaive (train_casesTS_iq, h=length(data_validation_iq$total_cases))
plot(naive_iq)

#Forecasts of STL objects are obtained by applying a non-seasonal forecasting method to the seasonally adjusted data
#STLF - Seasonal and Trend decomposition using Loess Forecasting model
stlf_model_iq = stlf(train_casesTS_iq, allow.multiplicative.trend = FALSE, h=length(data_validation_iq$total_cases))
plot(stlf_model_iq)

#Triple exponential Holt Winters- models level, trend, and seasonal components
triple_fit <- HoltWinters(train_casesTS_iq)
holt_winters_iq <- (forecast(triple_fit,h=length(data_validation_iq$total_cases)))
plot(holt_winters_iq)

#TBATS
#Exponential Smoothing State Space Model with Box-Cox Transformation
tbats_model_iq = tbats(train_casesTS_iq,seasonal.periods=52, use.trend=TRUE,use.box.cox=TRUE)
tbats_forecast_iq = forecast(tbats_model_iq, h=length(data_validation_iq$total_cases))
plot(tbats_forecast_iq)

accuracy(naive_iq$mean, validation_casesTS_iq)
accuracy(stlf_model_iq$mean, validation_casesTS_iq)
accuracy(holt_winters_iq$mean, validation_casesTS_iq)
accuracy(tbats_forecast_iq$mean, validation_casesTS_iq)

#Appending all the forecast time series into one dataframe for plotting
Seasonal_Naive_forecast_iq_df = data.frame(Seasonal_Naive=as.matrix(naive_iq$mean), date=time(naive_iq$mean))
STLF_forecast_iq_df = data.frame(STLF=as.matrix(stlf_model_iq$mean), date=time(stlf_model_iq$mean))
HW_forecast_iq_df = data.frame(Holt_Winters=as.matrix(holt_winters_iq$mean), date=time(holt_winters_iq$mean))
TBATS_forecast_iq_df = data.frame(TBATS=as.matrix(tbats_forecast_iq$mean), date=time(tbats_forecast_iq$mean))
validation_casesTS_iq_df = data.frame(Validation=as.matrix(validation_casesTS_iq), date=time(validation_casesTS_iq))
collated_forecast_iq_df = merge(Seasonal_Naive_forecast_iq_df, STLF_forecast_iq_df)
collated_forecast_iq_df = merge(collated_forecast_iq_df, HW_forecast_iq_df)
collated_forecast_iq_df = merge(collated_forecast_iq_df,TBATS_forecast_iq_df)
collated_forecast_iq_df = merge(collated_forecast_iq_df,validation_casesTS_iq_df)

#Change negative cases to zero
collated_forecast_iq_df$Seasonal_Naive <- sapply(collated_forecast_iq_df$Seasonal_Naive,zero_values)
collated_forecast_iq_df$STLF <- sapply(collated_forecast_iq_df$STLF,zero_values)
collated_forecast_iq_df$Holt_Winters<- sapply(collated_forecast_iq_df$Holt_Winters,zero_values)
collated_forecast_iq_df$TBATS <- sapply(collated_forecast_iq_df$TBATS,zero_values)

colors <- c("Validation" = "black", "Seasonal Naive" = "red", "Holt Winters" = "blue", "Loess Forecasting" = "green", "Box-Cox Transformation" = "purple")
ggplot(collated_forecast_iq_df, aes(x=date)) + geom_line(aes(y=Validation,color="Validation")) + 
  geom_line(aes(y=Seasonal_Naive,color="Seasonal Naive")) +
  geom_line(aes(y=Holt_Winters,color="Holt Winters")) +
  geom_line(aes(y=STLF,color="Loess Forecasting")) +
  geom_line(aes(y=TBATS,color="Box-Cox Transformation")) +
  labs(title = "All forecast models with validation data", x = "Date", y = "Number of Cases") +
  scale_color_manual(values = colors)

#Plotting all time series in one plot using ts.plot
ts.plot(validation_casesTS_iq,naive_iq$mean,stlf_model_iq$mean,holt_winters_iq$mean,tbats_forecast_iq$mean, col=c("black","red","blue","cyan","green"))
legend("top", c("Validation","Seasonal Naive","Loess Forecasting","Holt Winters", "Box-Cos Transformation"), col=c("black","red","blue","cyan","green"), pch=15)


# %% [code] {"_execution_state":"idle"}
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load
#install.packages("forecastML")
#install.packages("xgboost")
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(xgboost)
library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

#list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

# %% [markdown]
# # Load the dataset 

# %% [code]
data_train_feature = read_csv("dengue_features_train.csv")
data_train_label = read_csv("dengue_labels_train.csv")
data_pop_iq = read_csv("Iquitos_Population_Data.csv")
data_pop_sj = read_csv("San_Juan_Population_Data.csv")


# %% [code]
# Join population table into data_train_feature
data_pop_iq$city = 'iq'
data_pop_sj$city = 'sj'
data_train_feature = merge(x = data_train_feature, y = data_pop_iq, by.x = c("year", "city"), by.y = c("Year", "city"), all.x = TRUE)
data_train_feature = merge(x = data_train_feature, y = data_pop_sj, by.x = c("year", "city"), by.y = c("Year", "city"), all.x = TRUE)

#Combine to one column
data_train_feature$Estimated_population.x = replace_na(data_train_feature$Estimated_population.x, 0)
data_train_feature$Estimated_population.y = replace_na(data_train_feature$Estimated_population.y, 0)
data_train_feature = mutate(data_train_feature, population = Estimated_population.x + Estimated_population.y)
data_train_feature = subset(data_train_feature, select = -c(Estimated_population.x,Estimated_population.y) )

# Fill forward missing values
data_train_feature = arrange(data_train_feature, city, year, weekofyear)
data_train_feature = data_train_feature %>%  group_by(city) %>% 
  fill(
    ndvi_ne, 
    ndvi_nw, 
    ndvi_se, 
    ndvi_sw,
    precipitation_amt_mm,
    reanalysis_air_temp_k,
    reanalysis_avg_temp_k,
    reanalysis_dew_point_temp_k,
    reanalysis_max_air_temp_k,
    reanalysis_min_air_temp_k,
    reanalysis_precip_amt_kg_per_m2,
    reanalysis_relative_humidity_percent,
    reanalysis_sat_precip_amt_mm,
    reanalysis_specific_humidity_g_per_kg,
    reanalysis_tdtr_k,
    station_avg_temp_c,
    station_diur_temp_rng_c,
    station_max_temp_c,
    station_min_temp_c,
    station_precip_mm
  )

#Start of Time Forecasting
data_train_feature <- as.data.frame(data_train_feature)
data_train_label <- as.data.frame(data_train_label)
data_train_total <- merge(data_train_feature, data_train_label, by = c("city","weekofyear","year"))
data_train_total <- data_train_total[order(as.Date(data_train_total$week_start_date, "%Y-%m-%d")),]
data_train_total$week_start_date <-as.Date(data_train_total$week_start_date, "%Y-%m-%d")
head(data_train_total)

# %% [markdown]
# # Plot the dataset 

# %% [code]
p <- ggplot(train_df, aes(x = weekofyear, y = total_cases, color = ordered(city)))
p <- p + geom_line()
p <- p + facet_wrap(~ ordered(year), scales = "fixed")
p <- p + theme_bw() + theme(legend.position = "none")
p


# %% [markdown]
# [](http://)# Forecast for City SJ

# %% [code]
data_sj <- data_train_total[data_train_total$city == 'sj',]
data_sj <- data_sj[data_sj$year < 2007,]
dates <- data_sj$week_start_date  # Grouped time series forecasting requires dates.

data_sj$week_start_date <- NULL  # Dates, however, don't need to be in the input data.
data_sj$population <- NULL
data_sj$city <- NULL 

outcome_col <- 23  # The column position of our 'wind_spd' outcome (after removing the 'date' column).

horizons <- c(52)  # Forecast 1, 1:7, and 1:30 days into the future.

lookback <- c(1:52)  # Features from 1 to 30 days in the past and annually.

frequency <- "7 day"  # A string that works in base::seq(..., by = "frequency").

dynamic_features <- c("weekofyear", "year")  # Features that change through time but which will not be lagged.

groups <- "city"  # 1 forecast for each group or buoy.

type <- "train"  # Create a model-training dataset.

data_sj_train <- forecastML::create_lagged_df(data_sj, type = type, outcome_col = outcome_col,
                                           horizons = horizons, lookback = lookback,
                                           dates = dates, frequency = frequency,
                                           dynamic_features = dynamic_features, 
                                           use_future = FALSE)

windows <- forecastML::create_windows(data_sj_train, window_length = 52, skip = 52,
                                      include_partial_window = FALSE)

p <- plot(windows, data_sj_train) + theme(legend.position = "none")
p

# %% [markdown]
# # Train the data

# %% [code]
# The value of outcome_col can also be set in train_model() with train_model(outcome_col = 1).
model_function <- function(data, outcome_col = 1) {
  
  # xgboost cannot handle missing outcomes data.
  data <- data[!is.na(data[, outcome_col]), ]

  indices <- 1:nrow(data)
  
  set.seed(37)
  train_indices <- sample(1:nrow(data), ceiling(nrow(data) * .8), replace = FALSE)
  test_indices <- indices[!(indices %in% train_indices)]

  data_train <- xgboost::xgb.DMatrix(data = as.matrix(data[train_indices, 
                                                           -(outcome_col), drop = FALSE]),
                                     label = as.matrix(data[train_indices, 
                                                            outcome_col, drop = FALSE]))

  data_test <- xgboost::xgb.DMatrix(data = as.matrix(data[test_indices, 
                                                          -(outcome_col), drop = FALSE]),
                                    label = as.matrix(data[test_indices, 
                                                           outcome_col, drop = FALSE]))

  params <- list("objective" = "reg:squarederror")
  watchlist <- list(train = data_train, test = data_test)
  
  set.seed(224)
  model <- xgboost::xgb.train(data = data_train, params = params, 
                              max.depth = 10, nthread = 4, nrounds = 50,
                              metrics = "rmse", verbose = 0, 
                              early_stopping_rounds = 8, 
                              watchlist = watchlist)

  return(model)
}

model_results_cv <- forecastML::train_model(lagged_df = data_sj_train,
                                            windows = windows,
                                            model_name = "xgboost",
                                            model_function = model_function, 
                                            use_future = FALSE)

# %% [markdown]
# # Cross Validation

# %% [code]
# If 'model' is passed as a named list, the prediction model would be accessed with model$model or model["model"].
prediction_function <- function(model, data_features) {
  x <- xgboost::xgb.DMatrix(data = as.matrix(data_features))
  data_pred <- data.frame("y_pred" = predict(model, x)) 
  return(data_pred)
}

data_sj_pred_cv <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_sj_train)
plot(data_sj_pred_cv)

# %% [code]
data_error <- forecastML::return_error(data_sj_pred_cv)
data_error

# %% [markdown]
# # ****Forecasting with multiple models from nested CV****
# 
# We have dataset that supports forecasting 52 weeks into the future. We’ll view the H-week-ahead forecasting data below

# %% [code]
type <- "forecast"  # Create a forecasting dataset for our predict() function.

data_sj_forecast <- forecastML::create_lagged_df(data_sj, type = type, outcome_col = outcome_col,
                                              horizons = horizons, lookback = lookback,
                                              dates = dates, frequency = frequency,
                                              dynamic_features = dynamic_features,
                                              use_future = FALSE)

for (i in seq_along(data_sj_forecast)) {
  data_sj_forecast[[i]]$weekofyear <- lubridate::week(data_sj_forecast[[i]]$index)  # When dates are given, the 'index` is date-based.
  data_sj_forecast[[i]]$year <- lubridate::year(data_sj_forecast[[i]]$index)
}

data_sj_forecasts <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_sj_forecast)
plot(data_sj_forecasts)

# %% [markdown]
# # ****Model Training & Forecast with All Data****

# %% [code]
windows <- forecastML::create_windows(data_sj_train, window_length = 0)

model_results_no_cv <- forecastML::train_model(lagged_df = data_sj_train, 
                                               windows = windows,
                                               model_name = "xgboost",
                                               model_function = model_function,
                                               use_future = FALSE)

data_sj_forecasts <- predict(model_results_no_cv, prediction_function = list(prediction_function), data = data_sj_forecast)

# %% [markdown]
# # Plot the forecast for 52 weeks ahead

# %% [code]
data_sj_combined <- forecastML::combine_forecasts(data_sj_forecasts)

# Plot a background dataset of actuals using the most recent data.
data_actual <- data_sj[dates >= as.Date("2005-01-01"), ]
actual_indices <- dates[dates >= as.Date("2005-01-01")]

# Plot all final forecasts plus historical data.
plot(data_sj_combined, data_actual = data_actual, actual_indices = actual_indices)

# %% [code]
accuracy(data_sj_forecasts$total_cases_pred,validation_casesTS_sj)

# %% [code]
collated_forecast_sj_df$xgb <- data_sj_forecasts$total_cases_pred

colors <- c("Validation" = "black", "Seasonal Naive" = "red", "Holt Winters" = "blue", "Loess Forecasting" = "green", "Box-Cox Transformation" = "purple", "XGBoost" = "yellow")
ggplot(collated_forecast_sj_df, aes(x=date)) + geom_line(aes(y=Validation,color="Validation")) + 
  geom_line(aes(y=Seasonal_Naive,color="Seasonal Naive")) +
  geom_line(aes(y=Holt_Winters,color="Holt Winters")) +
  geom_line(aes(y=STLF,color="Loess Forecasting")) +
  geom_line(aes(y=TBATS,color="Box-Cox Transformation")) +
    geom_line(aes(y=xgb,color="XGBoost")) +
  labs(title = "All forecast models with validation data", x = "Date", y = "Number of Cases") +
  scale_color_manual(values = colors)


# %% [code]

#Plotting all time series in one plot using ts.plot
ts.plot(validation_casesTS_sj,naive_sj$mean,stlf_model_sj$mean,holt_winters_sj$mean,tbats_forecast_sj$mean, data_sj_forecasts$total_cases_pred, col=c("black","red","blue","cyan","green","yellow"))
legend("top", c("Validation","Seasonal Naive","Loess Forecasting","Holt Winters", "Box-Cos Transformation","XGBoost"), col=c("black","red","blue","cyan","green","yellow"), pch=15)


# %% [markdown]
# # ****Forecast for City IQ****

# %% [code]
data_iq <- data_train_total[data_train_total$city == 'iq',]
data_iq <- data_iq[data_iq$year < 2009,]
dates <- data_iq$week_start_date  # Grouped time series forecasting requires dates.

data_iq$week_start_date <- NULL  # Dates, however, don't need to be in the input data.
data_iq$population <- NULL
data_iq$city <- NULL 

type <- "train"  # Create a model-training dataset.

data_iq_train <- forecastML::create_lagged_df(data_iq, type = type, outcome_col = outcome_col,
                                           horizons = horizons, lookback = lookback,
                                           dates = dates, frequency = frequency,
                                           dynamic_features = dynamic_features, 
                                           use_future = FALSE)

windows <- forecastML::create_windows(data_iq_train, window_length = 52, skip = 52,
                                      include_partial_window = FALSE)

p <- plot(windows, data_iq_train) + theme(legend.position = "none")
p

# %% [markdown]
# # Train the model & perform CV

# %% [code]
model_results_cv <- forecastML::train_model(lagged_df = data_iq_train,
                                            windows = windows,
                                            model_name = "xgboost",
                                            model_function = model_function, 
                                            use_future = FALSE)

data_iq_pred_cv <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_iq_train)
plot(data_iq_pred_cv)

# %% [code]
data_error <- forecastML::return_error(data_iq_pred_cv)
data_error

# %% [markdown]
# # ****Forecasting with multiple models from nested CV****

# %% [code]
type <- "forecast"  # Create a forecasting dataset for our predict() function.

data_iq_forecast <- forecastML::create_lagged_df(data_iq, type = type, outcome_col = outcome_col,
                                              horizons = horizons, lookback = lookback,
                                              dates = dates, frequency = frequency,
                                              dynamic_features = dynamic_features,
                                              use_future = FALSE)

for (i in seq_along(data_iq_forecast)) {
  data_iq_forecast[[i]]$weekofyear <- lubridate::week(data_iq_forecast[[i]]$index)  # When dates are given, the 'index` is date-based.
  data_iq_forecast[[i]]$year <- lubridate::year(data_iq_forecast[[i]]$index)
}

data_iq_forecasts <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_iq_forecast)
plot(data_iq_forecasts)

# %% [markdown]
# # Model Training & Forecast with All Data

# %% [code]
windows <- forecastML::create_windows(data_iq_train, window_length = 0)

model_results_no_cv <- forecastML::train_model(lagged_df = data_iq_train, 
                                               windows = windows,
                                               model_name = "xgboost",
                                               model_function = model_function,
                                               use_future = FALSE)

data_iq_forecasts <- predict(model_results_no_cv, prediction_function = list(prediction_function), data = data_iq_forecast)

# %% [markdown]
# # Plot the Forecast

# %% [code]
data_iq_combined <- forecastML::combine_forecasts(data_iq_forecasts)

# Plot a background dataset of actuals using the most recent data.
data_actual <- data_iq[dates >= as.Date("2007-01-01"), ]
actual_indices <- dates[dates >= as.Date("2007-01-01")]

# Plot all final forecasts plus historical data.
plot(data_iq_combined, data_actual = data_actual, actual_indices = actual_indices)

# %% [code]
accuracy(data_iq_forecasts$total_cases_pred,validation_casesTS_iq)

# %% [code]
collated_forecast_iq_df <- data.frame(Validation=as.matrix(validation_casesTS_iq), date=time(validation_casesTS_iq))
collated_forecast_iq_df$Seasonal_Naive <- ifelse(naive_iq$mean < 0, 0,naive_iq$mean)
collated_forecast_iq_df$STLF <- ifelse(stlf_model_iq$mean < 0, 0,stlf_model_iq$mean)
collated_forecast_iq_df$Holt_Winters <- ifelse(holt_winters_iq$mean < 0, 0,holt_winters_iq$mean)
collated_forecast_iq_df$TBATS <- ifelse(tbats_forecast_iq$mean < 0, 0,tbats_forecast_iq$mean)
collated_forecast_iq_df$xgb <- data_iq_forecasts$total_cases_pred

colors <- c("Validation" = "black", "Seasonal Naive" = "red", "Holt Winters" = "blue", "Loess Forecasting" = "green", "Box-Cox Transformation" = "purple", "XGBoost" = "yellow")
ggplot(collated_forecast_iq_df, aes(x=date)) + geom_line(aes(y=Validation,color="Validation")) + 
  geom_line(aes(y=Seasonal_Naive,color="Seasonal Naive")) +
  geom_line(aes(y=Holt_Winters,color="Holt Winters")) +
  geom_line(aes(y=STLF,color="Loess Forecasting")) +
  geom_line(aes(y=TBATS,color="Box-Cox Transformation")) +
    geom_line(aes(y=xgb,color="XGBoost")) +
  labs(title = "All forecast models with validation data", x = "Date", y = "Number of Cases") +
  scale_color_manual(values = colors)