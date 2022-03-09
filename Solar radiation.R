# Install the packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")

# Importing the data
rawdata <- read.csv(file="SolarPrediction.csv", header = TRUE)

#check for any missing values
check <- anyNA(rawdata) 

# Convert UNIX time to datetime and adjust sunrise/sunset times in correct timezone 
# Add length of daylight hours and relative time of the day
rawdata <- rawdata %>% 
  mutate(data = round_date(as_datetime(UNIXTime), unit = "seconds")) %>%
  mutate(time = with_tz(data,tzone="US/Hawaii")) %>%
  mutate(date=date(time)) %>%
  mutate(sunrisetime = as.POSIXct(paste(date, TimeSunRise), format = "%Y-%m-%d %H:%M:%S",tz="US/Hawaii")) %>%
  mutate(sunsettime = as.POSIXct(paste(date, TimeSunSet), format = "%Y-%m-%d %H:%M:%S",tz="US/Hawaii")) %>% 
  mutate(daylighthours = as.numeric(difftime(sunsettime, sunrisetime), units="hours", tz="US/Hawaii")) %>%
  mutate(rel_timeofday = as.numeric(difftime(time,sunrisetime)/daylighthours, units="hours",tz="US/Hawaii"))

# Select the required columns and sort them with time
solardata <- rawdata %>% 
  select(UNIXTime,date,time,sunrisetime,sunsettime,Radiation,Temperature,Pressure,Humidity,WindDirection.Degrees.,Speed,daylighthours,rel_timeofday) %>%
  arrange(UNIXTime) 

rm(rawdata,check)

# Separate and plot a day's data 
data_one_day <- solardata %>%
  filter(as_date(date) >= as_date("2016-10-01") & 
           as_date(date) <= as_date("2016-10-01"))

a <- data_one_day %>% ggplot(aes(time,Radiation)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = expression(paste("Radiation", (W/m^2))))+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

b <- data_one_day %>% ggplot(aes(time,Temperature)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = expression("Temperature ("*~degree*F*")"))+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

c <- data_one_day %>% ggplot(aes(time,Pressure)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = "Pressure (Hg)")+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

d <- data_one_day %>% ggplot(aes(time,Humidity)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = "Humidity (%)")+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

e <- data_one_day %>% ggplot(aes(time,WindDirection.Degrees.)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = "Wind direction (degrees)")+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

f <- data_one_day %>% ggplot(aes(time,Speed)) + 
  geom_point() + 
  theme_light() +
  geom_vline(data=data_one_day,aes(xintercept=sunrisetime[1]), linetype="dotted",color="red")+
  geom_vline(data=data_one_day,aes(xintercept=sunsettime[1]), linetype="dotted", color="blue")+
  labs(x = "Local time", y = "Speed (miles/hr)")+ 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") 

plot_grid(a, b, c, d, e, f, label_size = 12, ncol = 2)

rm(data_one_day,a, b, c, d, e, f)

# Distribution of solar radiation
solardata %>%
  ggplot(aes(Radiation)) +
  geom_histogram(aes(y=..density..),binwidth = 50, color = "black") +
  geom_density(alpha=0.6,color="red") +
  theme_light() +
  labs(x = expression(paste("Solar radiation", (watts/m^2))), y = "Density")

# Distribution of temperature
solardata %>%
  ggplot(aes(Temperature)) +
  geom_histogram(aes(y=..density..),binwidth = 1, color = "black") +
  geom_density(alpha=0.6, color="red") + 
  theme_light()+
  labs(x = expression("Temperature ("*~degree*F*")"), y = "Density")
  
# Distribution of pressure
solardata %>%
  ggplot(aes(Pressure)) +
  geom_histogram(aes(y=..density..),binwidth = 0.02, color = "black") +
  geom_density(alpha=0.6, color="red") + 
  theme_light()+
  labs(x = "Pressure (Hg)", y = "Density") 

# Distribution of humidity
solardata %>%
  ggplot(aes(Humidity)) +
  geom_histogram(aes(y=..density..),binwidth = 1, color = "black") +
  geom_density(alpha=0.6, color="red") +
  theme_light() +
  labs(x = "Humidity (%)", y = "Density")  

# Distribution of wind direction
solardata %>%
  ggplot(aes(WindDirection.Degrees.)) +
  geom_histogram(aes(y=..density..),binwidth = 5, color = "black") +
  geom_density(alpha=0.6, color="red") +
  theme_light() +
  labs(x = "Wind direction (degrees)", y = "Density")

# Distribution of wind speed
solardata %>%
  ggplot(aes(Speed)) +
  geom_histogram(aes(y=..density..),binwidth = 1.5, color = "black") +
  geom_density(alpha=0.6, color="red") +
  theme_light() +
  labs(x = "Wind speed (miles/hour)", y = "Density")

# Correlation matrix
cor_data <- solardata %>% 
  select(Radiation,Temperature,Pressure,Humidity,WindDirection.Degrees.,Speed,rel_timeofday)

data.cor <- cor(cor_data)  

corrplot(data.cor, method="color", type="upper",is.corr = FALSE, addCoef.col = "black",cl.pos="n")

rm(data.cor)

# Scatter plots
a <- cor_data %>% ggplot(aes(Temperature,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = expression("Temperature ("*~degree*F*")"), y = expression(paste("Radiation", (W/m^2))))

b <- cor_data %>% ggplot(aes(Pressure,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = "Pressure (Hg)", y = expression(paste("Radiation", (W/m^2))))

c <- cor_data %>% ggplot(aes(Humidity,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = "Humidity (%)", y = expression(paste("Radiation", (W/m^2))))

d <- cor_data %>% ggplot(aes(WindDirection.Degrees.,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = "Wind direction (degrees)", y = expression(paste("Radiation", (W/m^2))))

e <- cor_data %>% ggplot(aes(Speed,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = "Speed (miles/hr)", y = expression(paste("Radiation", (W/m^2))))
  
f <- cor_data %>% ggplot(aes(rel_timeofday,Radiation)) + 
  geom_point() + 
  theme_light() +
  labs(x = "Relative time", y = expression(paste("Radiation", (W/m^2))))

plot_grid(a, b, c, d, e, f, label_size = 12, ncol = 2)

rm(a, b, c, d, e, f, cor_data)

# Create train and final validation data sets
data_ana <- solardata %>% 
  select(time,Radiation,Temperature,Pressure,Humidity,WindDirection.Degrees.,Speed,rel_timeofday)

set.seed(1, sample.kind="Rounding")
WkBeg <- as.Date(data_ana$time, format = "%Y/%m/%d %H:%M:%S",tz="US/Hawaii")
N = as.Date("2016-12-20")
train_data <- data_ana[WkBeg < N,]
test_data <- data_ana[WkBeg >= N,]

rm(N, WkBeg, solardata)

# Use the train data set for algorithm development
set.seed(1, sample.kind="Rounding")
train.control <- trainControl(method = "cv", number = 5, search = "grid")

dtrain <- train_data[-1]

rm(data_ana,train_data)

# Linear regression
set.seed(1, sample.kind="Rounding")
lm_fit <- train(Radiation ~ ., data=dtrain, 
                method = "lm", trControl = train.control)

rmse_lm <- lm_fit$results$RMSE
r2_lm <- lm_fit$results$Rsquared

results <- data.frame(Method = c("Linear regression"),
                      R2 = round(r2_lm,5), RMSE = round(rmse_lm,5))

# Support Vector Regression 
set.seed(1, sample.kind="Rounding")
grid <- expand.grid(sigma = 1, C = 2)

svr_fit <- train(Radiation ~ ., data = dtrain, 
                 method = "svmRadial", 
                 trControl = train.control, 
                 tuneGrid = grid,
                 preProc = c("center", "scale"))

rmse_svr <- svr_fit$results$RMSE
r2_svr <- svr_fit$results$Rsquared

results <- results %>% 
  rbind(c("Support Vector Regression", round(r2_svr,5),
          round(rmse_svr,5)))

# Random forest
set.seed(1, sample.kind="Rounding")
rf_fit <- train(Radiation ~., data=dtrain, 
                method = "rf",
                tuneGrid = data.frame(mtry=5),
                trControl = train.control,
                importance = TRUE,
                ntree = 500,nodesize=5)

rmse_rf <- rf_fit$results$RMSE
r2_rf <- rf_fit$results$Rsquared

results <- results %>% 
  rbind(c("Random Forest regression",round(r2_rf,5), 
          round(rmse_rf,5)))

varImp(rf_fit)$importance %>% 
  mutate(names=row.names(.)) %>%
  arrange(desc(Overall)) %>% 
  ggplot(aes(reorder(names, Overall), Overall)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Important variables")

# Stochastic Gradient boosting
set.seed(1, sample.kind="Rounding")

sgb_grid <-  expand.grid(interaction.depth = 12, 
                        n.trees = 1500, 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

sgb_fit <- train(Radiation ~ ., data = dtrain, 
                 method = "gbm", 
                 trControl = train.control,
                 verbose = FALSE,
                 tuneGrid = sgb_grid)

rmse_sgb <- sgb_fit$results$RMSE
r2_sgb <- sgb_fit$results$Rsquared

results <- results %>% 
  rbind(c("SGB regression", round(r2_sgb,5), round(rmse_sgb,5)))

varImp(sgb_fit) %>%
  ggplot(aes(reorder(names, Overall), Overall)) +
  geom_col() +
  theme_light() +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Important variables") 

# Extreme Gradient boosting
set.seed(1, sample.kind="Rounding")
xgb_grid = expand.grid(nrounds = 1000,
                       max_depth = 11,
                       eta = 0.01,
                       gamma = 0,
                       colsample_bytree = 1,  
                       subsample = 1, 
                       min_child_weight = 1)

xgb_fit <- train(Radiation ~ ., data = dtrain, 
                 method = "xgbTree", 
                 trControl = train.control,
                 verbose = FALSE,
                 tuneGrid = xgb_grid)

rmse_xgb <- xgb_fit$results$RMSE
r2_xgb <- xgb_fit$results$Rsquared

results <- results %>% 
  rbind(c("XGBoost regression", round(r2_xgb,5),
          round(rmse_xgb,5)))

varImp(xgb_fit) %>%
  ggplot(aes(reorder(names, Overall), Overall)) +
  geom_col() +
  theme_light() +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Important variables") 

# Ensemble model
set.seed(1, sample.kind="Rounding")
train.Control <- trainControl(method = "cv", number = 5, 
                              search = "grid", 
                              savePredictions="final")

xgbgrid <- data.frame(nrounds = 1000, max_depth = 11, 
                      eta = 0.01, gamma = 0, 
                      colsample_bytree = 1,  
                      subsample = 1, 
                      min_child_weight = 1)

svrgrid <- data.frame(sigma = 1, C = 2)

rfgrid <- data.frame(mtry=5)

sgb_grid <-  expand.grid(interaction.depth = 12, 
                         n.trees = 1500, 
                         shrinkage = 0.1,
                         n.minobsinnode = 10)

set.seed(1, sample.kind="Rounding")

model_list <- caretList(Radiation ~ ., data = dtrain,
              tuneList = list(
          SGB = caretModelSpec(method = "gbm", 
                                 verbose = FALSE,
                                 tuneGrid = sgb_grid),
      RF = caretModelSpec(method='rf', 
                        tuneGrid = rfgrid, ntree = 500,
                        importance = TRUE), 
      SVR = caretModelSpec(method = "svmRadial", 
                          tuneGrid = svrgrid),
      XGBoost = caretModelSpec(method = "xgbTree", 
                                tuneGrid = xgbgrid, nthread = 2)),
      trControl = train.Control,
      preProc = c("center", "scale"))

# Results
res <- resamples(model_list)
a <- modelCor(res)

modelcorrelation <- data.frame(a)

set.seed(1, sample.kind="Rounding")
modelEnsemble <- caretEnsemble(
  model_list, metric="RMSE", trControl=train.Control)

results <- results %>% 
  rbind(c("Ensemble algorithm", round(modelEnsemble$error$Rsquared,5), round(modelEnsemble$error$RMSE,5)))

# Final validation

dtest <- test_data[-1]

# Random forest
y_hat_rf <- predict(rf_fit, dtest)

r2_rf <- cor(dtest$Radiation,y_hat_rf)
rmse_rf <- RMSE(dtest$Radiation,y_hat_rf)

final_results <- data.frame(Method = c("RF regression"),
                      R2 = round(r2_rf,5), 
                      RMSE = round(rmse_rf,5))

# Extreme Gradient boosting
y_hat_xgb <- predict(xgb_fit, dtest)

r2_xgb <- cor(dtest$Radiation,y_hat_xgb)
rmse_xgb <- RMSE(dtest$Radiation,y_hat_xgb)

final_results <- final_results %>% 
  rbind(c("XGBoost regression", round(r2_xgb,5), round(rmse_xgb,5)))

# Ensemble algorithm
y_hat_ensemble <- predict(modelEnsemble, dtest)

r2_ensemble <- cor(dtest$Radiation,y_hat_ensemble)
rmse_ensemble <- RMSE(dtest$Radiation,y_hat_ensemble)

final_results <- final_results %>% 
  rbind(c("Ensemble algorithm", round(r2_ensemble,5), round(rmse_ensemble,5)))

# Plot the model predicted and actual values for a period of 5 days

df <- test_data %>% select(time,Radiation) %>%
  mutate(RF=y_hat_rf,XGBoost=y_hat_xgb,Ensemble=y_hat_ensemble)

COLORS <- c(Actual = "black", RF ="red", XGBoost = "blue", Ensemble = "green")

df %>% pivot_longer(cols = Radiation:Ensemble, names_to = "Models", values_to = "values") %>%
  ggplot(aes(x = time, y = values, color = Models))+
  scale_color_manual(values = COLORS)+
  geom_point() +
  geom_line()+
  theme_light() +
  labs(x = "Date", y = expression(paste("Radiation", (W/m^2))))
