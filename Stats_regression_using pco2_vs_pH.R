##   NP created 5-15-2020 ##
############################

library(lubridate)
library (ggplot2)
library (padr)
library(gridExtra)
library(caret)
library(tidyverse)
library(ggpmisc)
library(data.table)


pco2<-read.csv("https://raw.githubusercontent.com/em0ls0n/nicole-pCO2/master/12-18-2019_ProCV_log_data_4R.csv") ###EO - moved files to github for easy R editing
pco2<-subset(pco2, select=c(2,3,4,5,6,7,10))

# get Date and Time in one column as POSIXct
pco2$Date = paste(pco2$Year, pco2$Month, pco2$Day, sep = "-")
pco2$Time = paste(pco2$Hour, pco2$Minute, pco2$Second, sep = ":")
pco2$Date_Time = as.POSIXct(paste(pco2$Date, pco2$Time), format = "%Y-%m-%d %H:%M:%OS")

# remove test/unwanted data from data frame (deployment vs lab testing)
pco2 = subset(pco2, Date_Time > as.POSIXct("2019-07-10 00:00:00") & Date_Time < as.POSIXct ("2019-12-12"))

# find min and max CO2 values
min(pco2$CO2, na.rm=T) # =  253.59
max(pco2$CO2, na.rm=T) # = 6139.62

# remove erroneous data from data frame
#use all data for plots, use subset for regressions
pco2 = subset(pco2, CO2 < 1500)

# add in dummy records to split up lines in plot
pco2 = thicken (pco2, interval = "day") ###EO - did you need to do this? You already have the column "Date" witht the same info, unless having a 2 digit month integer matters vs a 1 digit month integer

# Read in EXO data
Exo = read.csv("https://raw.githubusercontent.com/em0ls0n/nicole-pCO2/master/2019BH_data.csv", header = TRUE)
#remove NAS
Exo[is.na(Exo)] <-"" ###EO - hmmmmm....
# get Date and Time in one column as POSIXct
Exo$Date_Time <- as.POSIXct(strptime(x = as.character(Exo$Date_Time),
                                     format = "%Y-%m-%d %H:%M:%S"))
#Change all WQ variables to numeric (columns 3-10)
Exo[, 3:10] <- sapply(Exo[, 3:10], as.numeric)

####### JOINING pCO2 and Exo pH into "combined"####################################################
pco2.table= as.data.table(pco2)
Exo.table=as.data.table(Exo)

setkey( Exo.table, Date_Time )
setkey( pco2.table, Date_Time )

combined <- Exo.table[ pco2.table, roll = "nearest" ]  ###EO - something went wrong with the combination here. Your "combined" values do not match up-- eg the metadata associated with the nearest Date_Time from Exos (2019-07-10 13:00:00) to the pCO2 value from the first PCO2 table Date_Time(2019-07-10 13:03:39) is wrong. And the temp values are all wrong in combined. I think this "roll" function is causing it but I don't know why.
combined


########################################################################################
#########################  CO2 Hourly means and plot   ####################################
#########################################################################################
# add in dummy records for hourly means
pco2A<-pco2 #make a new file
pco2A<-subset(pco2A, select=c(1:10)) #remove date_time_day
PA = thicken(pco2A, interval = "hour") #creates Date_Time_hour col
PA = pad (PA, by = 'Date_Time_hour', interval = "hour") #adds in all missing hours for entire dataset

# Average the hourly readings - keeping all date holders
means<-aggregate(CO2 ~ Date_Time_hour, PA, mean, na.action=na.pass)

# plot hourly means
M_plot <- ggplot(means, aes(x=Date_Time_hour, y=CO2)) + # plot date vs co2 means
  geom_point(alpha=0.8,color='red', size=1)+ #color of points
  #geom_smooth(method="lm", col='blue')+  #linear model - line of best fit
  ggtitle('2019 Beach Haven pCO2', subtitle='Hourly Means')+ #add title
  xlab('Date')+
  ylab('CO2 ppm')+
  theme(plot.title = element_text(size=20, face="bold"))+ # format title
  # change limits and breaks to include or delete data-range= 253-6140 CO2
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500)) + # add limits and ticks/labels for y axis
  scale_x_datetime(date_labels = "%b",   # customize date axis to show a tick the first of each month
                   limits = c(as.POSIXct("2019-07-01"), as.POSIXct("2019-12-12")),
                   breaks = seq(as.POSIXct("2019-07-01"), as.POSIXct("2019-12-12"), "month"))
M_plot

#########################################################################################
############################   PLOTS   #################################################
########################################################################################

## pH/pCO2 linear regression with formula
pCO2_pH = ggplot(data=combined, aes(CO2, pH))+ #choose data and x y)
  geom_point(size=1.5) + #size of point on plot
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x) + #lm=linear model
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), #adds formula to plot
               parse = TRUE, label.x=250, label.y=7.8) +
  scale_y_log10(limits = c(7.50, 8.5), breaks = seq(7.50, 8.5, 0.25)) +
  #coord_trans(y= "log")+
  scale_x_continuous(limits = c(250, 1000), breaks = seq(250, 1000, 50))+ # change high limit if necc
  ggtitle("2019 Beach Haven", subtitle="pH~pCO2")+
  xlab("CO2 ppm")
pCO2_pH

## pH/pCO2 polynomial regression with formula #**need to check if this is correct
pCO2_pH = ggplot(data=combined, aes(CO2, pH))+ #choose data and x y)
  geom_point(size=1.5) + #size of point on plot
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~poly(x,2)) + #lm=linear model
  stat_poly_eq(formula = y ~ poly(x,2),                                       # x,"order"
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), #adds formula to plot
               parse = TRUE, label.x=250, label.y=7.8) +
  scale_y_log10(limits = c(7.50, 8.5), breaks = seq(7.50, 8.5, 0.25)) +
  #coord_trans(y= "log")+
  scale_x_continuous(limits = c(250, 1000), breaks = seq(250, 1000, 50))+ # change high limit if necc
  ggtitle("2019 Beach Haven", subtitle="pH~pCO2")+
  xlab("CO2 ppm")
pCO2_pH

## pH/pCO2 logrithmic regression with formula
pCO2_pH = ggplot(data=combined, aes(CO2, pH))+ #choose data and x y)
  geom_point(size=1.5) + #size of point on plot
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ log(x)) + #lm=linear model
  stat_poly_eq(formula = y ~ log(x),                                       
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), #adds formula to plot
               parse = TRUE, label.x=250, label.y=7.8) +
  scale_y_log10(limits = c(7.50, 8.5), breaks = seq(7.50, 8.5, 0.25)) +
  #coord_trans(y= "log")+
  scale_x_continuous(limits = c(250, 1000), breaks = seq(250, 1000, 50))+ # change high limit if necc
  ggtitle("2019 Beach Haven", subtitle="pH~pCO2")+
  xlab("CO2 ppm")
pCO2_pH

######################################################################################
## plot co2 vs date

## make col with color depending on <500, >500, >1000, >1500 etc
pco2_color <- pco2
pco2_color$color <-



pCO2_plot = ggplot(pco2, aes(Date_Time,CO2)) +
  # add the line that will plot each data point from 2019
  geom_point(color='red', size=1) +
  # add limits and ticks/labels for y axis - these limits can change
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 250)) + # change high limit as needed
  # customize date axis to show a tick the first of each month
  scale_x_datetime(date_labels = "%m/%d/%y", 
                   limits = c(as.POSIXct("2019-07-01"), as.POSIXct("2019-12-31")),
                   breaks = seq(as.POSIXct("2018-07-01"), as.POSIXct("2019-12-31"), "month")) +
  ggtitle("2019 Beach Haven pCO2")+
  # add x and y axis labels
  xlab('Date') +
  ylab ('CO2 ppm')
pCO2_plot

## plot pH vs date
pH_plot <- ggplot(Exo, aes(Date_Time, pH)) +
  # add the line that will plot each data point from 2019
  geom_point(color='blue', size=1) +
  # add limits and ticks/labels for y axis
  scale_y_log10(limits = c(7.5, 8.5), breaks = seq(7.5, 8.5, 0.25)) +
  # customize date axis to show a tick the first of each month
  scale_x_datetime(date_labels = "%m/%d/%y", 
                   limits = c(as.POSIXct("2019-07-01"), as.POSIXct("2019-12-31")),
                   breaks = seq(as.POSIXct("2019-07-01"), as.POSIXct("2019-12-31"), "month")) +
  ggtitle('2019 Beach Haven pH')+ #add title
  # add x and y axis labels
  xlab('Date') +
  ylab ('pH')
pH_plot #plot

## combine both plots into panel
pCO2_pH_panel = grid.arrange(pCO2_plot, pH_plot, ncol = 1, nrow = 2)
pCO2_pH_panel

###########################################################################################

##########################################################
#### Correlation pH ~ CO2  ######
library(e1071)
C_cor = subset(combined, CO2 < 1000) #remove >1000 values
cor(C_cor$CO2, C_cor$pH, use = "complete.obs")  
#calculate correlation between CO2 and pH. 
#use = "complete.obs" will not use the NA values
#result= -0.7902417 (<1000)
#result= -0.7488452 (<1500)
##########################################################







################################################################################################
#### NICOLE PLAYING - STATS and visuals

### scatter plot with smoothing line suggests a non-linearly decreasing relationship.
scatter.smooth(x=combined$CO2, y=combined$pH, main="pCO2~pH")

### Boxplot - check for outliers
par(mfrow=c(1,2))
boxplot(combined$CO2, main="pCO2", sub=paste("Outlier rows:", boxplot.stats(combined$CO2)$out))
boxplot(combined$pH, main="pH", sub=paste("Outlier rows:", boxplot.stats(combined$pH)$out))

par(mfrow=c(1,2))
boxplot(combined$CO2, main="pCO2")#, sub=paste("Outlier rows:", boxplot.stats(combined$CO2)$out))
boxplot(combined$pH, main="pH")#, sub=paste("Outlier rows:", boxplot.stats(combined$pH)$out))

a<-boxplot(combined$pH)# shows n=sample#, conf=upper and lower extremes, out=outliers, stat= upper extreme, top of box, median, lower box, lower extreme
a
a<-boxplot(combined$CO2)
a


#create a group and assign colors to group



### density plot - check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(combined$CO2), main="Density Plot: pCO2", ylab="Density", sub=paste("Skewness:", round(e1071::skewness(combined$CO2), 2)))  # density plot for 'CO2'
polygon(density(combined$CO2), col="red")

combined[is.na(combined)] <-"" # make na values in pH""
combined[, 10] <- sapply(combined[, 10], as.numeric) # make all pH values numeric
plot(density(combined$pH,na.rm=T), main="Density Plot: pH", ylab="Density", sub=paste("Skewness:", round(e1071::skewness(combined$pH), 2)))  # density plot for 'pH'
polygon(density(combined$pH,na.rm=T), col="red")

#### Correlation
cor(combined$CO2, combined$pH, use = "complete.obs")  # calculate correlation between CO2 and pH. use = "complete.obs" will not use the NA values
#result= -0.7174108

#build linear model
linearMod <- lm(CO2 ~ pH, data=combined)  # build linear regression model on full data
print(linearMod)
#linear regression diagnostics
#pvalue - checking for statistical significance
summary(linearMod) # p<0.05, r^2=0.5147
#Calculate the t-statistic and p-values
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["pH", "Estimate"]  # get beta estimate for pH
std.error <- modelCoeffs["pH", "Std. Error"]  # get std.error for pH
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(combined)-ncol(combined))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
AIC(linearMod)
BIC(linearMod)

#predicting linear models
#create training and test data samples from orig data
set.seed(500)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(combined), 0.8*nrow(combined))  # row indices for training data
trainingData <- combined[trainingRowIndex, ]  # model training data
testData  <- combined[-trainingRowIndex, ]   # test data

#visualize scatter plot with linear/non linear relationship
ggplot(testData,aes(CO2,pH))+
  geom_point()+
  stat_smooth()+
  scale_y_log10()
############################################################

#Linear Regression
# Build the model
model <- lm(pH ~ CO2, data = trainingData)+

# Make predictions
predictions <- model %>% predict(testData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, testData$pH,na.rm = T),
  R2 = R2(predictions, testData$pH,na.rm=T))  # RMSE=0.07673875, R2=0.5343687
#Visualize the data
ggplot(trainingData, aes(CO2, pH) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)+
  scale_y_log10()

####################################################################

#polynomial regression - figure out to what order (6)
lm(pH ~ poly(CO2, 2, raw = TRUE), data = trainingData)
lm(pH ~ poly(CO2, 4, raw = TRUE), data = trainingData) %>%
  summary()

#redo model with polynomial to the 3rd order
model <- lm(pH ~ poly(CO2, 3, raw = TRUE), data = trainingData)
predictions <- model %>% predict(testData)
data.frame(
  RMSE = RMSE(predictions, testData$pH,na.rm = T),
  R2 = R2(predictions, testData$pH,na.rm=T))    #RMSE=0.06263471, R2=0.6888306
ggplot(trainingData, aes(CO2, pH) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 6, raw = TRUE))+
  scale_y_log10()

##################################################################################

#logarithm transformation of the predictor variables:
# Build the model
model <- lm(pH ~ log(CO2), data = trainingData)
# Make predictions
predictions <- model %>% predict(testData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, testData$pH,na.rm = T),
  R2 = R2(predictions, testData$pH,na.rm = T))      # RMSE=0.0673581, R2=0.6405185
ggplot(trainingData, aes(CO2, pH) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))+
  scale_y_log10()

#####################################################################################
#Spline regression
knots <- quantile(trainingData$CO2, p = c(0.25, 0.5, 0.75))
library(splines)
# Build the model
knots <- quantile(trainingData$CO2, p = c(0.25, 0.5, 0.75))
model <- lm (pH ~ bs(CO2, knots = knots), data = trainingData)
# Make predictions
predictions <- model %>% predict(testData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, testData$pH,na.rm = T),
  R2 = R2(predictions, testData$pH,na.rm = T))    # RMSE=0.06271599, R2=0.6880465
ggplot(trainingData, aes(CO2, pH) ) +
  geom_point() +
  scale_y_log10()+
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
  
####################################################################################
#Generalized additive models
library(mgcv)
# Build the model
model <- gam(pH ~ s(CO2), data = trainingData) #The term s(CO2) tells the gam() function to find the "best" knots for a spline term
# Make predictions
predictions <- model %>% predict(testData)
# Model performance
data.frame(
  RMSE = RMSE(predictions, testData$pH,na.rm=TRUE),
  R2 = R2(predictions, testData$pH,na.rm=TRUE))     #RMSE=0.06270392, R2=0.6881666
ggplot(trainingData, aes(CO2, pH) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
  

#######################################################################################





# Build the model on training data -
lmMod <- lm(CO2 ~ pH, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)
AIC (lmMod)  # Calculate akaike information criterion
#Calculate prediction accuracy and error rates
combined<-na.omit(combined)
actuals_preds <- data.frame(cbind(actuals=testData$CO2, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)
#calculate the Min Max accuracy and MAPE(mean absolute percentage deviation )                  
combined<-na.omit(combined)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

library(DAAG)
combined<-na.omit(combined)
cvResults <- suppressWarnings(CVlm(data=combined, form.lm=CO2 ~ pH, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 39568.29 mean squared error


