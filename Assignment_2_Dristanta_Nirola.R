#Author@Dristanta Nirola
#Time_series_assignment_2

#************************************************* Assignment 2 ***********************************************************
## Read the data and save as a time series object.
df= read.csv('D:/ISI 2019-2020/Class 2nd semester/t-series/Project/monthly_data_total.csv')
df=df[1:165,]
colnames(df)=c("Date","Import","Export")
View(df)

vec = df$Import
TS=ts(as.numeric(vec),frequency=12,start=c(2006,1),end = c(2019,9))

len= length(vec)
print(len)

################################################Step 1: plotting x(t) ######################################################
plot(TS,lwd="3",col="Blue",main="India's Monthly Imports(Jan 2006 - Sep 2019)", ylab="Total Import in US Billion $")

#############################################Step 2: Deseasonalisation######################################################

## Using decompose function to plot the trend, seasonal and the random components from the given series. 
dcompose_series=decompose(TS)
plot(dcompose_series,lwd="3",col="blue")

# subtracting the seasonal component from the actual series to get the deseasonalised series
deseasonal_xt = TS- dcompose_series$seasonal
plot(deseasonal_xt,col="Blue", type='o', main = "India's Monthly Imports(Jan:2006 - Sep:2019)",xlab = "Time", ylab = "Total Import in US Billion $")

########################################Step 3:Checking stationary:ADF Test #################################################

# applying ADF function on the deseasonlaised series for checking stationary
require(urca)
adf_test=summary(ur.df(deseasonal_xt,type = c("trend"),lags=20,selectlags = c("AIC")))
adf_test

##Hypothesis:Since, the test statistics is -2.217 and the critical value  at 5% is -3.43 so we fail to reject the null hypothesis and 
#conclude that our series is non stationary ##Cancer is found


# Removing Cancer: Applying 1st differencing xt= x(t)-x(t-1)
yt= diff(deseasonal_xt)

### applying adf test on yt
adf_test_yt=summary(ur.df(yt,type=c("trend"),lags = 20,selectlags = c("AIC")))
adf_test_yt

##Hypothesis:Since, the test statistics is -10.0701 and the critical value  at 5% is -3.43 thus we reject the null hypothesis and 
#conclude that our series yt is stationary ##Cancer is removed

### testing for trend in stationary series yt
t=seq(1:(len-1))
trend_test=summary(lm(yt~t))
trend_test

##Hypothesis:Since, the p value 0.6679 is greater than 0.05 i.e. we fail to reject the null hypothesis and conclude that
#there is no trend present in the series now our series yt is free from both cancer and fever

#####################################################Step 4: ACF Test##################################################

## Plot the ACF of deseasonal series with 95 % confidence limits.
acf_value= acf(deseasonal_xt,lag.max=20,plot=F)
plot(acf_value, main = "ACF plot (lag=20)")


## use Ljung-Box for finding the q statistics for ACF test

# Determine critical region at DF=lag.
chisqr=c()
for (k in 1:20){
  chisqr= append(chisqr,qchisq(0.95,k))
}
chisqr= as.numeric(chisqr)

# Calculating the test statistics
q_statistics=c()
p_value=c()
for (i in 1:20){
  BT=Box.test(yt,lag=i,type="Ljung-Box")
  q_statistics[i]=as.numeric(BT$statistic)
# p_value[i]=BT$p.value
}
print(q_statistics)

# Checking test statistics for conclusion
result=ifelse(q_statistics>chisqr,"Rejected","Accepted")
acf_check_table=data.frame(acf_value$acf[1:20],q_statistics,chisqr, result)
acf_check_table
write.csv(acf_check_table,"Acf_check_table_xt.csv")

###################################################Step 5: Sample PACF ############################################

## Plot the PACF of differenced series with 95 % confidence limits.
pacf_val= pacf(deseasonal_xt,lag.max=20,plot=F)
plot(pacf_val, main = "PACF plot (lag=20)")

# checking pacf test statistic for conclusion
pacf_statistics=sqrt(len)* as.numeric(pacf_val$acf)
result_1=ifelse(abs(pacf_statistics)>1.96,"Rejected","Accepted")
result_1
pacf_check_table=data.frame(pacf_val$acf[1:20],abs(pacf_statistics), result_1)
View(pacf_check_table)
write.csv(pacf_check_table,"pacf_check_table_xt.csv")
###Conclusion: Since, ACF is diminishing and pacf cuts i.e. becoming insignificant from 3 onwards so our model is AR(2)

###############################################Step 6: Estimation ##################################################

## Fit a AR model to original series. The arima function will perform the necessary differences.
AR = arima(yt, order = c(2, 0, 0), seasonal=list(order=c(2,1,0), period=12))
AR

#######################################Step 7: Box-Ljung test for residuals ########################################

## Use the Box-Ljung test to determine if the residuals are 
## random up to 20 lags.
acf_res = acf(AR$residuals, plot = T)


# Finding the test statistics 
q_statistics=c()
p_value=c()
for (i in 1:20){
  BT=Box.test(AR$residuals,lag=i,type="Ljung-Box",fitdf=2)
  q_statistics[i]=as.numeric(BT$statistic)
  #p_value[i]=q$p.value
}
chisqr_res=c(NA,NA,chisqr[1:18])
 
#checking the test statistics
result=ifelse(q_statistics>chisqr_res,"Rejected","Accept")
acf_check_table=data.frame(acf_res$acf[1:20],q_statistics, chisqr_res, result)
write.csv(acf_check_table,"res_Acf_check_table_xt.csv")
#Conclusion: All the test statistics couldn't be rejected thus the residuals are the White Noise

##########################################Step 8: Model Fitting######################################################

# Fit a AR model to original series after all the parameter estimation for future forecasting.
## The arima function will perform the necessary differences.
AR = arima(TS, order = c(2, 1, 0), seasonal=list(order=c(2,1,0), period=12))
AR

##########################################Step 9: Forecasting#######################################################

## forecasting 
fore_arima = forecast::forecast(AR,12)
df_arima = as.data.frame(fore_arima)
fore_values = df_arima$`Point Forecast`

#plotting the forecasts values
plot(fore_arima,ylab="Total Import billion $", xlab="Observation", col="black",
     main="12 Months Forecasts from ARIMA(2,1,0)")

View(df_arima)
print(fore_values)
write.csv(df_arima,"forecast_values_table.csv")
#******************************************* THE END *****************************************************************