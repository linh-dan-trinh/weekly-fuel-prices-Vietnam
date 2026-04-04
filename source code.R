# Check the working directory
getwd() #where are you working?

# Use the menu to change the wd:
# Session --> Set Working directory --> Choose directory
# Choose the FOLDER where your file is stored
getwd() #check that you are now in the right folder
dir() #list of the files available in the wd

# Import the data

#This file contains data about the following assets:
#Diesel 0.005% sulfur	#Diesel 0.001% sulfur  	#Fuel Oil 3% sulfur	
#Fuel Oil 3.5% sulfur	#Kerosene	#RON 95 gasoline #E5 Biofuel with RON 92	
#E10 Biofuel with RON 95


class(mydata) #this is an object of class data frame
head(mydata) #first lines of the data frame
tail(mydata) #last lines of the data frame
nrow(mydata)
ncol(mydata)
colnames(mydata) #the variables names

###################################################
####### Select data in the data frame
####### Produce some plots
####### Create new variables
###################################################
# The selection is done with the square parentheses as usual.
# In this case we  have to specify a double index (one for row and one for column)
mydata[1,2] # price for the first day

# To select a specific row  (and all the columns)
mydata[1,] #all the prices for the first day

# To select a specific column  (and all the rows)
mydata[,2]  #second column (all the daily data for AMZN)
mydata[, c(2,3)] #second and third column
mydata[, 2:3] #short alternative to the previous command

mydata$Date = as.Date(mydata$Date,format = "%d/%m/%Y")
str(mydata)
max(mydata$RON.95.gasoline)


# Let's start plotting the data

plot(mydata$Date,mydata$Diesel.0.005..sulfur,type="l", lwd = 2,col=1,
     xlab = "Date", ylab = "Price (VND/Liter)", ylim=c(0,35000),
     main = "Fuel Price Trends")
lines(mydata$Date,mydata$Diesel.0.001..sulfur,lwd = 2, col=2)
lines(mydata$Date,mydata$Fuel.Oil.3..sulfur, lwd = 2, col=3)
lines(mydata$Date,mydata$Fuel.Oil.3.5..sulfur,lwd = 2, col=4)
lines(mydata$Date,mydata$Kerosene,lwd = 2, col=5)
lines(mydata$Date,mydata$RON.95.gasoline,lwd = 2, col=6)
lines(mydata$Date,mydata$E5.Biofuel.with.RON.92,lwd = 2, col=7)
lines(mydata$Date,mydata$E10.Biofuel.with.RON.95,lwd = 2, col=8)


legend = c("Diesel 0.005% sulfur","Diesel 0.001% sulfur",
           "Fuel Oil 3% sulfur",	"Fuel Oil 3.5% sulfur",
           "Kerosene", "RON 95 gasoline", 
           "E5 Biofuel with RON 92", "E10 Biofuel with RON 95")
legend  ("topright", legend, col = c(1,2,3,4,5,6,7,8),
         inset = c(-0.18, -0.02),xpd = FALSE,
         lty = 1, lwd = 2,  cex = 0.5, bty = "n")
##############

ron95_prices <- as.numeric(mydata$RON.95.gasoline)
ron95_prices <- ron95_prices[!is.na(ron95_prices) & ron95_prices > 0]
ron95_density <- density(ron95_prices)
hist(ron95_prices, probability = TRUE,
     breaks = "FD",
     ylim = c(0, max(ron95_density$y) * 1.1),
     main = "Distribution of RON 95 Gasoline Prices",
     xlab = "Price (VND/Liter)", col = "lightblue", border = "black")
lines(ron95_density, col = "darkgreen", lwd = 2)

diesel05_prices <- as.numeric(mydata$Diesel.0.005..sulfur)
diesel05_prices <- diesel05_prices[!is.na(diesel05_prices) & diesel05_prices > 0]
diesel05_density <- density(diesel05_prices)
hist(diesel05_prices, probability = TRUE,
     breaks = "FD",
     ylim = c(0, max(diesel05_density$y) * 1.1),
     main = "Distribution of Diesel 0.005% Sulfur Prices",
     xlab = "Price (VND/Liter)", col = "lightblue", border = "black")
lines(diesel05_density, col = "darkgreen", lwd = 2)

kerosene_prices <- as.numeric(mydata$Kerosene)
kerosene_prices <- kerosene_prices[!is.na(kerosene_prices) & kerosene_prices > 0]
kerosene_density <- density(kerosene_prices)
hist(kerosene_prices, probability = TRUE,
     breaks = "FD",
     ylim = c(0, max(kerosene_density$y) * 1.1),
     main = "Distribution of Kerosene Prices",
     xlab = "Price (VND/Liter)", col = "lightblue", border = "black")
lines(kerosene_density, col = "darkgreen", lwd = 2)

e5ron92_prices <- as.numeric(mydata$E5.Biofuel.with.RON.92)
e5ron92_prices <- e5ron92_prices[!is.na(e5ron92_prices) & e5ron92_prices > 0]
e5ron92_density <- density(e5ron92_prices)
hist(e5ron92_prices, probability = TRUE,
     breaks = "FD",
     ylim = c(0, max(e5ron92_density$y) * 1.1),
     main = "Distribution of E5 Biofuel RON 92 Gasoline Prices",
     xlab = "Price (VND/Liter)", col = "lightblue", border = "black")
lines(e5ron92_density, col = "darkgreen", lwd = 2)

# Overlay a fitted normal curve to compare with the empirical density.
ron95_mean <- mean(ron95_prices)
ron95_sd <- sd(ron95_prices)
curve(dnorm(x, mean = ron95_mean, sd = ron95_sd),
      add = TRUE, col = "firebrick", lwd = 2, lty = 2)
legend("topright",
       legend = c("Kernel density", "Fitted normal"),
       col = c("darkgreen", "firebrick"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")

# Distribution diagnostics: skewness, excess kurtosis, and Jarque-Bera.
ron95_n <- length(ron95_prices)
ron95_skew <- mean((ron95_prices - ron95_mean)^3) / (ron95_sd^3)
ron95_excess_kurt <- mean((ron95_prices - ron95_mean)^4) / (ron95_sd^4) - 3
jb_stat <- (ron95_n / 6) * (ron95_skew^2 + 0.25 * ron95_excess_kurt^2)
jb_p_value <- 1 - pchisq(jb_stat, df = 2)

cat("RON95 distribution diagnostics\n")
cat("N:", ron95_n, "\n")
cat("Mean:", round(ron95_mean, 2), " Median:", round(median(ron95_prices), 2), "\n")
cat("Skewness:", round(ron95_skew, 4), "\n")
cat("Excess kurtosis:", round(ron95_excess_kurt, 4), "\n")
cat("Jarque-Bera statistic:", round(jb_stat, 4), " p-value:", signif(jb_p_value, 4), "\n")

# Q-Q plot against normal distribution.
qqnorm(ron95_prices,
       main = "Q-Q Plot: RON 95 Gasoline Prices",
       pch = 19,
       col = rgb(0.1, 0.2, 0.8, 0.5))
qqline(ron95_prices, col = "firebrick", lwd = 2)




#Data frequency seems inconsistent
diff_days <- diff(mydata$Date)
table(diff_days) #results showed 17 different time gaps 

#create a full weekly timeline
all_dates <- seq(min(mydata$Date), max(mydata$Date), by = "7 days")

library(dplyr)
mydata_monthly <- mydata %>%
  mutate(month = format(Date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(Date = as.Date(paste0(month, "-15")))

plot(mydata$Date, mydata$RON.95.gasoline, type = "l", col = "red", lwd = 2,
     main = "Original vs Transformed Data",
     ylab = "Price (VND/l)", xlab = "Date")
lines(mydata_monthly$Date, mydata_monthly$RON.95.gasoline, col = "blue", lwd = 2)
legend("topleft", legend = c("Original", "Interpolated (monthly)"),
       col = c("red", "blue"), lty = 1, bty = "n")

mean(diff(mydata$Date))
mean(diff(mydata_monthly$Date))

mean(is.na(mydata_monthly$RON.95.gasoline)) #>20–30% of rows were missing before
#interpolation → your interpolation probably dominated the pattern


library(readr)
library(dplyr)
str(mydata$Date)

#check data completeness
colSums(mydata == 0, na.rm = TRUE) #Diesel 0.001%S got 113 data missing
#248 for Fuel Oil 3.5% #235 for E10 Biofuel with RON95

#filter missing data
# Compute completeness ratio (non-zero / total)
valid_cols <- names(mydata)[colMeans(mydata == 0 | is.na(mydata)) < 0.3]

# Keep Date + valid columns
mydata_clean <- mydata_monthly %>% select(Date, all_of(valid_cols))

head(mydata_clean)

mydata_clean <- mydata_clean %>%
  rowwise() %>%
  mutate(Average_Fuel_Price = mean(c_across(-Date)[c_across(-Date) > 0], na.rm = TRUE)) %>%
  ungroup()
head(select(mydata_clean, Date, Average_Fuel_Price))

library(ggplot2)

ggplot(mydata_clean, aes(x = Date)) +
  geom_line(aes(y = `Diesel.0.005..sulfur`, color = "Diesel 0.005% sulfur")) +
  geom_line(aes(y = `E5.Biofuel.with.RON.92`, color = "E5 Biofuel with RON 92")) +
  geom_line(aes(y = `Kerosene`, color = "Kerosene")) +
  geom_line(aes(y = `RON.95.gasoline`, color = "RON 95 gasoline")) +
  geom_line(aes(y = Average_Fuel_Price, color = "Average Fuel Price"), linewidth = 1.2) +
  labs(title = "Fuel Prices and Average (VND/Liter)",
       y = "Price (VND/Liter)", color = "Fuel Type") +
  theme_minimal()

ggplot(mydata_clean, aes(x = Date, y = Average_Fuel_Price)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Average Retail Fuel Price Over Time",
       y = "Average Price (VND/Liter)", x = "Date") +
  theme_minimal()


#Make sure data are properly ordered (important for differences)
mydata_clean <- mydata_clean %>% arrange(Date)

#Calculate MoM and YoY changes
mydata_clean <- mydata_clean %>%
  mutate(
    MoM_change = (Average_Fuel_Price - lag(Average_Fuel_Price)) / lag(Average_Fuel_Price) * 100,
    YoY_change = (Average_Fuel_Price - lag(Average_Fuel_Price, 12)) / lag(Average_Fuel_Price, 12) * 100
  )
head(select(mydata_clean, Date, Average_Fuel_Price, MoM_change, YoY_change))

lag(mydata_clean$Average_Fuel_Price)

ggplot(mydata_clean, aes(x = Date, y = MoM_change)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Month-on-Month Change in Average Fuel Price",
       y = "Change (%)", x = "Date") +
  theme_minimal()

ggplot(mydata_clean, aes(x = Date, y = YoY_change)) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Year-on-Year Change in Average Fuel Price",
       y = "Change (%)", x = "Date") +
  theme_minimal()

#Volatility
install.packages("zoo")
library(zoo)

mydata_clean <- mydata_clean %>%
  arrange(Date) %>%
  mutate(Volatility_3M = rollapply(Average_Fuel_Price,width = 3,
                                   FUN = sd,align = "right",fill = NA ) )

ggplot(mydata_clean, aes(x = Date, y = Volatility_3M)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "3-Month Rolling Volatility of Average Fuel Price",
       y = "Standard Deviation (VND/Liter)", x = "Date") +
  theme_minimal()

mydata_clean <- mydata_clean %>%
  arrange(Date) %>%
  mutate(Volatility_6M = rollapply(Average_Fuel_Price,width = 6,
                                   FUN = sd,align = "right",fill = NA ) )

ggplot(mydata_clean, aes(x = Date, y = Volatility_6M)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "6-Month Rolling Volatility of Average Fuel Price",
       y = "Standard Deviation (VND/Liter)", x = "Date") +
  theme_minimal()

mydata_clean <- mydata_clean %>%
  arrange(Date) %>%
  mutate(Volatility_1Y = rollapply(Average_Fuel_Price,width = 12,
                                   FUN = sd,align = "right",fill = NA ) )

ggplot(mydata_clean, aes(x = Date, y = Volatility_1Y)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "1-Year Rolling Volatility of Average Fuel Price",
       y = "Standard Deviation (VND/Liter)", x = "Date") +
  theme_minimal()

#Correlation matrix
library(corrplot)
corr_matrix <- cor(mydata_clean[, c("Diesel.0.005..sulfur", "Kerosene", 
                                    "RON.95.gasoline", "E5.Biofuel.with.RON.92")],
                   use = "pairwise.complete.obs")

corrplot(corr_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

library(dplyr)

# Make sure 'date' is numeric for regression
mydata$resid_diesel   <- resid(lm(Diesel.0.005..sulfur ~ as.numeric(Date), data = mydata))
mydata$resid_kerosene <- resid(lm(Kerosene ~ as.numeric(Date), data = mydata))
mydata$resid_ron95    <- resid(lm(RON.95.gasoline ~ as.numeric(Date), data = mydata))
mydata$resid_e5       <- resid(lm(E5.Biofuel.with.RON.92 ~ as.numeric(Date), data = mydata))

residuals_df <- mydata[, c("resid_diesel", "resid_kerosene",
                           "resid_ron95", "resid_e5")]
pairs(
  residuals_df,
  main = "Pairwise Scatterplots of Fuel Price Residuals",
  pch = 19,
  col = rgb(0, 0, 1, 0.4))

#Check multicollinearity
library(car)
fuel_vars <- mydata_clean[, c("Diesel.0.005..sulfur", "Kerosene", "RON.95.gasoline", "E5.Biofuel.with.RON.92")]

# Fit a simple model with one target (say, RON95) and the others as predictors
model_vif <- lm(RON.95.gasoline ~ Diesel.0.005..sulfur + Kerosene + E5.Biofuel.with.RON.92, data = fuel_vars)

vif(model_vif)

#Test for Cointegration-Cointegration checks whether two (or more) 
#time series move together in the long run, 
#even if they fluctuate short term
library(urca)

# Create a time series matrix
fuel_ts <- na.omit(mydata_clean[, c("Diesel.0.005..sulfur", "Kerosene", "RON.95.gasoline", "E5.Biofuel.with.RON.92")])

# Johansen cointegration test (trace test)
coint_test <- ca.jo(fuel_ts, type = "trace", ecdet = "const", K = 2)
summary(coint_test)

#Build Predictive Model
# Simple linear model: predict RON95 based on Diesel
model_ron95 <- lm(RON.95.gasoline ~ Diesel.0.005..sulfur, data = mydata_clean)

summary(model_ron95)

# Predict values
mydata_clean$RON95_pred <- predict(model_ron95)

# Compare actual vs predicted
plot(mydata_clean$Date, mydata_clean$RON.95.gasoline, type = "l", col = "blue", lwd = 2,
     main = "RON95 vs Predicted (based on Diesel)", ylab = "Price (VND/L)")
lines(mydata_clean$Date, mydata_clean$RON95_pred, col = "red", lwd = 2)
legend("topleft", legend = c("Actual RON95", "Predicted RON95 (Diesel-based)"),
       col = c("blue", "red"), lty = 1, bty = "n")

#check model fitness
summary(model_ron95)$r.squared #R² > 0.8 → Diesel explains 80%+ of RON95 variation.

fuel_baseline <- subset(mydata_clean, RON.95.gasoline >= 15000 
                        & RON.95.gasoline <= 25000)
fuel_stress <- subset(mydata_clean, RON.95.gasoline < 15000 |
                        RON.95.gasoline > 25000)

summary(fuel_baseline[, c("Diesel.0.005..sulfur", "Kerosene", "RON.95.gasoline",
                          "E5.Biofuel.with.RON.92")])
summary(fuel_stress[, c("Diesel.0.005..sulfur", "Kerosene", "RON.95.gasoline",
                        "E5.Biofuel.with.RON.92")])