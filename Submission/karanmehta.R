# Final Assignment 1 - Statistical Analysis
library(dplyr)
library(lubridate) 
library(stringr)
library(ggplot2)
library(lmtest)

standard_cols <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

data2016 <- read.csv("D://University of Chicago//Courses///ADSP 31007 IP06 - Statistical Analysis//Final Assignment//2016_brooklyn.csv", skip = 4, header = TRUE, col.names = standard_cols)
data2017 <- read.csv("D://University of Chicago//Courses///ADSP 31007 IP06 - Statistical Analysis//Final Assignment//2017_brooklyn.csv", skip = 4, header = TRUE, col.names = standard_cols)
data2018 <- read.csv("D://University of Chicago//Courses///ADSP 31007 IP06 - Statistical Analysis//Final Assignment//2018_brooklyn.csv", skip = 4, header = TRUE, col.names = standard_cols)
data2019 <- read.csv("D://University of Chicago//Courses///ADSP 31007 IP06 - Statistical Analysis//Final Assignment//2019_brooklyn.csv", skip = 4, header = TRUE, col.names = standard_cols)
data2020 <- read.csv("D://University of Chicago//Courses///ADSP 31007 IP06 - Statistical Analysis//Final Assignment//2020_brooklyn.csv", skip = 6, header = TRUE, col.names = standard_cols)

#Combining the Data into one dataset
combined_data <- rbind(data2016, data2017, data2018, data2019, data2020)

combined_data$date <- mdy(combined_data$date)

combined_data %>% group_by(landsqft) %>% count()
combined_data %>% group_by(grosssqft) %>% count()
combined_data %>% group_by(yrbuilt) %>% count()
combined_data %>% group_by(price) %>% count()
combined_data %>% group_by(date) %>% count()
combined_data %>% group_by(comunits) %>% count()
combined_data %>% group_by(resunits) %>% count()
combined_data %>% group_by(totunits) %>% count()


#Cleaning Columns after combining data [removing white spaces, replacing "-" with NA and converting datatype to numeric]
combined_data$neighborhood <- trimws(combined_data$neighborhood)

combined_data$resunits <- trimws(combined_data$resunits)
combined_data$resunits[combined_data$resunits == "-"] <- NA
combined_data$resunits <- as.numeric(combined_data$resunits)

combined_data$comunits <- trimws(combined_data$comunits)
combined_data$comunits[combined_data$comunits == "-"] <- NA
combined_data$comunits <- as.numeric(combined_data$comunits)

combined_data$totunits <- trimws(combined_data$totunits)
combined_data$totunits[combined_data$totunits == "-"] <- NA
combined_data$totunits <- as.numeric(combined_data$totunits)

combined_data$landsqft <- trimws(combined_data$landsqft)
combined_data$landsqft[combined_data$landsqft == "-"] <- NA
combined_data$landsqft <- as.numeric(gsub("[,]","", combined_data$landsqft))

combined_data$grosssqft <- trimws(combined_data$grosssqft)
combined_data$grosssqft[combined_data$grosssqft == "-"] <- NA
combined_data$grosssqft <- as.numeric(gsub("[,]","", combined_data$grosssqft))

combined_data$price <- trimws(combined_data$price)
combined_data$bldclasssale <- trimws(combined_data$bldclasssale)

#Filtering Data to select only single family homes, Condos, Totunits = 1, Resunits = 1, grosssqft > 0 and no considering NA price values
filtered_data <- combined_data %>%
  filter(str_detect(bldclasssale, "[A|R]"),
         totunits == 1,
         resunits == 1, 
         grosssqft > 0,
         !is.na(price)
  )

#Cleaning Price column to remove $ Sign and ,
filtered_data$price[filtered_data$price == '-'] <- NA
filtered_data$price <- gsub("[$,]","", filtered_data$price)
filtered_data$price <- as.numeric(filtered_data$price)

na_count <- sum(is.na(filtered_data$price))
filtered_data <- filtered_data[!is.na(filtered_data$price), ]

# Counting occurrences of 0 values in the 'price' column
zero_count <- sum(filtered_data$price == 0, na.rm = TRUE)
filtered_data <- filtered_data[filtered_data$price != 0, ]

filtered_data$year <- year(filtered_data$date)
filtered_data$BuildingAgeatSale <- year(filtered_data$date) - filtered_data$yrbuilt

filtered_data$quarter <- paste("Q", quarter(filtered_data$date), "-", format(filtered_data$date, "%Y"), sep = "")

#Box plot for price
ggplot(filtered_data, aes(y = filtered_data$price)) +
  geom_boxplot() +
  ylab("Price") + 
  ggtitle("Boxplot of Price")

#Scatter plot of Price vs GrossSqft
ggplot(filtered_data, aes(x = grosssqft, y = price)) +
  geom_point() +
  labs(
    x = "Gross Square Feet",
    y = "Price",
    title = "Scatter Plot of Price vs Gross Square Feet"
  )


#Scatter plot of Price vs Building Age
ggplot(filtered_data, aes(x = BuildingAgeatSale, y = price)) +
  geom_point() +
  labs(
    x = "Age of Building",
    y = "Price",
    title = "Scatter Plot of Price vs Building Age"
  )

#Scatter plot of Price vs Year
ggplot(filtered_data, aes(x = year, y = price)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Price",
    title = "Distribution of Price across Years"
  )

# It can be observed from this scatter plot that there are a few apartments which don't have data for when the building was made.
# Hence they are standing out with Building age 2018 which is an outlier in the graph.


unique(filtered_data$neighborhood)

#Filtering neighborhood based on similarity and proximity to each other
filtered_data <- filtered_data %>%
  mutate(Neighborhood_Category = case_when(
    neighborhood %in% c("WILLIAMSBURG-CENTRAL", "WILLIAMSBURG-EAST", "WILLIAMSBURG-NORTH", "WILLIAMSBURG-SOUTH") ~ "Williamsburg",
    neighborhood %in% c("PARK SLOPE", "PARK SLOPE SOUTH") ~ "Park Slope",
    neighborhood %in% c("COBBLE HILL", "COBBLE HILL-WEST") ~ "Cobble Hill",
    neighborhood %in% c("DOWNTOWN-FULTON FERRY", "DOWNTOWN-METROTECH", "DOWNTOWN-FULTON MALL", "CARROLL GARDENS","NAVY YARD","FORT GREENE","BOERUM HILL") ~ "Downtown",
    neighborhood %in% c("BERGEN BEACH", "BRIGHTON BEACH", "GERRITSEN BEACH", "MANHATTAN BEACH", "BATH BEACH") ~ "Beach",
    neighborhood %in% c("MILL BASIN", "OLD MILL BASIN") ~ "Mill Basin",
    neighborhood %in% c("OCEAN PARKWAY-NORTH", "OCEAN PARKWAY-SOUTH") ~ "Ocean Parkway",
    neighborhood %in% c("CROWN HEIGHTS") ~ "CROWN HEIGHTS",
    neighborhood %in% c("KENSINGTON") ~ "KENSINGTON",
    neighborhood %in% c("WINDSOR TERRACE") ~ "WINDSOR TERRACE",
    neighborhood %in% c("BROWNSVILLE") ~ "BROWNSVILLE",
    neighborhood %in% c("CANARSIE") ~ "CANARSIE",
    neighborhood %in% c("CYPRESS HILLS") ~ "CYPRESS HILLS",
    neighborhood %in% c("EAST NEW YORK") ~ "EAST NEW YORK",
    neighborhood %in% c("SPRING CREEK") ~ "SPRING CREEK",
    neighborhood %in% c("BEDFORD STUYVESANT") ~ "BEDFORD STUYVESANT",
    neighborhood %in% c("BUSHWICK") ~ "BUSHWICK",
    neighborhood %in% c("GREENPOINT") ~ "GREENPOINT",
    neighborhood %in% c("OCEAN HILL") ~ "OCEAN HILL",
    neighborhood %in% c("WYCKOFF HEIGHTS") ~ "WYCKOFF HEIGHTS",
    neighborhood %in% c("CONEY ISLAND") ~ "CONEY ISLAND",
    neighborhood %in% c("FLATLANDS") ~ "FLATLANDS",
    neighborhood %in% c("GRAVESEND") ~ "GRAVESEND",
    neighborhood %in% c("MADISON") ~ "MADISON",
    neighborhood %in% c("MARINE PARK") ~ "MARINE PARK",
    neighborhood %in% c("MIDWOOD") ~ "MIDWOOD",
    neighborhood %in% c("SEAGATE") ~ "SEAGATE",
    neighborhood %in% c("SHEEPSHEAD BAY") ~ "SHEEPSHEAD BAY",
    neighborhood %in% c("BUSH TERMINAL") ~ "BUSH TERMINAL",
    neighborhood %in% c("BROOKLYN HEIGHTS") ~ "BROOKLYN HEIGHTS",
    neighborhood %in% c("CLINTON HILL") ~ "CLINTON HILL",
    neighborhood %in% c("GOWANUS") ~ "GOWANUS",
    neighborhood %in% c("PROSPECT HEIGHTS") ~ "PROSPECT HEIGHTS",
    neighborhood %in% c("RED HOOK") ~ "RED HOOK",
    neighborhood %in% c("BAY RIDGE") ~ "BAY RIDGE",
    TRUE ~ "Other"
  ))

summary(filtered_data$price)

#Removing the outliers on extreme ends
filtered_data <- filtered_data %>%filter(price >= 110000 & price <= 6850000)
nrow(filtered_data)

hist(filtered_data$price)

#Now Adding blocks into buckets
filtered_data <- filtered_data %>%
  mutate(block_bucket = cut(block, breaks = seq(0, 9000, by = 500), labels = FALSE))

#Best Running Model
model_f <- lm(price ~  BuildingAgeatSale + block_bucket*grosssqft + factor(Neighborhood_Category), data = filtered_data)
summary(model_f)

RMSE_value <- sqrt(mean((model_f$residuals)^2))
print(paste("RMSE =", round(RMSE_value, 2)))

# Selecting variables based on relevance to the price of real estate (after testing with multiple variables)
# *Rows used for model execution = 13413, Adj R-squared = 0.6332, Df = 39, RMSE = 449790.83*

filtered_data$predicted_prices <- predict(model_f)

#Plotting the regression model
ggplot(filtered_data, aes(x = price, y = predicted_prices)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Observed Prices",
    y = "Predicted Prices",
    title = "Regression Model"
  )

#Checking for the IID Assumptions
#Testing for autocorrelation (independence)
dwtest(model_f)
# 
# Check if the residuals are normally distributed
ks.test(model_f$residuals/summary(model_f)$sigma,pnorm)
# 
# Testing for Heteroskedasticity - Breusch-Pagan Test
bptest(model_f)

#Saving files into folder
write.csv(filtered_data, "D://University of Chicago//Courses//ADSP 31007 IP06 - Statistical Analysis//Final Assignment//Submission//filtered_data.csv", row.names = FALSE)
saveRDS(list(model=model_f, filtered_data), file='D://University of Chicago//Courses//ADSP 31007 IP06 - Statistical Analysis//Final Assignment//Submission//karanmehta.RDS') 

#**************************Models implemented before selecting final*********************************#
# model1 <- lm(log(price) ~ (grosssqft-landsqft)^2 + block*lot + BuildingAgeatSale + Neighborhood_Category, data = filtered_data)
# summary(model1)

# sqrt(mean(model1$residuals^2))

# model2 <- lm(log(price) ~ (grosssqft-landsqft) + (block*lot) + BuildingAgeatSale + factor(Neighborhood_Category), data = filtered_data)
# summary(model2)

# model3 <- lm(sqrt(price) ~ (grosssqft-landsqft)^2 + (block*lot) + BuildingAgeatSale + factor(Neighborhood_Category), data = filtered_data)
# summary(model3)

# model4 <- lm(price ~ (grosssqft-landsqft) + lot + BuildingAgeatSale + factor(Neighborhood_Category), data = filtered_data)
# summary(model4)
# RMSE_value <- sqrt(mean(model4$residuals^2))
# print(paste("RMSE =", round(RMSE_value, 2)))

# model5 <- lm(price ~ grosssqft + lot + BuildingAgeatSale + Neighborhood_Category + zip, data = filtered_data)
# summary(model5)

# filtered_data <- filtered_data %>%
#   mutate(block_bucket = cut(block, breaks = seq(0, 9000, by = 1000), labels = FALSE))
# 
# model6 <- lm(price ~ grosssqft + (block_bucket*lot) + BuildingAgeatSale + factor(Neighborhood_Category), data = filtered_data)
# summary(model6)
# RMSE_value <- sqrt(mean(model6$residuals^2))
# print(paste("RMSE =", round(RMSE_value, 2)))

# Variable LandSqft doesn't have any impact on the model.

#Second best running model [without binning blocks]
# model_t <- lm(price ~ grosssqft + block*lot + BuildingAgeatSale + factor(Neighborhood_Category), data = filtered_data)
# summary(model_t)
# 
# RMSE_value <- sqrt(mean(model_t$residuals^2))
# print(paste("RMSE =", round(RMSE_value, 2)))

#*********************************************************************************************#
