### import the dataset
fertilizer_data <- read.csv("Fertilizer_Soil_Data.csv")
fertilizer_data

### view the dataset
View(fertilizer_data)
### check the data type
str(fertilizer_data)

### binding the dataset with  environment
attach(fertilizer_data)
### view the dataser
View(fertilizer_data)
###check the null values
fertilizer_data[!complete.cases(fertilizer_data),]

### Preparing the data sets( we calles as data preprocessing)
install.packages('plyr')
library('plyr')
fertilizer_data$Crop_Type <- revalue(Crop_Type,c( Wheat=7,Rice=6,Corn=5,Soybean=4, Barley=3,Oats=2,Potato=1))
write.csv(fertilizer_data,"fertilizer_data_New.csv")

### detach the fertilizer_data dataset
detach(fertilizer_data)


### import the fertilizer_data_New dataset
fertilizer_data_New <- read.csv("fertilizer_data_New.csv")
fertilizer_data_New
### attach the fertilizer_data_New dataset
attach(fertilizer_data_New)
### View the data of new dataset
View(fertilizer_data_New)


### Normality testing
## import the nortest package
install.packages("nortest")

## call the library
library("nortest")
### Anderson darling testing for Crop_Yield
ad.test(Crop_Yield)
### Lillifor testing for Crop_Yield
lillie.test(Crop_Yield)
### Shapiro-wiki testing for Crop_Yield
shapiro.test(Crop_Yield)
### summary statistics
summary(Crop_Yield)
#### Graphical Analysis for Crop_Yield
hist(Crop_Yield, main="Crop_Yield Distribution",
     xlab="Crop_Yield Category",
     ylab="Crop_Yield",
     prob=TRUE)
curve(dnorm(x, mean=mean(Crop_Yield, na.rm=TRUE), 
            sd=sd(Crop_Yield, na.rm =TRUE)), add=TRUE)

### quintile plot for Crop_Yield
library('Rcmdr')
with(fertilizer_data_New, qqPlot(Crop_Yield, dist="norm", 
      id=list(method="y", n=2, labels=rownames(fertilizer_data_New)), 
                xlab="Crop_Yield Category", ylab="Crop_Yield", 
                main="Crop_Yield Distribution"))

### Anderson darling testing for Crop_Type
ad.test(Crop_Type)


### Lillifor testing for Crop_Type
lillie.test(Crop_Type)

### shapiro-wilk normality testing for Crop_Type
shapiro.test(Crop_Type)

### Graphical analysis for Crop_Type
hist(Crop_Type, main="Crop_Type Distribution",
     xlab="Crop_Type Category",
     ylab="Crop_Type",
     prob=TRUE)
curve(dnorm(x, mean=mean(Crop_Type, na.rm=TRUE), 
            sd=sd(Crop_Type, na.rm =TRUE)), add=TRUE)
### summary statistics
summary(Crop_Type)



### Quantile comparison plot
library('Rcmdr')

with(fertilizer_data_New, qqPlot(Crop_Type, dist="norm", 
                  id=list(method="y",
                  n=2, labels=rownames(fertilizer_data_New)),
                  xlab="Crop_Type Category", 
                  ylab="Crop_Type", main="Crop_Type Distribution"))
### Anderson darling test for Fertilizer_Amount
ad.test(Fertilizer_Amount)

### Lillifor test for Fertilizer_Amount
lillie.test(Fertilizer_Amount)
### Shapiro wiki test for Fertilizer_Amount
shapiro.test(Fertilizer_Amount)
### Graphical analysis for Crop_Type
hist(Fertilizer_Amount, main="Fertilizer_Amount Distribution",
     xlab="Fertilizer_Amount Category",
     ylab="Fertilizer_Amount",
     prob=TRUE)
curve(dnorm(x, mean=mean(Fertilizer_Amount, na.rm=TRUE), 
            sd=sd(Crop_Type, na.rm =TRUE)), add=TRUE)
### summary statistics
summary(Fertilizer_Amount)

### Quantile comparison plot for fertilizer_amount
library('Rcmdr')
with(fertilizer_data_New, qqPlot(Fertilizer_Amount, dist="norm", 
      id=list(method="y", n=2, labels=rownames(fertilizer_data_New)), 
      xlab="Fertilize_Amount Category", ylab="Fertilize_Amount", 
      main="Fertilize_Amount Distribution"))
## Anderson darling test for Potassium-Level
ad.test(Potassium_Level)

## Lillifor test for potassium-Level
lillie.test(Potassium_Level)

## shapiro-wiki test for potassium_Level
shapiro.test(Potassium_Level)

## summary statistics of potassium_Level
summary(Potassium_Level)

### Graphical Analysis for Potassium_Level
hist(Potassium_Level, main="Potassium_Level Distribution",
     xlab="Potassium_Level Category",
     ylab="Potassium_Level",
     prob=TRUE)
curve(dnorm(x, mean=mean(Potassium_Level, na.rm=TRUE), 
            sd=sd(Potassium_Level, na.rm =TRUE)), add=TRUE)

### quntile plot for Potassium_Level
with(fertilizer_data_New, qqPlot(Potassium_Level, dist="norm", 
                                    id=list(method="y", n=2, labels=rownames(fertilizer_data_New)), 
                                   xlab="Potassium-Level Category", ylab="Potassium-Level", 
                                   main="Potassium Level Distribution"))

### Anderson Darling for Phosphorus_Level
ad.test(Phosphorus_Level)
### Lillifor test Phosphorus_Level
lillie.test(Phosphorus_Level)
### shapiro-wiki test for Phosphorus_Level
shapiro.test(Phosphorus_Level)
### summary  statistics of Phosphorus_Level
summary(Phosphorus_Level)
### Graphical analysis for Phosphorus_Level
hist(Phosphorus_Level, main="Phosphorus_Level Distribution",
     xlab="Phosphorus_Level Category",
     ylab="Phosphorus_Level",
     prob=TRUE)
curve(dnorm(x, mean=mean(Phosphorus_Level, na.rm=TRUE), 
            sd=sd(Phosphorus_Level, na.rm =TRUE)), add=TRUE)

### quantile comparison plot for Phosphorus_Level
with(fertilizer_data_New, qqPlot(Phosphorus_Level, dist="norm", 
      id=list(method="y", n=2, labels=rownames(fertilizer_data_New)), 
      xlab="Phosphrous_Level Category", ylab="Phosphorus_Level", 
      main="Phosphorus_Level Distribution"))
### Anderson darling test Nitrogen_Level
ad.test(Nitrogen_Level)
### Lillifor test for Nitrogen_Level
lillie.test(Nitrogen_Level)
### shaipro-wiki test for Nitrogen_Level
shapiro.test(Nitrogen_Level)
### summary  statistics of Nitrogen_Level
summary(Nitrogen_Level)
### Graphical Analysis for Nitrogen_Level
hist(Nitrogen_Level, main="Nitrogen_Level Distribution",
     xlab="Nitrogen_Level Category",
     ylab="Nitrogen_Level",
     prob=TRUE)
curve(dnorm(x, mean=mean(Nitrogen_Level, na.rm=TRUE), 
            sd=sd(Nitrogen_Level, na.rm =TRUE)), add=TRUE)
### quantile plot for Nitrogen_Level
library('Rcmdr')

with(fertilizer_data_New, qqPlot(Nitrogen_Level, dist="norm", 
                            id=list(method="y", n=2, labels=rownames(fertilizer_data_New)), 
                            xlab="Nitrogen_Level Category", ylab="Nitrogen_Level", 
                            main="Nitrogen_Level Distributionn"))

#### Anderson Darling test for Soil Moisture
ad.test(Soil_Moisture)

#### Lillifor test for Soil Moisture
lillie.test(Soil_Moisture)
#### Shapiro-wiki for Soil Moisture
shapiro.test(Soil_Moisture)
### summary statistics
summary(Soil_Moisture)
### Graphicl Analysis for Soil_Moisture
hist(Soil_Moisture, main="Soil_Moisture Distribution",
     xlab="Soil_Moisture Category",
     ylab="Soil_Moisture",
     prob=TRUE)
curve(dnorm(x, mean=mean(Soil_Moisture, na.rm=TRUE), 
            sd=sd(Soil_Moisture, na.rm =TRUE)), add=TRUE)

### quantile plot for Soil_Moisture
with(fertilizer_data_New, qqPlot(Soil_Moisture, dist="norm", id=list(method="y", n=2, 
        labels=rownames(fertilizer_data_New)), xlab="Soil_Moisture Category", 
        ylab="Soil_Moisture", main="Soil_Moisture Distributionn"))


#### Anderson Darling test for Soil_pH
ad.test(Soil_pH)

#### Lillifor test for Soil_pH
lillie.test(Soil_pH)
#### Shapiro-wiki for Soil_pH
shapiro.test(Soil_pH)
### summary statistics
summary(Soil_pH)
### Graphicl Analysis for Soil_pH
hist(Soil_pH, main="Soil_pH Distribution",
     xlab="Soil_pH Category",
     ylab="Soil_pH",
     prob=TRUE)
curve(dnorm(x, mean=mean(Soil_pH, na.rm=TRUE), 
            sd=sd(Soil_pH, na.rm =TRUE)), add=TRUE)

### quantile plot for Soil_Moisture
with(fertilizer_data_New, qqPlot(Soil_pH, dist="norm", id=list(method="y", n=2, 
                    labels=rownames(fertilizer_data_New)), xlab="Soil_pH Category", 
                        ylab="Soil_pH", main="Soil_pH Distributionn"))

#### Anderson Darling test for Rainfall
ad.test(Rainfall)

#### Lillifor test for Rainfall
lillie.test(Rainfall)
#### Shapiro-wiki for Rainfall
shapiro.test(Rainfall)
### summary statistics
summary(Rainfall)
### Graphicl Analysis for Rainfall
hist(Rainfall, main="Rainfall Distribution",
     xlab="Rainfall Category",
     ylab="Rainfall",
     prob=TRUE)
curve(dnorm(x, mean=mean(Rainfall, na.rm=TRUE), 
            sd=sd(Rainfall, na.rm =TRUE)), add=TRUE)

### quantile plot for Rainfall
with(fertilizer_data_New, qqPlot(Rainfall, dist="norm", id=list(method="y", n=2, 
                      labels=rownames(fertilizer_data_New)), xlab="Rainfall Category", 
                    ylab="Rainfall", main="Rainfall Distributionn"))

#### Anderson Darling test for Humidity
ad.test(Humidity)

#### Lillifor test for Humidity
lillie.test(Humidity)
#### Shapiro-wiki for Humidity
shapiro.test(Humidity)
### summary statistics of Humidity
summary(Humidity)
### Graphicl Analysis for Humidity
hist(Humidity, main="Humdity Distribution",
     xlab="Humidity Category",
     ylab="Humidity",
     prob=TRUE)
curve(dnorm(x, mean=mean(Humidity, na.rm=TRUE), 
            sd=sd(Humidity, na.rm =TRUE)), add=TRUE)

### quantile plot for Humidity
with(fertilizer_data_New, qqPlot(Humidity, dist="norm", id=list(method="y", n=2, 
              labels=rownames(fertilizer_data_New)), xlab="Humidity Category", 
                                 ylab="Humidity", main="Humidity Distributionn"))

#### Anderson Darling test for Temperature
ad.test(Temperature)

#### Lillifor test for Temperature
lillie.test(Temperature)
#### Shapiro-wiki for Soil Temperature
shapiro.test(Temperature)
### summary statistics of Temperature
summary(Temperature)
### Graphicl Analysis for Temperature
hist(Temperature, main="Temperature Distribution",
     xlab="Temperature Category",
     ylab="Temperature",
     prob=TRUE)
curve(dnorm(x, mean=mean(Temperature, na.rm=TRUE), 
            sd=sd(Temperature, na.rm =TRUE)), add=TRUE)

### quantile plot for Temerature
with(fertilizer_data_New, qqPlot(Temperature, dist="norm", id=list(method="y", n=2, 
                  labels=rownames(fertilizer_data_New)), xlab="Temperature Category", 
                  ylab="Temperature", main="Temperature Distribution"))


### correlational analysis
### Spearman method  for Crop_Yield and Crop_Type###
cor.test(Crop_Yield,Crop_Type, method="spearman",alternative="two.sided")

### graphical analysis between Crop_Yield and Crop_Type
scatterplot(Crop_Yield~Crop_Type, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
            xlab="Crop_Type", ylab="Crop_Yield", main="Crop_Yield vs Crop_Type", 
            data=fertilizer_data_New)
### Spearman method  for Crop_Yield and Fertilizer_Amount###
cor.test(Crop_Yield,Fertilizer_Amount, method="spearman",alternative="two.sided")
### graphical analysis between Crop_Yield and Fertilizer_Amount
scatterplot(Crop_Yield~Fertilizer_Amount, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Fertilize_Amount", ylab="Crop_Yield", 
            main="Crop_Yield vs Fertilize_Amount", data=fertilizer_data_New)
### Spearman method  for Crop_Yield and Potassium_Level###
cor.test(Crop_Yield,Potassium_Level, method="spearman",alternative="two.sided")
###graphical analysis  between Crop_Yield and Potassium_Level
scatterplot(Crop_Yield~Potassium_Level, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Potassium_Level", ylab="Crop_Yield", 
            main="Crop_Yield vs Potassium_Level", data=fertilizer_data_New)

### Spearman method for Crop_Yield and Phosphorus_Level
cor.test(Crop_Yield,Phosphorus_Level, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Phosphorus_Level
scatterplot(Crop_Yield~Phosphorus_Level, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Phosphorus_Level", ylab="Crop_Yield", 
            main="Crop_Yield vs Phosphorus_Level", data=fertilizer_data_New)

### Spearman method for Crop_Yield and Nitrogen_Level
cor.test(Crop_Yield,Nitrogen_Level, method="spearman", alternative = "two.sided")

### graphical analysis  between Crop_Yield and Nitrogen_Level
scatterplot(Crop_Yield~Nitrogen_Level, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Nitrogen_Level", ylab="Crop_Yield", 
            main="Crop_Yield vs Nitrogen_Level", data=fertilizer_data_New)

### Spearman method for Crop_Yield and Soil_Moisture
cor.test(Crop_Yield,Soil_Moisture, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Nitrogen_Level
scatterplot(Crop_Yield~Soil_Moisture, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Soil_Moisture", ylab="Crop_Yield", 
            main="Crop_Yield vs Soil_Moisture", data=fertilizer_data_New)


### Spearman method for Crop_Yield and Soil_pH
cor.test(Crop_Yield,Soil_pH, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Soil_pH
scatterplot(Crop_Yield~Soil_pH, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Soil_pH", ylab="Crop_Yield", 
            main="Crop_Yield vs Soil_pH", data=fertilizer_data_New)


### Spearman method for Crop_Yield and Rainfall
cor.test(Crop_Yield,Rainfall, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Rainfall
scatterplot(Crop_Yield~Rainfall, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Rainfall", ylab="Crop_Yield", 
            main="Crop_Yield vs Rainfall", data=fertilizer_data_New)


### Spearman method for Crop_Yield and Humidity
cor.test(Crop_Yield,Humidity, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Rainfall
scatterplot(Crop_Yield~Humidity, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Rainfall", ylab="Crop_Yield", 
            main="Crop_Yield vs Humidity", data=fertilizer_data_New)

### Spearman method for Crop_Yield and Humidity
cor.test(Crop_Yield,Temperature, method="spearman", alternative = "two.sided")
### graphical analysis  between Crop_Yield and Rainfall
scatterplot(Crop_Yield~Temperature, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Temperature", ylab="Crop_Yield", 
            main="Crop_Yield vs Temperature", data=fertilizer_data_New)

###  Simple Regression Analysis
# dependent variable : Crop_Yield
# independent variable : Fertilizer_Amount
cropyield_fertilizeramount_model <- lm(Crop_Yield~Fertilizer_Amount, model = TRUE)
cropyield_fertilizeramount_model 


### summary  statisitcs of cropyield_fertilizeramount_model
summary(cropyield_fertilizeramount_model)

# dependent variable : Crop_Yield
# independent variable : Potassium_Level
cropyield_potassium_model <- lm(Crop_Yield~Potassium_Level, model=TRUE)
cropyield_potassium_model

### summary statistics of cropyield_potassium_level
summary(cropyield_potassium_model)

# dependent variable : Crop_Yield
# independent variable : Phosphorus_Level
cropyield_phosphorus_model <- lm(Crop_Yield~Phosphorus_Level, model=TRUE)
cropyield_phosphorus_model

### summary statistics of cropyield_phosphorus_level
summary(cropyield_phosphorus_model)

# dependent variable : Crop_Yield
# independent variable : Nitrogen_Level
cropyield_nitrogen_model <- lm(Crop_Yield~Nitrogen_Level, model=TRUE)
cropyield_nitrogen_model

### summary statistics of cropyield_nitrogen_level
summary(cropyield_nitrogen_model)

# dependent variable : Crop_Yield
# independent variable : Soil_Moisture
cropyield_soil_moisture_model <- lm(Crop_Yield~Soil_Moisture, model=TRUE)
cropyield_soil_moisture_model

### summary statistics of cropyield_sil_moisture_level
summary(cropyield_soil_moisture_model)


# dependent variable : Crop_Yield
# independent variable : Soil_pH
cropyield_soil_pH_model <- lm(Crop_Yield~Soil_pH, model=TRUE)
cropyield_soil_pH_model

### summary statistics of cropyield_soil_pH_level
summary(cropyield_soil_pH_model)

summary(cropyield_temperature_model)

### Multi-Linear Regression analysis
cropyield_multi_linear_model <- lm(Crop_Yield~Crop_Type+Fertilizer_Amount+
                                     Potassium_Level+Phosphorus_Level+
                                     Nitrogen_Level+Soil_Moisture+
                                     Soil_pH+Rainfall+
                                     Humidity+Temperature, model=TRUE)
cropyield_multi_linear_model

### summary statistics of cropyield_multi_lineaar_model
summary(cropyield_multi_linear_model)

### graphical analysis for multilinear model
library('Rcmdr')
scatterplotMatrix(~Crop_Type+Crop_Yield+Fertilizer_Amount+Humidity+Nitrogen_Level+Phosphorus_Level+Potassium_Level+Rainfall+Soil_Moisture+Soil_pH+Temperature,
                  regLine=TRUE, smooth=FALSE, diagonal=list(method="density"), 
                  data=fertilizer_data_New)


#cor.test(Crop_Type,Temperature, method="spearman", sided="two.alternative");
#cor.test(Phosphorus_Level, Potassium_Level, method="spearman", sided="two.sided")
