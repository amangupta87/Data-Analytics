---
title: "Premium Economy vs Economy Ticket Pricing by Airlines"
output: html_document
author: "Aman Gupta"
date : February 16, 2018
---

This is a mini project based on the study of Premium Economy Vs Economy Ticket Pricing by Airlines. The main research question that we are concerned with is “What factors explain the difference in price between an economy ticket and a premium-economy airline ticket?” Some other questions to be answered are: How Premium and Economy class ticket prices vary in domestic and International Flights respectively? Does the Relative Price increase or decrease with percentage of premium and economy seats? What are the other contributing factors?

### Read the data
```{r}
airlines <- read.csv(paste("SixAirlinesDataV2.csv",sep=""))
View(airlines)
```

### Attach the dataframe
```{r}
attach(airlines)
```

### Summarize the data
```{r}
summary(airlines)
library(psych)
describe(airlines)
```

```{r}
#MEAN
apply(airlines[,6:18], FUN=mean, MARGIN=2)
```

```{r}
#STANDARD DEVIATION
apply(airlines[,6:18], FUN=sd, MARGIN=2)
```
The average price of an economy seat is USD 1327, while the average price of a premium-economy seat is USD 1845, i.e premium-economy seats are pricier by almost 49%.


### Data Types
```{r}
str(airlines)
```

# Visualization
### Visualization using Histograms
```{r}
par(mfrow=c(3,3))
plot(airlines$Airline, xlab="Airline")
hist(airlines$FlightDuration , xlab="Flight duration")
hist(airlines$SeatsEconomy, xlab="Number of Economy Seats")
hist(airlines$SeatsPremium, xlab="Number of Premium Seats")
hist(airlines$WidthEconomy, xlab="Width of Economy Seats")
hist(airlines$WidthPremium, xlab="Width of Premium Economy Seats")
hist(airlines$PriceEconomy, xlab="Price of Economy Seats")
hist(airlines$PricePremium, xlab="Price of Premium Economy Seats")
hist(airlines$SeatsTotal, xlab = "Number of total seats")
par(mfrow=c(1,1))
```

The difference in price can be explained by the enhanced features in premium-economy seats, i.e greater legroom (pitch) and wider seats. On average, premium-economy seats have 6.6 inches more legroom than economy seats, and are wider by around 1.6 inches. There are some other factor affecting the price of the airline ticket, like flight duration, whether the flight is international or domestic or factors like the type of airline it is(Boeing or Airbus).

### Visualization using Boxplots
```{r}
boxplot(FlightDuration~Aircraft,data=airlines,xlab="Aircraft type", ylab="Flight duration",col = c("red","yellow"))
```

```{r}
boxplot(FlightDuration~Airline,data=airlines,xlab="Airline", ylab="Flight duration",col = c("peachpuff","gray","turquoise","green","blue","orangered"))
```

```{r}
par(mfrow=c(1,3))
hist(airlines$FlightDuration,xlab="Flight Duration(hrs)",ylab="Frequency",main="flight duration histogram", col=c("red","blue","green","yellow"), breaks=15, xlim=c(0,16), ylim=c(0,60))
boxplot(airlines$FlightDuration,main="Flight Duration Boxplot", xlab="Flight Duration(hrs)",ylab="Frequency")
barplot(airlines$FlightDuration,main = "Flight Duration Barplot", ylim=c(0,16), xlab="Flight Duration(hrs)",ylab="Frequency")
```

```{r}
par(mfrow=c(1,3))
hist(airlines$SeatsEconomy,xlab="Number of Economy Seats ",ylab="Frequency",main="Seats Economy", col=c("blue","green"), xlim=c(0,450),ylim=c(0,195))
boxplot(airlines$SeatsEconomy,main="Number of Economy Seats Boxplot", xlab="Number of Economy Seats ",ylab="Frequency")
barplot(airlines$SeatsEconomy,main = "Number of Economy Seats Barplot", ylim=c(0,400), xlab="Number of Economy Seats ",ylab="Frequency")
```

```{r}
par(mfrow=c(1,3))
hist(airlines$SeatsPremium,xlab="Number of Premium Economy Seats ",ylab="Frequency",main="Seats PremiumEconomy", col=c("pink","magenta"),xlim=c(0,70),ylim=c(0,200))
boxplot(airlines$SeatsPremium,main="Number of PremiumEconomy Seats Boxplot")
barplot(airlines$SeatsPremium,main = "Number of PremiumEconomy Seats Barplot",ylim=c(0,70))
```

### Visualization using Pie-Charts
```{r}
par(mfrow=c(1,1))
pie(table(airlines$Airline),col=c("violet","blue","green","yellow","red","turquoise"),main="Airline split up")
```

```{r}
pie(table(airlines$Aircraft),col=c("red","yellow"),main="Aircraft manufacturer split up")
```
```{r}
pie(table(airlines$TravelMonth),main="Analysing peak months",col=c("linen","magenta","moccasin","pink"))
```

```{r}
# Creating a new column PriceDifference 
airlines$PriceDifference <- airlines$PricePremium - airlines$PriceEconomy
```

### Visualisation of PriceDifference 
```{r}
boxplot(airlines$PriceDifference~airlines$Airline, ylab="Price Difference", xlab="Airline", main="Boxplot of Price Difference vs. Airline", col=c("red","orangered","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$Aircraft, ylab="Price Difference", xlab="Aircraft", main="Boxplot of Price Difference vs. Aircraft", col=c("red","orangered","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$TravelMonth, ylab="Price Difference", xlab="Travel Month", main="Boxplot of Price Difference vs. Month of Travel", col=c("red","orangered","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$IsInternational, ylab="Price Difference", main="Boxplot of Price Difference vs. Type of flight", col=c("orangered","red","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$SeatsTotal, ylab="Price Difference", xlab="Number of Total seats", main="Boxplot of Price Difference vs. Number of Total seats", col=c("red","orangered","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$PercentPremiumSeats, ylab="Price Difference", xlab="PercentPremiumSeats", main="Boxplot of Price Difference vs. Percentage of Premium seats", col=c("red","orangered","yellow2","green3","skyblue","blue2"))
```

```{r}
boxplot(airlines$PriceDifference~airlines$PitchDifference, ylab="Price Difference", xlab="Pitch Difference", main="Boxplot of Price Difference vs.Pitch Difference", col=c("yellow2","green3","skyblue","blue2"))

```

```{r}
boxplot(airlines$PriceDifference~airlines$WidthDifference, ylab="Price Difference", xlab="Width Difference", main="Boxplot of Price Difference vs.Width Difference", col=c("yellow2","green3","skyblue","blue2"))
```

```{r}
library(lattice)
par(mfrow=c(2,1))
histogram(~PriceEconomy | IsInternational,data=airlines, col="maroon",main="Price of economy class tickets in international and domestic flights")
```

```{r}
par(mfrow=c(2,1))
histogram(~PricePremium | IsInternational,data=airlines,col="yellow2", main="Price of premium economy class tickets in international and domestic flights")
```

```{r}
par(mfrow=c(1,2))
histogram(~PriceRelative | IsInternational, data=airlines,
          type="count",
          layout=c(2,1), 
          col=c("burlywood", "darkolivegreen"),main="Relative pricing in international and domestic flights")
```

```{r}
bwplot(airlines$PriceRelative~ airlines$IsInternational |airlines$Airline,ylab="PriceRelative",main="Relative pricing in international and domestic flights in each arilines")
```

```{r}
par(mfrow=c(2,1))
histogram(~PercentPremiumSeats | Airline + IsInternational, data=airlines,
        layout=c(6,2),
          col="orangered", main="Percent of premium seats in international and domestic flights in each airline")
```

```{r}
barchart(~PercentPremiumSeats | Airline + IsInternational,ylab="PercentPremiumSeats",data=airlines,col="maroon",
                 layout=c(6,2), main="Percent of premium seats in international and domestic flights in each airline")
```

# Scatterplots
```{r}
library(car)
library(ggplot2)
ggplot(airlines, aes(PricePremium, FlightDuration)) + geom_point(aes(color = Airline)) +  scale_x_continuous("price of Premium ticket") + scale_y_continuous("Flight duration")+ labs(title="Scatterplot of premium ticket prices")
```

```{r}
library(ggplot2)
ggplot(airlines, aes(PricePremium, FlightDuration)) + geom_point(aes(color = Airline)) +  scale_x_continuous("price of Premium ticket") + scale_y_continuous("Flight duration")+ labs(title="Scatterplot of premium ticket prices in different months") + facet_wrap( ~ TravelMonth)
```

```{r}
ggplot(airlines, aes(PriceEconomy, FlightDuration)) + geom_point(aes(color = Airline)) +  scale_x_continuous("Price of economy ticket") + scale_y_continuous("Flight duration") + labs(title="Scatterplot of economy ticket prices")
```

```{r}
ggplot(airlines, aes(PriceEconomy, FlightDuration)) + geom_point(aes(color = Airline)) +  scale_x_continuous("price of economy ticket") + scale_y_continuous("Flight duration") + labs(title="Scatterplot of ticket prices in different months") + facet_wrap( ~ TravelMonth)
```

```{r}
scatterplot(PriceRelative ~PitchDifference,     data=airlines,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of price relative vs pitch difference",
            xlab="pitch difference",
            ylab="price relative")
```


```{r}
scatterplot(PriceRelative ~ WidthDifference, data= airlines,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatter plot of price relative vs Width difference",
            xlab="Width difference",
            ylab="Price relative")
```

```{r}
scatterplot(PriceEconomy~SeatsEconomy, data= airlines,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of pricing of no. of seats with price of economy class",
            xlab = "Number of seats in economy class",
            ylab= "Price of economy class")
```

```{r}
scatterplot(PricePremium~SeatsPremium, data= airlines,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of pricing of no. of seats with price of premium economy class",
             xlab = "Number of seats in premium economy class",
            ylab= "Price of premium economy class")
```


# Scatterplot Matrix
```{r}
scatterplotMatrix( ~  WidthDifference + PitchDifference + PriceDifference , 
                   data = airlines , diagonal = "histogram")
```

```{r}
scatterplotMatrix( ~  SeatsTotal + PercentPremiumSeats + PriceDifference, data = airlines , diagonal = "histogram")
```

```{r}
scatterplotMatrix( ~  FlightDuration + Aircraft + PriceDifference, data = airlines , diagonal = "histogram")
```

```{r}
scatterplotMatrix( ~  FlightDuration+ Airline + PriceDifference, data = airlines , diagonal = "histogram")
```

```{r}
scatterplotMatrix( ~  TravelMonth + Airline + PriceDifference, data = airlines , diagonal = "histogram")
```

# Corrgram
```{r}
library(corrgram)
par(mfrow=c(1,1))
corrgram(airlines, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Airlines data")
```

```{r}
correlationmatrix <- cor(airlines[,6:19])
round(correlationmatrix,digits = 2)
```


# Variance- Covariance Matrix
```{r}
VarianceCovariancematrix <- var(airlines[,6:19])
round(VarianceCovariancematrix, 2)
```

### Hypothesis
H0 : There is no correlation between “Price Difference of Premium Economy and Economy airline seat tickets” and “The variables present in the data provided”. H1 : Alternate Hypothesis i.e. Yes, there is a correlation between the above mentioned variables.
```{r}
newairlines <- airlines
newairlines$Airline <- as.numeric(airlines$Airline)
newairlines$Aircraft <- as.numeric(airlines$Aircraft)
newairlines$IsInternational <- as.numeric(airlines$IsInternational)
newairlines$TravelMonth <- as.numeric(airlines$TravelMonth)
newairlines <- newairlines[order(newairlines$Airline),]
```

### T-test
```{r}
t.test(newairlines$PriceEconomy, newairlines$PricePremium)
```
There is a significant difference between pricing of economy class and premium economy class tickets.

### Pearson’s Correlation Test
```{r}
cor.test(newairlines$PriceDifference, newairlines$FlightDuration)
```

```{r}
cor.test(newairlines$PriceDifference, newairlines$PitchDifference)
```

```{r}
cor.test(newairlines$PriceDifference, newairlines$WidthDifference)
```
These three correlations tests suggest that the difference in pricing of the two class of tickets depends strongly on the flightduration and also on the pitch and width difference(p-value<0.05).


# Regression Analysis

### Consider the following Regression equation

$PriceDifference = \beta_0 + \beta_1 PitchDifference + \beta_2 WidthDifference + \beta_3 FlightDuration + \beta_4 IsInternational + \beta_5 PercentPremiumSeats +   \epsilon$

```{r}
fit=lm(PriceDifference ~ PitchDifference + WidthDifference + FlightDuration + IsInternational + PercentPremiumSeats, data = airlines)
summary(fit)
```

```{r}
# beta coefficients
fit$coefficients
```

```{r}
# confidence intervals
confint(fit)
```

### Visualizing the beta coefficients
```{r}
library(coefplot)
# 1. WidthDifference, FlightDuration and PercentPremiumSeats are statistically significant
coefplot(fit, predictors=c("WidthDifference", "FlightDuration", "PercentPremiumSeats"))

# 2. PitchDifference and IsInternational are NOT statistically significant.
# We infer this since its confidence interval includes zero within it.
coefplot(fit, predictors=c("PitchDifference","IsInternational"))

```


* Hence, the p-values and the coefficients suggest that the model is a good fit and the regression is good and we can reject the null hypothesis.
* Prices of premium economy seats is more with increasing width, pitch and flight duration.
* WidthDifference and FlightDuration are statistically significant for PriceDifference.
* PitchDifference and IsInternational are not statistically significant, since their Confidence Inteval includes 0.

