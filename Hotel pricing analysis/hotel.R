---
title: "Hotel Room Pricing in Indian Cities"
output: html_document
author: "Aman Gupta"
date : February 23, 2018
---

### Read the data
```{r}
hotel <- read.csv(paste("Cities42.csv", sep=""))
View(hotel)
```

```{r}
dim(hotel)
```

### Attach the dataframe
```{r}
attach(hotel)
```

### Summarize the data
```{r}
summary(hotel)
library(psych)
describe(hotel)
```

### Data Types
```{r}
str(hotel)
```

### Variable Analysis

##### One way contingency tables
```{r}
mytable <- with(hotel,table(CityName))
mytable
```

```{r}
mytable1 <- with(hotel,table(FreeWifi))
mytable1
```

```{r}
mytable2 <- with(hotel,table(FreeBreakfast))
mytable2
```

```{r}
mytable3 <-with(hotel,table(HasSwimmingPool))
mytable3
```

```{r}
mytable4 <- with(hotel, table(StarRating))
mytable4
```

```{r}
mytable5 <- with(hotel, table(IsMetroCity))
mytable5
```

```{r}
mytable6 <- with(hotel, table(IsTouristDestination))
mytable6
```

##### Two way contingency tables
```{r}
mytable <- xtabs(~ StarRating + FreeBreakfast, data=hotel)
mytable
```

```{r}
mytable1<- xtabs(~ StarRating + FreeWifi, data=hotel)
mytable1
```

```{r}
mytable3 <- xtabs(~ IsMetroCity+IsTouristDestination, data=hotel)
mytable3
```


```{r}
library(gmodels)
CrossTable(hotel$FreeWifi,hotel$FreeBreakfast)
```

```{r}
CrossTable(hotel$FreeWifi,hotel$HasSwimmingPool)
```


# Visualization
```{r}
boxplot(hotel$StarRating,
        xlab="Star Rating of the hotel",
        main="Box plot of Star Rating of hotel",
        col = "yellow",
        horizontal = TRUE)
```


```{r}
boxplot(hotel$Airport,
        xlab="Distance between Hotel and closest major Airport(in km)",
        main="Box plot of Airport Distance of hotel",
        col = "yellow",
        horizontal = TRUE)
```


```{r}
boxplot(hotel$HotelCapacity,
        xlab="Hotel Capacity",
        main="Box plot of Hotel Capacity",
        col = "yellow",
        horizontal = TRUE)
```


```{r}
boxplot(hotel$StarRating ~ hotel$FreeBreakfast, horizontal=TRUE,
           ylab="breakfast avalability", xlab="Star ratings", las=1,
           main="Analysis of star rating and breakfast avalability",
           col=c("red","green")
           )
```


```{r}
boxplot(hotel$RoomRent ~ hotel$IsMetroCity, horizontal=TRUE,
           ylab="City(metro=1,other=0)", xlab="Room rent", las=1,
           main="Analysis of type of city and room rent of hotels",
           col=c("red","blue"),
           ylim=c(0,30000)
           )
```


```{r}
boxplot(hotel$RoomRent ~ hotel$IsNewYearEve, horizontal=TRUE,
        ylab="IsNewYearEve", xlab="Room rent", las=1,
        main="Analysis of room rent of hotels with New Year",
        ylim = c(0,30000),
        col = c("yellow","pink")
        )
```


```{r}
hist(StarRating , col = "green")
hist(Airport , col = "magenta" , xlab = "Distance between Hotel and closest major Airport in Km" , main = " Airport to Hotel")
hist(HotelCapacity , col = "blue")
```


```{r}
hist(hotel$RoomRent, 
      main="Analysis of room rents of hotels",
      xlab="Rents of room",ylab="Relative frequency",
      xlim = c(0,30000),breaks=30,
      col="green")
```


```{r}
hotel$FreeWifi=factor(hotel$FreeWifi, levels=c(0,1), labels=c("No","Yes"))
plot(hotel$FreeWifi,col="yellow",main="Has Wifi?")
```


```{r}
hotel$FreeBreakfast=factor(hotel$FreeBreakfast, levels=c(0,1), labels=c("No","Yes"))
plot(hotel$FreeBreakfast,col="blue",main="Has Free Breakfast?")
```


```{r}
hotel$HasSwimmingPool=factor(hotel$HasSwimmingPool, levels=c(0,1), labels=c("No","Yes"))
plot(hotel$HasSwimmingPool,col="green",main="Swimming pool?")
```



# Scatterplots
```{r}
library(car) 
scatterplot(RoomRent~StarRating,     data=hotel,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of Star Rating vs Room rent",
            ylab="Room Rent",
            xlab="Star Rating")
```


```{r}
scatterplot(x = hotel$Population, y = hotel$CityRank,
            spread=FALSE, smoother.args=list(lty=2),
            main="Population Vs City Rank " , 
            xlab="Population", ylab="City rank")
```


```{r}
scatterplot(HotelCapacity~StarRating,     data=hotel,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of Star Rating vs Hotel Capacity",
            ylab="Room Rent",
            xlab="Hotel Capacity")
```


```{r}
scatterplot(Airport~StarRating,data=hotel,
            spread=FALSE, smoother.args=list(lty=2),
            main="Star Rating vs Distance from Airport",
            ylab="Airport Distance",
            xlab="Star Rating")
```


```{r}
plot(y=hotel$RoomRent, x=hotel$Airport,
     col="green",
     ylim=c(0, 100000), xlim=c(0, 150), 
     main="Relationship Btw Room Rent and Airport Distance",
     ylab="Hotel Rent", xlab="Airport Distance")
```


```{r}
plot(y=hotel$RoomRent, x=hotel$StarRating,
     col="blue",
     ylim=c(0, 200000), xlim=c(0, 5), 
     main="Relationship Btw Room Rent and Star Rating of Hotel",
     ylab="Hotel Rent", xlab="Star Rating")
```


# Scatterplot Matrix

### Effect of external factors on room rent
```{r}
scatterplotMatrix(formula = ~ RoomRent + StarRating +Airport+ IsWeekend + IsNewYearEve ,
                  data = hotel , 
                  main="Scatter plot matrix of external factors")
```

### Effect of internal factors on room rent
```{r}
scatterplotMatrix(formula = ~ RoomRent + FreeWifi + FreeBreakfast + HotelCapacity + HasSwimmingPool, 
                  data = hotel ,
                  main="Scatter plot matrix of internal factors")
```



# Corrgram
```{r}
library(corrgram)
corrgram(hotel, order=TRUE, lower.panel=panel.shade,
upper.panel=panel.pie, text.panel=panel.txt,
main="Hotel Pricing Corrgram")
```


# Variance- Covariance Matrix
```{r}
VarianceCovariancematrix <- var(hotel[,c(4:7,10:12,16:19)])
round(VarianceCovariancematrix, 2)
```


# Correlation Matrix

```{r} 
x<- hotel[,c("Population", "RoomRent","StarRating","Airport","HotelCapacity")]
y<- hotel[,c("Population", "RoomRent","StarRating","Airport","HotelCapacity")]
correlationmatrix <- cor(x,y,method="pearson")
round(correlationmatrix,digits = 2) 
```

```{r}
#Correlation matrix of Room Rent with IsWeekend and IsnewyearEve
round(cor(hotel$RoomRent, hotel[,c("IsWeekend","IsNewYearEve")]),4)
```

```{r}
#correlation Matrix of RoomRent with CityRank, IsMetroCity, IsTouristDestination
round(cor(hotel$RoomRent, hotel[,c("CityRank","IsMetroCity","IsTouristDestination")]),4)
```


# Pearson's correlation Test
```{r}
cor.test(hotel$RoomRent , hotel$StarRating)
```

```{r}
cor.test(hotel$RoomRent , hotel$Airport)
```

```{r}
cor.test(hotel$RoomRent , hotel$IsTouristDestination)
```

```{r}
cor.test(hotel$RoomRent , hotel$HotelCapacity)
```


# T-tests

### RoomRent and IsMetroCity

H0: There is no significant difference between the Room Rent of Hotels in non-metro cities and metro cities.
H1: Hotels in non-metro cities are more expensive than that in metro cities.
```{r}
t.test(RoomRent~IsMetroCity,data=hotel)
```
Since p-value<0.05, we reject the null hypothesis,hence,the Room Rents of Hotels in non-metro cities is more than that of metro cities.

### RoomRent and IsTouristDestination

H0: There is no significant difference between the Room Rent of Hotels in Tourist destinations and non tourist destinations.
H1: The Room Rents of Hotels in Tourist destinations are greater than that in non tourist destinations
```{r}
t.test(RoomRent~IsTouristDestination,data=hotel)
```
Since p-value<0.05, we reject the null hypothesis, hence, the Room Rents of Hotels in Tourist destinations are greater than that in non tourist destinations.

### RoomRent and IsNewYearEve

H0: There is no significant difference between the Room Rent of Hotels on normal Eve and New Year’s Eve.
H1: The Room Rents of Hotels on normal Eve are cheaper than that on New Year’s Eve
```{r}
t.test(RoomRent~IsNewYearEve,data=hotel)
```
Since p-value<0.05, we reject the null hypothesis, hence, the Room Rents of Hotels on normal Eve are cheaper than that on New Year’s Eve.

### RoomRent and FreeBreakfast

H0: There is no significant difference between the Room Rent of Hotels with free breakfast and hotels without free breakfast.
H1: There is a significant difference between the Room Rent of Hotels with free breakfast and hotels without free breakfast
```{r}
t.test(RoomRent~FreeBreakfast,data=hotel)
```
Since p-value>0.05, we accept H0, hence, there is no significant difference between the Room Rent of Hotels with free breakfast and hotels without free breakfast.

### RoomRent and FreeWifi

H0:-There is no significant difference between the Room Rent of Hotels providing free wifi and those which do not
H1:-There is a significant difference between the Room Rent of Hotels providing free wifi and those which do not.
```{r}
t.test(RoomRent~FreeWifi,data=hotel)
```
Since p-vale>0.05, we accept H0, hence there is no significant difference between the Room Rent of Hotels providing free wifi and those which do not.


# Regression Analysis

### Consider the following Regression Model

$RoomRent = \beta_0 + \beta_1 StarRating + \beta_2 Airport + \beta_3 FreeWifi + \beta_4 FreeBreakfast + \beta_5 HotelCapacity + \beta_6 HasSwimmingPool + \beta_7 IsMetroCity + \beta_8 IsNewYearEve + \beta_9 IsTouristDestination + \epsilon$

```{r}
fit <- lm(RoomRent ~ StarRating + Airport + FreeWifi + FreeBreakfast + HotelCapacity + HasSwimmingPool + IsMetroCity + IsNewYearEve + IsTouristDestination, data = hotel)
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
coefplot(fit, predictors=c("StarRating", "Airport", "FreeWifi", "FreeBreakfast", "HotelCapacity", "HasSwimmingPool", "IsMetroCity", "IsNewYearEve", "IsTouristDestination"))
```

---------------------------------------------------------------------------------------------

