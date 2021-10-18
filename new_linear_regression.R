#Regression Problem 

setwd("E:\\Acedimics\\MS\\Sems_1\\Bussiness_Analytics\\Term_paper")

df = read.csv('qsar_fish_toxicity.csv') # read the excel file

nrow(df)
ncol(df)

names(df)

library(dplyr) # install the package of dplyr

#structure of data
str(df)

#renaming few column names
names(df)[7]<- "quantitative_response"
names(df)[2]<- "SM1_Dzz"

#finding null values in data
sapply(df, function(x) sum(is.na(x)))

#Scatter plots to show relation between independent and dependent variable
plot(df$CIC0,df$quantitative_response)

plot(df$SM1_Dzz,df$quantitative_response)

plot(df$GATS1i,df$quantitative_response)

plot(df$NdsCH,df$quantitative_response)

plot(df$NdssC,df$quantitative_response)

plot(df$MLOGP,df$quantitative_response)

library(ggplot2)

#boxplot of each variable

boxplot_1 <- ggplot(df, aes(x=quantitative_response)) + 
  geom_boxplot(fill = "#0c4c8a")
boxplot_1

boxplot_2 <- ggplot(df, aes(x=CIC0)) + 
  geom_boxplot(fill = "#0c4c8a")
boxplot_2

boxplot_3 <- ggplot(df, aes(x=SM1_Dzz)) + 
  geom_boxplot(fill = "#0c4c8a")
boxplot_3

boxplot_4 <- ggplot(df, aes(x=GATS1i)) + 
  geom_boxplot(fill = "#0c4c8a")
boxplot_4

boxplot_5 <- ggplot(df, aes(x=MLOGP)) + 
  geom_boxplot(fill = "#0c4c8a")
boxplot_5

#-------------Multiple Regression--------------

#dividing data into train and test set 
smp_size <- floor(0.8 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ <- sample(seq_len(nrow(df)), size = smp_size)

training_data <- df[train_, ]
testing_data <- df[-train_, ]

relation <- lm( quantitative_response ~ training_data.,
               data=training_data)

summary(relation)

predictions = predict.lm(relation, testing_data)

RMS = mean(predictions-testing_data$quantitative_response)^2

print('Residual Error (MSE): ')
RMS

plot(relation)

plot(testing_data$quantitative_response,predictions)


#K-Fold

library(caret)

model_cv = train(quantitative_response ~ ., df,method = "lm",
                 trControl = trainControl(
                 method = "repeatedcv", 
                 number = 30,
                 repeats = 3,
                 verboseIter = TRUE)
                 )

summary(model_cv)

#Assign the next value nearer to the mean in place of the outlier value.
#DEaling with outliers

#1.quantitative_response
high <- mean(df$quantitative_response) + sd(df$quantitative_response) * 3
low <- mean(df$quantitative_response) - sd(df$quantitative_response) * 3

df$Outlier <- (df$quantitative_response < low | df$quantitative_response > high)

df$quantitative_response[df$Outlier]

med_<-median(df$quantitative_response[!df$Outlier])
med_
median(df$quantitative_response)

df$quantitative_response[df$outlier] <- med_

#2.

high <- mean(df$CIC0) + sd(df$CIC0) * 3
low <- mean(df$CIC0) - sd(df$CIC0) * 3

df$Outlier <- (df$CIC0 < low | df$CIC0 > high)

df$CIC0[df$Outlier]

med_=median(df$CIC0[!df$Outlier])

df$CIC0[df$outlier] <- med_

#3.

high <- mean(df$SM1_Dzz) + sd(df$SM1_Dzz) * 4
low <- mean(df$SM1_Dzz) - sd(df$SM1_Dzz) * 4

df$Outlier <- (df$SM1_Dzz < low | df$GATS1i > high)

df$SM1_Dzz[df$Outlier]

med_=median(df$SM1_Dzz[!df$Outlier])

df$SM1_Dzz[df$outlier] <- med_

#4.

high <- mean(df$GATS1i) + sd(df$GATS1i) * 3
low <- mean(df$GATS1i) - sd(df$GATS1i) * 3

df$Outlier <- (df$GATS1i < low | df$GATS1i > high)

df$GATS1i[df$Outlier]

med_ = median(df$GATS1i[!df$Outlier])

df$GATS1i[df$outlier] <- med_


#5.

high <- mean(df$MLOGP) + sd(df$MLOGP) * 3
low <- mean(df$MLOGP) - sd(df$MLOGP) * 3

df$Outlier <- (df$MLOGP < low | df$MLOGP > high)

df$MLOGP[df$Outlier]

med_=median(df$MLOGP[!df$Outlier])

df$MLOGP[df$outlier] <- med_


df = df[-c(8)]
names(df)


#model building again after handling outliers in data 

model_cv = train(quantitative_response ~ ., df,method = "lm",
                 trControl = trainControl(
                   method = "repeatedcv", 
                   number = 30,
                   repeats = 3,
                   verboseIter = TRUE)
)

summary(model_cv)

relation <- lm( quantitative_response ~ .,
                data=training_data)

summary(relation)


