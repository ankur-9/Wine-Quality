library(ggplot2)
library(dplyr)
library(Simpsons)
library(caret)


wine <- read.csv("winequality-red.csv",sep = ";")
wine_backup <- wine
str(wine)
wine$quality <- factor(wine$quality,ordered = TRUE)
wine$rating <- ifelse(wine$quality < 5,"poor",ifelse(wine$quality > 6,"good","average"))
wine$rating <- factor(wine$rating,levels = c("poor","average","good"),ordered = TRUE)

summary(wine)
#number of average rating wine are way more than others.

# Univariate Analysis
#Distribution plots for each variable

# Quality and rating
ggplot(wine,aes(x = quality))+geom_bar()
ggplot(wine,aes(x = rating))+geom_bar()
# It seems that most wines in the dataset are of average quality.

# Fixed Acidity
ggplot(wine,aes(x = fixed.acidity))+geom_histogram()
ggplot(wine,aes(x = 1, y = fixed.acidity))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.5)
# 1. Distribution of fixed acidity is rightly or positively skewed
# 2. Median is around 8 with high concentration of wines with fixed acidity 
# but due to some outliers mean has been dragged to 8.32

# Volatile Acidity
ggplot(wine,aes(x = volatile.acidity))+geom_histogram(binwidth = 0.05,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = volatile.acidity))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.5)
# 1. Distribution on volatile acidity is Bimodel at 0.4 and 0.6
# 2. Due to some outliers mean has been dragged above median

# Citric Acid
ggplot(wine,aes(x = citric.acid))+geom_histogram(binwidth = 0.08,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = citric.acid))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.5)
# 1. there are few outliers
# 2. from the histogram it seems that there is no/incomplete data for higher values.
# 3. Distribution is almost rectangle.

# Residual Sugar
ggplot(wine,aes(x = residual.sugar))+geom_histogram(binwidth = 0.3,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = residual.sugar))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Distribution is positively skewed with peak at around 2-2.5
# IQR is very small ~0.7 and there are many outliers


#chlorides
ggplot(wine,aes(x = chlorides))+geom_histogram(binwidth = 0.03,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = chlorides))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Distribution is positively skewed with peak at around 0.07
# there are many outliers


# Free Sulphur Dioxide
ggplot(wine,aes(x = free.sulfur.dioxide))+geom_histogram(binwidth = 5,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = free.sulfur.dioxide))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Distribution is positively skewed with peak at around 7-10
# some outliers in higher range

# Total Sulphur Dioxide
ggplot(wine,aes(x = total.sulfur.dioxide))+geom_histogram(binwidth = 5,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = total.sulfur.dioxide))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# as seen from the summary, mean is above median that means distibution must be positively skewed
# some outliers in higher range

# Density
ggplot(wine,aes(x = density))+geom_histogram(binwidth = 0.001,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = density))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# as seen from the summary, mean~median that means distibution must be close to normal
# As expected distribution is Normal
# Outliers on both the sides

# pH
ggplot(wine,aes(x = pH))+geom_histogram(binwidth = 0.1,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = pH))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Normal distributed

# Sulphates
ggplot(wine,aes(x = sulphates))+geom_histogram(binwidth = 0.05,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = sulphates))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Distribution is positively skewed with peak at around 0.6
# some outliers in higher range

# Alcohol
ggplot(wine,aes(x = alcohol))+geom_histogram(binwidth = 0.1,color = "black",fill = "orange")
ggplot(wine,aes(x = 1, y = alcohol))+geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
# Distribution is positively skewed with peak at around 9.5
# very few outliers


# Bivariate Plots

cor_data <- wine%>%
  select(1:12)%>%
  mutate(quality = as.numeric(quality))%>%
  cor()%>%
  round(2)
# Volatile acidity has a positive correlation with pH. But how can that be possible! 
# We know that with the decrease in pH, acidity increases. 
# So is it possible that a Simpson's Paradox is at play here?

# Fixed Acidity vs Quality
ggplot(wine,aes(x = quality, y = fixed.acidity))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 5)
# Fixed Acidity has almost no effect on the Quality. 
# The mean and median values of fixed acidity remains almost unchanged with increase in quality.

# Volatile Acidity vs Quality
ggplot(wine,aes(x = quality, y = volatile.acidity))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# With the increase in volatile acid level, quality of wine degrades.

# Citric Acid vs Quality
ggplot(wine,aes(x = quality, y = citric.acid))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Positive correlation. Citric acid improves the quality of wines


# Residual Sugar vs Quality
ggplot(wine,aes(x = quality, y = residual.sugar))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Residual Sugar has almost no effect on the Quality.

# Chlorides vs Quality
ggplot(wine,aes(x = quality, y = chlorides))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Weakly correlated
# lower chloride increases the quality of wine

# Free Sulphur Dioxide vs Quality
ggplot(wine,aes(x = quality, y = free.sulfur.dioxide))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Too low Concn produces poor quality wine and too high concn produces average quality wine.

# Total Sulphur Dioxide vs Quality
ggplot(wine,aes(x = quality, y = total.sulfur.dioxide))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Similar effect as free sulphur dioxide.

# Density vs Quality
ggplot(wine,aes(x = quality, y = density))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
#Better wines seems to have lower densities.

# pH vs Quality
ggplot(wine,aes(x = quality, y = pH))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)

#..............................pH vs Acids................................................#
ggplot(wine,aes(x = fixed.acidity, y = pH))+
  geom_point()+geom_smooth(method = "lm")+scale_x_log10()
# pH decrease as fixed acid increases

ggplot(wine,aes(x = volatile.acidity, y = pH))+
  geom_point()+geom_smooth(method = "lm")+scale_x_log10()
# !!!! pH increase as volatile acid increase !!!!!!

ggplot(wine,aes(x = citric.acid, y = pH))+
  geom_point()+geom_smooth(method = "lm")+scale_x_log10()
# pH decrease as citric acid increases

# Recall that we saw for Volatile Acid, pH has a positive correlation. 
# But we know acidity has a negative correlation with pH. 
# So is it possible, that we are seeing a Simpson's Paradox at play here? Let's investigate.

simpsons <- Simpsons(volatile.acidity,pH,data = wine)

#........................................................................................#

# Sulphates vs Quality
ggplot(wine,aes(x = quality, y = sulphates))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Better wines have high concentration of sulphates

# Alcohol vs Quality
ggplot(wine,aes(x = quality, y = alcohol))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)+
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape = 1)
# Good wines have higher alcohol content

# From the correlation table we see that following variables have a higher correlation to wine quality
# 1. Volatile acidity
# 2. citric acid
# 3. sulphates
# 4. alcohol


# Split data into train and test
set.seed(123)
rows <- sample(nrow(wine))
df <- wine[rows,]
split <- round(nrow(df) * 0.7)
train <- df[1:split,]
test <- df[(split+1):nrow(df),]

#............................Random Forest....................................#

# Prediction by rating:
rf_model <- train(rating~volatile.acidity+citric.acid+sulphates+alcohol,
      data = train,
      method = "ranger",
      trControl = trainControl(method = "cv",number = 5))
plot(rf_model)
pred <- predict(rf_model,newdata = test[,-c(12,13)])
confusionMatrix(pred,test$rating)
# Accuracy 0.8792


# Prediction by Quality(0-10):
rf_model2 <- train(quality~volatile.acidity+citric.acid+sulphates+alcohol,
                  data = train,
                  method = "ranger",
                  trControl = trainControl(method = "cv",number = 5))
plot(rf_model2)
pred2 <- predict(rf_model2,newdata = test[,-c(12,13)])
confusionMatrix(pred2,test$quality)
# Accuracy 0.6583