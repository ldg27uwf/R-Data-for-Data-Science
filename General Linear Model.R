# using OpenIntro possum data set to demonstrate general linear model

library(readr)

install.packages("MASS")
library(MASS)

install.packages("car") # Cmake required for ubuntu
library(car)

install.packages("tidyverse")

possum <- read_csv("Downloads/possum.csv")
head(possum)

# scatterplot of total length to head length
plot(possum$total_l,possum$head_l)

cor(possum$total_l,possum$head_l)

m1 <- lm(head_l~total_l, data = possum)
summary(m1)
plot(m1, 1)

plot(possum$total_l,possum$head_l)
abline(m1)

# diagnostics

# 1. Check linearity between residuals and head length

plot(m1$residuals ~ possum$head_l)
abline(h = 0, lty = 3)

# 2. Check normailty of residuals

hist(m1$residuals) # residuals appear approx. normal

qqnorm(m1$residuals)
qqline(m1$residuals) # residuals appear approx. normal

# 3.  Constant variance

plot(m1,3)
ncvTest(m1) # null hypothesis is constant variance
# p-value >0.05 means fail to reject null, assumption is met

plot(m1,5) # not indicative of significant points of leverage


