setwd("C:/Users/Christopher/Desktop/STA 138/Project 1")

# https://archive.ics.uci.edu/ml/datasets/Teaching+Assistant+Evaluation
tae <- read.csv("C:/Users/Christopher/Desktop/STA 138/Project 1/tae.data", header=FALSE)

colnames(tae) <- c("English", "Instructor", "Course", "Type", "Size", "Attribute")

# We will only deal with English, Type, Size (categorized), and Attribute
tae <- tae[, -c(2, 3)]

# Small = 0-20, Medium = 21-40, Large = 41+
tae$Size <- cut(tae$Size, c(0, 20, 40, max(tae$Size)), labels = c("Small", "Medium", "Large"))

# English: 1 = native English speaker
# Type: 1 = Summer session
# Size: categorized into small, medium, large
# Attribute: 1 = Low, 2 = Medium, 3 = High

tae.data <- data.frame(expand.grid(English = factor(c("Yes", "No")), Type = factor(c("Summer", "Semester")), Size = factor(c("Small", "Medium", "Large")), Attribute = factor(c("Low", "Medium", "High"))), count = c(2, 0, 0, 12, 0, 0, 2, 24, 0, 0, 1, 8, 1, 3, 1, 14, 0, 2, 3, 16, 0, 0, 1, 9, 4, 9, 1, 9, 1, 0, 6, 13, 1, 0, 5, 3))

homogeneous.model <- glm(count~English+Type+Size+Attribute+English:Type+English:Size+English:Attribute+Type:Size+Type:Attribute+Size:Attribute+English:Type:Size+English:Type:Attribute+Type:Size:Attribute+English:Size:Attribute, family = poisson, data = tae.data)
# 1-pchisq(homogeneous.model$deviance, df = 4)
# [1] 0.7251979

# Looking at p-values, consider models with English:Size, English:Attribute

tae.model1 <- glm(count~English+Type+Size+Attribute+English:Size+English:Attribute, family = poisson, data = tae.data)
# 1-pchisq(tae.model1$deviance, df = 25)
# [1] 4.899922e-06

# Maybe remove English:Size?

tae.model2 <- glm(count~English+Type+Size+Attribute+English:Attribute, family = poisson, data = tae.data)
# 1-pchisq(tae.model2$deviance, df = 27)
# [1] 7.963628e-06
# This one has a bigger p-value than tae.model1
# May be the best model we can make

# Examining higher p-values alternaties:
# Size:Attribute, English:Size:Attribute

tae.model3 <- glm(count~English+Type+Size+Attribute+English:Size+English:Attribute+Size:Attribute+English:Size:Attribute, family = poisson, data = tae.data)
# 1-pchisq(tae.model3$deviance, df = 17)
# [1] 7.620366e-07
# Lower p-value! This is not good




# Odds ratio stuff
fit.table <- xtabs(~English+Attribute, data = tae)
chisq.test(fit.table)
# Pearson's Chi-squared test
# 
# data:  fit.table
# X-squared = 12.19, df = 2, p-value = 0.002255
# Since p-value is low, reject null hypothesis of independence

fit.table2 <- chisq.test(xtabs(~English+Size, data = tae))
# Pearson's Chi-squared test
# 
# data:  xtabs(~English + Size, data = tae)
# X-squared = 2.0098, df = 2, p-value = 0.3661
# English and Size are independent, hence why it didn't work well with our model



# Given English and Attribute, Type and Size are independent
tae.model4 <- glm(count~English+Type+Size+Attribute+English:Attribute+English:Type:Attribute+English:Size:Attribute, family = poisson, data = tae.data)
1-pchisq(tae.model4$deviance, df = 12)
# 0.000892321 HIGHER p-value than tae.model2
sum(abs(tae.model4$residuals) > 2)

# Given Type and Size, English and Attribute are independent
tae.model5 <- glm(count~English+Type+Size+Attribute+English:Type:Size+Type:Size:Attribute, family = poisson, data = tae.data)
sum(abs(tae.model5$residuals) > 2)
1-pchisq(tae.model5$deviance, df = 4)
# Worse model

# Also compare AIC and BIC of homogeneous and other models. You will find tae.model2 is the best
AIC(homogeneous.model, tae.model1, tae.model2, tae.model3, tae.model4, tae.model5)

BIC(homogeneous.model, tae.model1, tae.model2, tae.model3, tae.model4, tae.model5)








