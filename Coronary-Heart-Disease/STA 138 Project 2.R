# https://www.umass.edu/statdata/statdata/stat-logistic.html

setwd("C:/Users/Christopher/Desktop/STA 138/Project 2")
chdage <- read.table("C:/Users/Christopher/Desktop/STA 138/Project 2/chdage.dat", header=TRUE, quote="\"")
# Remove ID column
chdage$ID <- NULL
attach(chdage)

# Logistic regression model
model <- glm(CHD~AGE, data = chdage, family = binomial(link = logit))
summary(model)

# Write out the model, so logit(pi_i) = log(\frac{\pi_i}{1-\pi_i}) = \beta_0 + \beta_1x_i
# Test if beta_1 is significant, create CI
# Odds ratio exp(\beta_1) and interpretation, also CI and interpretation
# Max slope of the probability
# Goodness of Fit for Logistic Regression and Hosmir and Lemeshow Test
# Measure of Association (maybe?)
# Residual diagnostics

b0=coef(model)[1]       # extract the estimate of intercept #
b1=coef(model)[2]       # extract the estimate of slope #

# define the estimated probability function #
estprob=function(x){    
  z=exp(b0+b1*x)/(1+exp(b0+b1*x))
  return(z)
}

chdage.table <- table(chdage)
# calculate the sample proportion at observed Age #
prop_raw=chdage.table[, 2]/(chdage.table[, 2]+chdage.table[, 1])    

# plot of observed (black), fitted (solid line)
curve(estprob,from=20,to=69,xlim=c(18,71),ylim=c(0,1),ylab="Probability of Coronary Heart Disease",xlab="Age")       
points(unique(chdage$AGE), prop_raw, pch=19)
legend('topleft', legend = c("Fitted", "Observed"), lty = c(1, NA), pch = c(NA, 19))

# Odds ratio
odds <- exp(b1)

# CI for odds ratio
odds.CI <- c(exp(0.11092-1.96*0.02406), exp(0.11092+1.96*0.02406))

# Maximum of slope and where it occurs
b1/4
-b0/b1


# diagnostics of model #
# Likelihood ratio goodness of fit
model$deviance # is distributed chi-square with df = 1
1 - pchisq(model$deviance, df = 1) # is the p-value of the test, so we reject the null model with fewer parameters in favor of our current model

# Hosmir and Lemeshow Test for goodness of fit
library(ResourceSelection)                                      
hoslem.test(CHD, fitted(model))

# standardized Pearson residuals #
pear.stdresid=resid(model,type="pearson")/sqrt(1-lm.influence(model)$hat)                 

# plot of standardized Pearson residuals against Age #
plot(chdage$AGE,pear.stdresid,pch=19,xlab="Age",ylab="standardized Pearson residuals")  
abline(h=0)                                                                               

# h_i values
h <- lm.influence(model)$hat
h[which(abs(pear.stdresid) > 2)]


# pointwise 95% confidence band of probability of remission #
pred_cb=predict(model,data.frame(AGE=seq(20,69)),type="response",se.fit=T)

# plot of pointwise 95% confidence band (dashed line) #
curve(estprob,from=20,to=69,ylab="Probability of Coronary Heart Disease",xlab="Age",ylim=c(-0.05,1)) 
lines(seq(20,69),pred_cb$fit-1.96*pred_cb$se.fit,lty=2)
lines(seq(20,69),pred_cb$fit+1.96*pred_cb$se.fit,lty=2)
legend('topleft', legend = c("Fitted", "95% Confidence band"), lty = c(1, 2))



# diagnostics of model #
pear.stdresid=resid(model,type="pearson")/sqrt(1-lm.influence(model)$hat)                         # standardized Pearson residuals #

plot(chdage$AGE,pear.stdresid,pch=19,xlab="LI",ylab="standardized Pearson residuals")  
abline(h=0)                                                                                       # plot of standardized Pearson residuals against LI #

plot(model$fitted,pear.stdresid,pch=19,xlab="fitted prob",ylab="standardized Pearson residuals")  
abline(h=0)                                                                                       # plot of standardized Pearson residuals against fitted probability #