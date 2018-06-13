setwd("C:/Users/Christopher/Desktop/STA 138/Final Project")

# DAV:
#   Diagnosis of depression in any visit during one year of care.
#   0 =  Not diagnosed
#   1 = Diagnosed
# PCS:
#   Physical component of SF-36 measuring health status of the patient.
# MCS:
#   Mental component of SF-36 measuring health status of the patient 
# BECK:
#   The Beck depression score.
# PGEND:
#   Patient gender
#   0 = Female
#   1 = Male 
# AGE:
#   Patient's age in years.
# EDUCAT:
#   Number of years of formal schooling.

depression <- read.table("final.dat", header = TRUE)
attach(depression)

# There should be an inverse relationship between MCS and BECK
# Turns out it doesn't matter!

model <- glm(dav~., data = depression, family = binomial(link = logit))
summary(model)$coefficients
# Hm, Wald test says pcs and age may be equal to zero


nothing <- glm(dav~1, data = depression, family = binomial(link = logit))
full <- glm(dav~., data = depression, family = binomial(link = logit))

forwards <- step(nothing, scope = list(lower = formula(nothing), upper = formula(full)), direction = "forward", trace = FALSE)

backwards <- step(full, scope = list(lower = formula(nothing), upper = formula(full)), direction = "backward", trace = FALSE)

# Forwards and backwards stepwise lead to the same model, minimizing AIC
final.model <- forwards


# Odds ratios and CIs for odds ratios
est.coef <- coef(final.model)
est.coef.se <- summary(final.model)$coefficients[, "Std. Error"]

odds <- exp(est.coef[-which(names(est.coef) == "(Intercept)")])

odds.CI <- data.frame(lower = exp(est.coef[-which(names(est.coef) == "(Intercept)")]-1.96*est.coef.se[-which(names(est.coef) == "(Intercept)")]),
      upper = exp(est.coef[-which(names(est.coef) == "(Intercept)")]+1.96*est.coef.se[-which(names(est.coef) == "(Intercept)")]))

# Hosmer-Lemeshow test for goodness of fit
library(ResourceSelection)                                      
hoslem.test(dav, fitted(final.model))









# Write out the model, so logit(pi_i) = log(\frac{\pi_i}{1-\pi_i}) = \beta_0 + \beta_1x_i
# Test if beta_1 is significant, create CI
# Odds ratio exp(\beta_1) and interpretation, also CI and interpretation
# Goodness of Fit for Logistic Regression and Hosmir and Lemeshow Test
# Forego the GOF for LR because we could have n = 1 for all categories. Very unlikely
# that two people have exactly the same pcs, mcs, beck, age, gender, educat
# Fit is decent with p = 0.5556
# Measure of Association (maybe?) nah
# Residual diagnostics


# standardized Pearson residuals #
pear.stdresid=resid(final.model,type="pearson")/sqrt(1-lm.influence(final.model)$hat)                 

# Outliers
outlier.pear.stdresid <- pear.stdresid[which(abs(pear.stdresid) > 2)]
h <- lm.influence(model)$hat
outlier.hat <- h[which(abs(pear.stdresid) > 2)]
outliers.df <- data.frame(cbind(depression[which(abs(pear.stdresid) > 2), ],
                                outlier.pear.stdresid,
                                outlier.hat))
names(outliers.df)[8] = "Pearson stdresid"

# Influential observations
n <- nrow(depression)
p <- length(final.model$coefficients)
influential.bound <- 2*p/n

# None of the outliers are influential observations
outlier.hat > influential.bound




