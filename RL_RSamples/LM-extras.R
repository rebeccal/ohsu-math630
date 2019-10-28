
library(tidyverse)
library(MASS) #ginv() matrix inversion
library(broom)
library(moderndive)


#Explore correlations
GGally::ggpairs(swiss)

swiss %>%
  cor()  %>% 
  round(3)
  

swiss %>%
  cor(.,use = "pairwise.complete.obs")

swiss %>%
  corrr::correlate()


# Look at it
ggplot(swiss,aes(x=Examination, y=Agriculture)) +
  geom_point()

#Again, with more info
ggplot(swiss,aes(x=Examination, y=Agriculture)) +
  geom_smooth(method="lm", se=FALSE) +
  geom_point() +
  geom_text(aes(label=rownames(swiss)))


#Model it
lm_swiss <-lm(Agriculture ~ Examination, swiss)

# get the dets
tidy(lm_swiss) # Model spec and coefficient statistics
glance(lm_swiss) # Model level statistics, FYI: sigma=residual standard error
augment(lm_swiss) # Fitted and diagnostic values
summary(lm_swiss) # most of the above, not tidy


# Residual standard error and standard error of the coefficients
design_matrix <- model.matrix(lm_swiss) # Essential the X matrix - here it's 1 for the intercept and the value of the explanatory variable

#Residual Standard Error (Almost the Standard Deviation, but not quite - different denominator)
MSE <- sum((swiss$Agriculture - fitted(lm_swiss))^2) / (nrow(design_matrix) - ncol(design_matrix))
RSE <- sqrt(MSE) # matches sigma and RSE above
sd(augment(lm_swiss)$.resid) # Doh! Standard error <> standard deviation!
#Another way - a little different
k   <- length(lm_swiss$coefficients) # Number of coefficients
n   <- length(lm_swiss$residuals) # Number of rows
SSE <- sum(lm_swiss$residuals**2)
RSE <- sqrt(SSE/(n-k)) # Residual Standard Error!! It's about sampling distributions and degrees of freedom.

#SE of the coefficients and the coefficients (betas)
sqrt(diag(solve(crossprod(design_matrix))) * RSE) # SE of the coefficients
ols_estimator <- solve(crossprod(design_matrix)) %*% t(design_matrix)
betas <- ols_estimator%*%swiss$Agriculture 


# Pretty much all of the below could be done with EITHER augment or get_regression_points
# get_regression points is more concise, but more fragile to in-line calculations
lm_swiss_aug <- augment(lm_swiss)
lm_swiss_pts <- moderndive::get_regression_points(lm_swiss)


# cov, SSs, r, R2
dev_swiss <- swiss %>% 
  mutate(dev_Y = (Agriculture - mean(Agriculture)), 
         dev_X = (Examination - mean(Examination)),
         var_YX = dev_Y * dev_X)

cov_swiss_byhand <- sum(dev_swiss$var_YX)/(nrow(dev_swiss)-1)
cov_swiss_byhand
cov_swiss <- with(swiss,cov(Agriculture, Examination)) 
cov_swiss #Woot, they match!

cor_swiss <- with(swiss,cor(Agriculture, Examination))
cor_swiss
sd_Y <- sd(swiss$Agriculture)
sd_X <- sd(swiss$Examination)
cor2_swiss <- cov_swiss/(sd_Y * sd_X) 
cor2_swiss # They match!!

R2_swiss <-  cor_swiss^2
R2_swiss
glance(lm_swiss)

swiss_ss <- lm_swiss_aug %>% 
  summarise(total_ss = sum((Agriculture - mean(Agriculture))^2),
            resid_ss = sum((Agriculture - .fitted)^2), 
            model_ss = sum((.fitted - mean(Agriculture))^2),
            total = resid_ss + model_ss,
            rsq = model_ss / total_ss,
            var_est = total_ss / (n()-1),
            swiss_var = var(Agriculture))

swiss_ss$rsq #and they match


lm_swiss_aug # Using augment for direct comparison of hat



#Hat
X <- design_matrix
ols_estimator <- ginv(t(X) %*% X) %*% t(X) #derivable, but yuck!
#ols_estimator <- ginv(crossprod(X)) %*% t(X)
#ols_estimator <- solve(crossprod(X)) %*% t(X)
hat_matrix <- X %*% ols_estimator 
hat <- diag(hat_matrix)
n_parameters <- sum(hat) #Including the intercept - only for intercept models??
#NOTE2self - Add all.equal() to the below...
sum(hat_matrix[1,]) # Another cool property - each row sums to 1
sum(hat_matrix[1,]^2) # And another,  hat_ii == sum (hat_ij^2)

SS_ex <- sum((swiss$Examination-mean(swiss$Examination))^2)
hat_ish <- lm_swiss_aug %>% 
  mutate(.hatish = (1/n()+(Examination-mean(Examination))^2/SS_ex))
hat_ish #matches .hat from augment!


#Betas and Fitted 
betas <- ols_estimator%*%swiss$Agriculture 
lm_swiss_fitted <-hat_matrix %*% lm_swiss_aug$Agriculture


#More Outliers - discrepancy via studentized residuals

SE <- glance(lm_swiss)$sigma #SE of the residuals aka ressidual standard error
hat_ish <- hat_ish %>%
  mutate(isr = .resid/(SE*sqrt(1-.hat))) # Internalized student residual - matches augment's .std.resid

hat_ish <- hat_ish %>%
  mutate(isr2 = .resid/sd(.resid)) # Internalized student residual ~ matches augment's .std.resid

hat_ish <- hat_ish %>%
  mutate(esr = rstudent(lm_swiss)) # Externalized student residual. NOT augment's .std.resid


car::outlierTest(lm_swiss) #outlier test with bonferroni correction



#More hat calcs
#1 minus the ratio of residuals with and without the point

fit = lm(mpg ~ wt, mtcars) # OLS including all points
X2 = model.matrix(fit)     # X model matrix
hat2_matrix = X2%*%(solve(t(X2)%*%X2)%*%t(X2)) # Hat matrix
diag(hat2_matrix)[1] # First diagonal point in Hat matrix

fitwithout1 = lm(mpg ~ wt, mtcars[-1,]) # OLS excluding first data point.
new = data.frame(wt=mtcars[1,'wt']) # Predicting y hat in this OLS w/o first point.
y_hat_without = predict(fitwithout1, newdata=new) # ... here it is.

residuals(fit)[1] # The residual when OLS includes data point.
lev = 1 - (residuals(fit)[1]/(mtcars[1,'mpg'] -  y_hat_without)) # Leverage
all.equal(diag(hat2_matrix)[1], lev) #TRUE

# And, for all of the hat values
hatvalues(fit) #Just the hats. ma'am.
augment(fit)

