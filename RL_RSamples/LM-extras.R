
library(tidyverse)
library(MASS) #ginv() matrix inversion
library(broom)
library(moderndive)


GGally::ggpairs(swiss)

swiss %>%
  cor()  %>% 
  round(3)
  

swiss %>%
  cor(.,use = "pairwise.complete.obs")

swiss %>%
  corrr::correlate()


ggplot(swiss,aes(x=Examination, y=Agriculture)) +
  geom_smooth(method="lm") +
  geom_point() +
  geom_text(aes(label=rownames(swiss)))


lm_swiss <-lm(Agriculture ~Examination,swiss)
tidy(lm_swiss)
glance(lm_swiss)


# Residual standard error and standard error of the coefficients
design_matrix <- model.matrix(lm_swiss)
rse2 <- sum((swiss$Agriculture - fitted(lm_swiss))^2) / (nrow(design_matrix) - ncol(design_matrix))
sqrt(rse2)
sqrt(diag(solve(crossprod(design_matrix))) * rse2)


# Pretty much all of the below could be done with EITHER augment or get_regression_points
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
ols_estimator <- ginv(t(X)%*%X)%*%t(X) #derivable, but yuck!
hat_matrix <- X%*%ols_estimator 
#hat_matrix <- X%*%ginv(crossprod(X))%*%t(X) #Same
#hat_matrix <- X%*%solve(crossprod(X))%*%t(X) #Same
hat <- diag(hat_matrix)
n_parameters <- sum(hat) #Including the intercept

SS_ex <- sum((swiss$Examination-mean(swiss$Examination))^2)
hat_ish <- lm_swiss_aug %>% 
  mutate(.hatish = (1/n()+(Examination-mean(Examination))^2/SS_ex))
hat_ish #matches!

#Fitted
betas <- ols_estimator%*%swiss$Fertility 
lm_swiss_fitted <-hat_matrix %*% lm_swiss_aug$Fertility


#More Outliers - discrepancy via studentized residuals

SE <- glance(lm_swiss)$sigma #SE of the residuals
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
all.equal(diag(hat2_matrix)[1],lev) #TRUE

augment(fit)

