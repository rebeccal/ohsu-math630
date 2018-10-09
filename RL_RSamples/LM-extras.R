
library(tidyverse)

ggplot(swiss,aes(Agriculture, Fertility)) +
geom_smooth(method="lm") +
geom_point()

lm_swiss <-lm(Fertility ~Education,swiss)
summary(lm_swiss)

# Residual standard error and standard error of the coefficients
design_matrix <- model.matrix(lm_swiss)
rse2 <- sum((swiss$Fertility - fitted(lm_swiss))^2) / (nrow(design_matrix) - ncol(design_matrix))
sqrt(rse2)
sqrt(diag(solve(crossprod(design_matrix))) * rse2)


# Pretty much all of the below could be done with either augment or get_regression_points
lm_swiss_aug <- augment(lm_swiss)
lm_swiss_pts <- moderndive::get_regression_points(lm_swiss)


# cov, SSs, r, R2
cov_swiss <- with(swiss,cov(Fertility, Education))
cor2_swiss <- cov_swiss/sd(swiss$Fertility)*sd(swiss$Education) # nope
cor2_swiss
cor_swiss <- with(swiss,cor(Fertility, Education))
cor_swiss

R2_swiss <-  cor_swiss^2


swiss_ss <- lm_swiss_aug %>% 
  summarise(total_ss = sum((Fertility - mean(Fertility))^2),
            resid_ss = sum((Fertility - .fitted)^2), 
            model_ss = sum((.fitted - mean(Fertility))^2),
            total = resid_ss + model_ss,
            rsq = model_ss / total_ss,
            var_est = total_ss / (n()-1),
            swiss_var = var(Fertility))


lm_swiss_aug # Using augment for direct comparison of hat
#Hat 
X <- design_matrix
#hat_matrix <- X%*%ginv(t(X)%*%X)%*%t(X) #Same
#hat_matrix <- X%*%ginv(crossprod(X))%*%t(X) #Same
hat_matrix <- X%*%solve(crossprod(X))%*%t(X)
aug_hat <- diag(hat_matrix)

#Fitted
lm_swiss_fitted <-hat_matrix %*% lm_swiss_aug$Fertility

