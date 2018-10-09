
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

lm_swiss_aug # Using augment for direct comparison of hat

#Hat 
X <- design_matrix
#hat_matrix <- X%*%ginv(t(X)%*%X)%*%t(X) #Same
#hat_matrix <- X%*%ginv(crossprod(X))%*%t(X) #Same
hat_matrix <- X%*%solve(crossprod(X))%*%t(X)
aug_hat <- diag(hat_matrix)

#Fitted
lm_swiss_fitted <-hat_matrix %*% lm_swiss_aug$Fertility

