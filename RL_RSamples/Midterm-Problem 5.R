old_mean <- 9.0
old_var <- 11.0
n <- 10
x10_old <- 8
x10_new <- 12
new_mean <- ((old_mean * n) + (x10_new - x10_old)) / n
new_mean

x10_adj <- (-(x10_old - old_mean)^2 + (x10_new - new_mean)^2) * (1/(n - 1)) # 0.64
mean_1_9 <- (old_mean * n - x10_old) / (n - 1) # calculate mean of x1 to x9
x1_9_adj <- (-((n - 1) * (mean_1_9 - old_mean)^2) + (n - 1)*(mean_1_9 - new_mean)^2) * (1/(n - 2))
new_var <- old_var + x10_adj + x1_9_adj
new_var

