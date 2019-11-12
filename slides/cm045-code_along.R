# Computing the power of a one-sample t-test
# Inspired by; https://www.cyclismo.org/tutorial/R/power.html 

h <- 100      #Hypothesized mean
a <- 105      #Sample mean
s <- 13       #Sample standard deviation
n <- 25       #Sample N
diff <- 5     #Hypothesized diff, e.g. 105-100

d <- (diff)/s #Cohen's d effect size

################################################################
# 1) If we don't have the non-central parameter (this is typical). the below assumes a two-sided t-test, i.e. u0 <> u1 

#NOTE: This is an approximation using a central t-distribution in lieue of a non-central distribution

error <- qt(0.975,df=n-1)*s/sqrt(n) # margin of error
left <- a - error  # left critical value for the sample distribution
right <- a + error # right critical value for the sample distribution

# Compute t-values for the difference, treating the critical values as hypothesized means
assumed_mean <- a - diff # Could be plus or minus, two-tailed. Also could use the hypothesized mean instead. 

# Now, find the critical t-values and overlay them onto a "standard" t-distribution of the difference
# Find ALL the probability that falls outside the critical values, both rightward and leftward!
tleft <-  (left - assumed_mean)/(s/sqrt(n)) 
tright <- (right - assumed_mean)/(s/sqrt(n))

(beta_1 <- pt(tright,df=n-1)-pt(tleft,df=n-1)) #Beta
(power_1 <- 1-beta_1) #Power

t <- (a-h)/(s/sqrt(n)) #Yes, in this case it is also the Cohen's d.
p_value_2_tailed <- 2*min(pt(t,n-1), 1 - pt(t,n-1))

################################################################
# 1+) Don't really need to use the sample mean or the hypothesized mean (assumed above in 1) 
#  - instead just use the standard t distribution and the expected diff to calculate the t-statistic 
#   (like the plot below)!

#NOTE: This is an approximation using a central t-distribution in lieue of a non-central distribution

error <- qt(0.975,df=n-1)*s/sqrt(n) # margin of error, based on the sample n
left <- -error #left critical value 
right <- +error #right critical value

# Compute t-values for the differences, treating the critical values as hypothesized means
tleft <- (left-diff)/(s/sqrt(n))
tright <- (right-diff)/(s/sqrt(n))

(beta_1plus <- pt(tright,df=n-1)-pt(tleft,df=n-1)) # Beta
(power_1plus <- 1-beta_1plus) #Power



###############################################################
# Now some using the ncp - atypical for a power calculation, but good for illustration
###############################################################

ncp <- diff/(s/sqrt(n)) # How many standard errors apart are the means? 
#Note - Power is affected by the effect size, the variance, and the sample size(n)!


################################################################
#2) With computed ncp for t-test with one-sample

t <- qt(0.975,df=n-1)
beta_2 <- pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp) #Beta
power_2 <- 1 - (pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp)) #Power


################################################################
#3) The easy way
#power.t.test(n=n, delta=diff, sd=s, type="one.sample", alternative = "one.sided")
(power_3easy <- power.t.test(n=n, delta=diff, sd=s, type="one.sample", alternative = "two.sided"))

power_3 <- power_3easy %>% 
  select(power)


################################################################
# Potentially helpful plot
library(ggfortify)

# Plot the NULL and critical values
crit <- c(qt(0.025,df=n-1),qt(0.975,df=n-1))
plot <- ggdistribution(dt,seq(-10,10,.01),df=n-1, colour = "red", fill="red") +
  geom_vline(xintercept = crit, color="red")
plot

# And overlay the distribution adjusted using the ncp
ggdistribution(dt,seq(-10,10,.01),df=n-1,ncp=ncp,p=plot, fill="blue")
