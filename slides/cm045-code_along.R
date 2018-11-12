# Computing the power of a one-sample t-test
# Inspired by; https://www.cyclismo.org/tutorial/R/power.html 

h <- 100  #Hypothesized mean
a <- 105  #Sample mean
s <- 13   #Sample standard deviation
n <- 25   #Sample N
diff <- 5 #Hypothesized diff, e.g. 105-100
d <- (diff)/s #Cohen's d effect size


#1) If we don't have the non-central parameter (typical). the below assumes a two-sided t-test, i.e. u0 <> u1 
    #!! This is an approximations using a central t-distribution in lieue of a non-crentral distribution
error <- qt(0.975,df=n-1)*s/sqrt(n) # margon of error
left <- a-error  # left critical value for the sample distribution
right <- a+error # right critical value for the sample distribution

# Compute t-values for the difference, treating the critical values as hypothesized means
assumed <- a - diff # Could be plus or minus, two-tailed. ALso could use the hypothesized mean instead. 
tleft <-  (left - assumed)/(s/sqrt(n)) # Overlay the critical values onto a "standard" t-distribution of the difference
tright <- (right - assumed)/(s/sqrt(n))
p <- pt(tright,df=n-1)-pt(tleft,df=n-1)
p #beta
1-p #power


#1+) Don't really need to use the sample mean - instead just use the standard distribution and the diff (like the plot below)!
#!! This is an approximations using a central t-distribution
error <- qt(0.975,df=n-1)*s/sqrt(n) # margon of error
left <- -error #left critical value 
right <- +error #right critical value

# Compute t-values for the differences, treating the critical values as hypothesized means
tleft <- (left-diff)/(s/sqrt(n))
tright <- (right-diff)/(s/sqrt(n))
p <- pt(tright,df=n-1)-pt(tleft,df=n-1)
p #beta
1-p #power



#2) With computed ncp for t-test with one-sample
ncp <- diff/(s/sqrt(n)) # How many standard deviations apart are the means?
# ncp <- d*sqrt(n) # power is affected by the effect size, the n, and the variance!!
t <- qt(0.975,df=n-1)
pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp) #beta
1-(pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp)) #power


#3) The easy way
#power.t.test(n=n, delta=diff,sd=13, type="one.sample", alternative = "one.sided")
power.t.test(n=n, delta=diff,sd=13, type="one.sample", alternative = "two.sided")

#Potentially helpful plot
crit <- c(qt(0.025,df=n-1),qt(0.975,df=n-1))
plot <- ggdistribution(dt,seq(-10,10,.1),df=24, colour="red") +
  geom_vline(xintercept = crit, color="red")
plot
ggdistribution(dt,seq(-10,10,.1),df=24,ncp=ncp,p=plot, colour="blue")
