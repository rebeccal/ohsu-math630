
# Mostly based on Salvatore Mangiafico's "R Companion":
#https://rcompanion.org/rcompanion/d_05.html



############################################################################
#Set up the data
############################################################################

Input =("
Location   Aam
Tillamook  0.0571
Tillamook  0.0813
Tillamook  0.0831
Tillamook  0.0976
Tillamook  0.0817
Tillamook  0.0859
Tillamook  0.0735
Tillamook  0.0659
Tillamook  0.0923
Tillamook  0.0836
Newport    0.0873
Newport    0.0662
Newport    0.0672
Newport    0.0819
Newport    0.0749
Newport    0.0649
Newport    0.0835
Newport    0.0725
Petersburg 0.0974
Petersburg 0.1352
Petersburg 0.0817
Petersburg 0.1016
Petersburg 0.0968
Petersburg 0.1064
Petersburg 0.1050
Magadan    0.1033
Magadan    0.0915
Magadan    0.0781
Magadan    0.0685
Magadan    0.0677
Magadan    0.0697
Magadan    0.0764
Magadan    0.0689
Tvarminne  0.0703
Tvarminne  0.1026 
Tvarminne  0.0956
Tvarminne  0.0973
Tvarminne  0.1039
Tvarminne  0.1045
")

Data = read.table(textConnection(Input),header=TRUE)


#Specify the order of factor levels for plots and Dunnett comparison
library(dplyr)
Data =
  mutate(Data,
         Location = factor(Location, levels=unique(Location)))


# Summarize using FSA (not installed)
#library(FSA)  
#Summarize(Aam ~ Location,
#          data=Data,
#          digits=3)
# Location  n  mean    sd   min    Q1 median    Q3   max
# 1  Tillamook 10 0.080 0.012 0.057 0.075  0.082 0.085 0.098
# 2    Newport  8 0.075 0.009 0.065 0.067  0.074 0.082 0.087
# 3 Petersburg  7 0.103 0.016 0.082 0.097  0.102 0.106 0.135
# 4    Magadan  8 0.078 0.013 0.068 0.069  0.073 0.081 0.103
# 5  Tvarminne  6 0.096 0.013 0.070 0.096  0.100 0.104 0.104



############################################################################
# Summarize using dplyr
############################################################################

library(skimr)
Data %>% 
  group_by(Location) %>% 
  skim()

# Compute the grandMean
GrandMean <- Data %>%
  group_by(Location) %>%
  summarize(meangrp = mean(Aam)) %>% 
  summarize(mean(meangrp)) %>% 
  pull()


library(ggplot2)
ggplot(Data, aes(x=Location, y=Aam)) +
  geom_boxplot() +
  geom_hline(yintercept = GrandMean, color = "red", linetype = 2, size = 1)



############################################################################
# Make a couple models
############################################################################

# Make the linear model
model = lm(Aam ~ Location,
           data=Data)

# Make an aov model - basically a wrapper for lm that produces more anova-like output by default
aovmodel = aov(Aam ~ Location,
               data=Data)

############################################################################
# Run some ANOVAs
############################################################################

library(car)

Anova(model, type="II") # Does not do type I - as you can see, not relevant for THIS problem


### Also,can use type="III", you need the following line before the analysis
### options(contrasts = c("contr.sum", "contr.poly"))
### Anova(model, type="III")

anova(model)                               # ONLY produces type I sum of squares - later

summary(aovmodel) # Same as above


############################################################################
## Linear Model Review - Computations for the summary statistics
## Blatantly copied the below from:
#http://www.learnbymarketing.com/tutorials/explaining-the-lm-summary-in-r/
############################################################################

# Look at the model-level statistics and compare the F and Probability to anovas above
summary(model)     # Produces r-square, overall p-value, parameter estimates


#Residual Standard error (Like Standard Deviation)
k   <- length(model$coefficients)-1 #Subtract one to ignore intercept
SSE <- sum(model$residuals**2)
n   <- length(model$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#Multiple R-Squared (Coefficient of Determination)
y    <- Data$Aam
SSyy <- sum((y-mean(y))**2)
SSE  <- sum(model$residuals**2)
(SSyy-SSE)/SSyy
# Alternatively
1-SSE/SSyy

#Adjusted R-Squared
k    <- length(model$coefficients)-1 #Subtract one to ignore intercept
SSyy <- sum((y-mean(y))**2)
SSE  <- sum(model$residuals**2)
n    <- length(model$residuals)
1-(SSE/SSyy)*(n-1)/(n-(k+1)) #Adjusting for the number of IVs

#F-Statistic
#Ho: All coefficients are zero
#Ha: At least one coefficient is nonzero
#Compare test statistic to F Distribution table
n <- length(y)
SSE <- sum(model$residuals**2)
SSyy <- sum((y-mean(y))**2)
k <- length(model$coefficients)-1
(F <-((SSyy-SSE)/k) / (SSE/(n-(k+1))))

#P-value
1-pf(F,k,model$df.residual)


############################################################################
#Look at the residuals - need to be homoskedastic and un-biased
############################################################################

## Look at the residuals
hist(residuals(model),
     col="darkgray")

plot(fitted(model),
     residuals(model))



############################################################################
#Comparing means across levels (contrasts!!)
############################################################################

library(multcomp) # For glht()

# Contrasts that look like lm()
###############################

#Dunnett compares each level to the control
#Control is assumed to be the first level
#Same as the default for lm() - compare them!
mc <- glht(model,
           mcp(Location = "Dunnett"))
summary(mc, test=adjusted("none"))


#Roll you own comparison - this matches "Dunnett", which matches the lm() default
contr <- rbind("Newport - Tillamook"     = c(-1, 1, 0, 0, 0),
               "Petersburg -  Tillamook" = c(-1, 0, 1, 0 ,0), 
               "Petersburg -  Tillamook" = c(-1, 0, 0, 1 ,0), 
               "Petersburg -  Tillamook" = c(-1, 0, 0, 0 ,1))

# Same as above
mc <-  glht(model,
          linfct = mcp(Location = contr))
summary(mc, test=adjusted("none"))


#Try it with the aov model - same, right?
aovmc <- glht(aovmodel,
          linfct = mcp(Location = contr))
summary(aovmc, test=adjusted("none"))


# Pairwise comparisons
###############################

#Using glht
mc <- glht(model,
           mcp(Location = "Tukey"))
summary(mc, test=adjusted("none"))


# Pairwise comparisons using lsmeans
# Good for unbalanced groups - not so interesting here
library(lsmeans)
#library(multcompView)
leastsquare = lsmeans(model,
                      pairwise ~ Location,
                      adjust = "none")
leastsquare



#Contrast against Grand Mean
###############################

# using glht
mc_gm <- glht(model,
           mcp(Location = "GrandMean"))
summary(mc_gm, test=adjusted("none"))

# Same, but using the contr.sum() function to generate the contrast
# the parameter for contr.sum() is the number of groups - 1
mc_gm <-  glht(model,
            linfct = mcp(Location = contr.sum(5)))
summary(mc_gm, test=adjusted("none"))


############################################################################
# And now, let's adjust those p's
############################################################################

#Using glht - with the default adjustment - probably Tukey
mc <- glht(model,
           mcp(Location = "Tukey"))
summary(mc)

# For Bonferroni, the p-values are those of the un-adjusted model * the number of comparisons (10)
mc <- glht(model,
           mcp(Location = "Tukey"))
summary(mc, test = adjusted("bonferroni"))


mc <- glht(model,
           mcp(Location = "Tukey"))
summary(mc, test = adjusted("BH"))


############################################################################
# And compute eta-squared
############################################################################

#Hmmm, is it R2?
summary(model)

# Using MOTE for computing eta with F
# All these params (except alpha) are from the above summary
library(MOTE)
eta.F(4, 34, 7.12, a = .05)$eta

