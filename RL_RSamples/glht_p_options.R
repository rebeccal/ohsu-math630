library(multcomp)
treatment <- as.factor(c(rep("A",5),rep("B",5),rep("C",5),rep("D",5),rep("E",5)))
# treatment

y <- c(19.08,17.07,18.91,15.09,17.00,22.04,21.44,18.82,20.49,19.34,
       18.68,19.86,19.68,17.78,17.86,16.99,13.18,16.97,12.90,15.00,
       15.34,13.52,15.23,15.63,13.21)

aovmodel <- aov(y~treatment)
summary(aovmodel)

# Run this line for any MCP
mcp <- glht(aovmodel,linfct=mcp(treatment="Tukey"))

# To output repeated-t and Fisher's LSD Test results
summary(mcp,test=univariate())

# To output Tukey's Test results
summary(mcp)

# To output Bonferroni's Test results
summary(mcp,test=adjusted(type="bonferroni"))

# By default the first level is the control. To make a different level
# the control level, redefine the treatment labels by putting
# an underscore at the beginning of the control level (e.g., "_C").

trt <- as.factor(c(rep("A",5),rep("B",5),rep("_C",5),rep("D",5),rep("E",5)))
trt
aovdunnett <- aov(y~trt)

# Dunnett's Test = multiple vs control
dunnett2 <- glht(aovdunnett,linfct=mcp(trt="Dunnett"))
summary(dunnett2)

dunnettL <- glht(aovdunnett,linfct=mcp(trt="Dunnett"),alternative="less")
summary(dunnettL)

dunnettU <- glht(aovdunnett,linfct=mcp(trt="Dunnett"),alternative="greater")
summary(dunnettU)
