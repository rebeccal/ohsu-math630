

advertising <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")

#Fixed - but cool!!
advertising_fit1 <- lm(sales~TV+radio, data = advertising)
sp <- scatterplot3d::scatterplot3d(advertising$TV, 
                                   advertising$radio, 
                                   advertising$sales, 
                                   angle = 45)
sp$plane3d(advertising_fit1, lty.box = "solid")#,
# polygon_args = list(col = rgb(.1, .2, .7, .5)) # Fill color
orig <- sp$xyz.convert(advertising$TV, 
                       advertising$radio, 
                       advertising$sales)
plane <- sp$xyz.convert(advertising$TV, 
                        advertising$radio,  fitted(advertising_fit1))
i.negpos <- 1 + (resid(advertising_fit1) > 0)
segments(orig$x, orig$y, plane$x, plane$y,
         col = c("blue", "red")[i.negpos], 
         lty = 1) # (2:1)[i.negpos]
sp <- FactoClass::addgrids3d(advertising$TV, 
                             advertising$radio, 
                             advertising$sales,
                             angle = 45,
                             grid = c("xy", "xz", "yz"))


##Interactive
rgl::plot3d(advertising$TV, 
            advertising$radio, 
            advertising$sales, type = "p", 
            xlab = "TV", 
            ylab = "radio", 
            zlab = "Sales", site = 5, lwd = 15)
rgl::planes3d(advertising_fit1$coefficients["TV"], 
              advertising_fit1$coefficients["radio"], -1, 
              advertising_fit1$coefficients["(Intercept)"], alpha = 0.3, front = "line")
rgl::segments3d(rep(advertising$TV, each = 2), 
                rep(advertising$radio, each = 2),
                matrix(t(cbind(advertising$sales, predict(advertising_fit1))), nc = 1),
                col = c("blue", "red")[i.negpos], 
                lty = 1) # (2:1)[i.negpos]
#rgl::rgl.postscript("./pics/plot-advertising-rgl.pdf","pdf") # does not really work...