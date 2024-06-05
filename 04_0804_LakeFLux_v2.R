#### Header ----
# Lake Balance Model Willandra Lakes
# ML Fischer 2023/03


remove(list=ls()) 
library(raster)
library(rgdal)

setwd("~/Documents/Projekte/Willandra Lakes/Modelling")
load("ETm.Rdata")
load("Pm.Rdata")
setwd("~/Documents/Projekte/Willandra Lakes/Modelling v6")

# dummy lists for model:
mulurulu <- list() # flows to -> gar
garnpung <- list() # flows to -> lea
leaghur  <- list() # flows to first -> mungo, then -> arumpo
mungo    <- list() # flows together then to -> arumpo
arumpo   <- list() # outflow then back to Lachlan River

# Water flux input km^3
influx_mon <- c(0,0,0,0,0,0,0,0,0,0,0,0)

# ETa lakes (annual) mm/a
ETa_mon <- ETm
sum(ETa_mon)

#lake size approx. km^2
mulurulu$size <- 87    #125
garnpung$size <- 389   #400
leaghur$size  <- 134   #125
mungo$size    <- 132   #140
arumpo$size    <- 146   #140

#lake level max m
mulurulu$maxlevel <- 13
garnpung$maxlevel <- 18
leaghur$mungolevel<- 5
leaghur$maxlevel  <- 15
mungo$mungolevel <- 8
mungo$maxlevel    <- 18
arumpo$maxlevel    <- 20.5   #140

#plot colors
col_mulurulu <- "darkgoldenrod3"
col_garnpung <- "darkorchid4"
col_leaghur  <- "darkslategrey"
col_mungo    <-  "dodgerblue4"
col_arumpo   <-  "darkorange4" 


# intial lake level
mulurulu$lev[1] <- 8*0
garnpung$lev[1] <- 9*0
leaghur$lev[1]  <- 6*0
mungo$lev[1]    <- 7*0
arumpo$lev[1]    <- 7*0

time_months <-240

par(mfrow=c(3,3))
par(mar = c(5, 2, 1, 2) + 0.1)


### oscilating lake simulations ----

pvec<- (pvec)/ sum(pvec)
pvec <- pvec * 1
influx_mon <- pvec

# intial lake level
mulurulu$lev[1] <- 13
garnpung$lev[1] <- 18
leaghur$lev[1]  <- 3*0
mungo$lev[1]    <- 0

source("LBM_Will_v3.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)
title(main = "tec. & osc., 1 km^3 per year simulations")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

# intial lake level
mulurulu$lev[1] <- 13
garnpung$lev[1] <- 18
leaghur$lev[1]  <- 2.5*0
mungo$lev[1]    <- 7*0

influx_mon <- c(0,0,0,0,0,0,0,0,0,1,0,0) #seasonal mungo peaks
source("LBM_Will_v3.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "seas. & osc.")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

# intial lake level
mulurulu$lev[1] <- 13
garnpung$lev[1] <- 10
leaghur$lev[1]  <- 0
mungo$lev[1]    <- 0

influx_mon_decad <-rep( c(  rep(0,6),0,5,0,0,0,0,  rep(0,48) ),10)



source("LBM_Will_v3_decad.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "decad. & osc.")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)


### 2km3 full lake simulation ----

# intial lake level
mulurulu$lev[1] <- 8*0
garnpung$lev[1] <- 9*0
leaghur$lev[1]  <- 6*0
mungo$lev[1]    <- 7*0
#Tectonics full lake 
pvec<- (pvec)/ sum(pvec)
pvec <- pvec * 2
influx_mon <- pvec
source("LBM_Will_v3.R")



plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)
title(main = "tec. & full, 2 km^3 per year simulations")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

influx_mon <- c(0,0,0,0,0,0,0,0,0,2,0,0) #seasonal mungo peaks
source("LBM_Will_v3.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "seas. & full")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

influx_mon_decad <-rep( c(  rep(0,6),0,10,0,0,0,0,  rep(0,48) ),10)

source("LBM_Will_v3_decad.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "decad. & full")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)


### 4km3 full lake simulations ----

pvec<- (pvec)/ sum(pvec)
pvec <- pvec * 4
influx_mon <- pvec
source("LBM_Will_v3.R")



plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)
title(main = "tec. & full, 4 km^3 simulations")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

influx_mon <- c(0,0,0,0,0,0,0,0,4,0,0,0) #seasonal mungo peaks
source("LBM_Will_v3.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "seas. & full")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

influx_mon_decad <-rep( c(  rep(0,6),0,20,0,0,0,0,  rep(0,48) ),10)

source("LBM_Will_v3_decad.R")

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = "decad. & full")

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)

legend(x = "bottomright",      cex=1,                           # Position
       legend = c("Muluru","Garnpung","Leaghur","Mungo","Arumpo"),    # Legend texts
       col = c(col_mulurulu, col_garnpung,col_leaghur,col_mungo,col_arumpo),   #  Line colors
       lwd = 2)  

print("done")

