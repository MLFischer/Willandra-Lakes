#### Header ----
# Lake Balance Model Willandra Lakes
# ML Fischer 2023/03


remove(list=ls()) 
library(raster)
library(magick)
library(rgdal)

setwd("~/Documents/Projekte/Willandra Lakes/Modelling")
load("ETm.Rdata")
load("Pm.Rdata")
setwd("~/Documents/Projekte/Willandra Lakes/Modelling v6 sim")

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

par(mfrow=c(1,1))
par(mar = c(5, 2, 1, 2) + 0.1)


### oscilating lake simulations ----
for (jk in 1:500) {
  
pvec<- (pvec)/ sum(pvec)
pvec <- pvec * jk/100
influx_mon <- pvec

# intial lake level
mulurulu$lev[1] <- 13
garnpung$lev[1] <- 18
leaghur$lev[1]  <- 3*0
mungo$lev[1]    <- 0

source("LBM_Will_v3.R")

filename <- paste0("plot_", sprintf("%03d", jk), ".png")
png(filename)

plot(mulurulu$lev,type="l",ylim=c(0,20),
     xlab="Time (months)",
     ylab="Relative Lake Level (m)",
     axes=F,
     col=col_mulurulu)

axis(2,las=1)
axis(1,las=1)

title(main = paste(jk/100,"km^3 per year water"))

lines(garnpung$lev,col=col_garnpung)
lines(leaghur$lev ,col=col_leaghur)
lines(mungo$lev   ,col=col_mungo)
lines(arumpo$lev   ,col=col_arumpo)



dev.off()
}

# List the files created by the loop
files <- list.files(pattern = "plot_\\d{3}\\.png")

# Read the images into a magick image object
images <- image_read(files)

# Create GIF
gif_filename <- "animation.gif"
image_animate(image_join(images), fps = 10, loop = 0) %>%
  image_write(gif_filename)

