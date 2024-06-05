################
# Paleoclim ET timeseries Willandra Lakes
# ML Fischer 2023/12
################
remove(list=ls()) 

setwd("~/Documents/Projekte/Willandra Lakes/Modelling")
source("ET_model_function.R")

library(raster)
library(rgdal)
library(pastclim)
library(dplyr)

setwd("~/Documents/Projekte/Willandra Lakes/climate_cru/data")
catch <- readOGR(".",layer = "willandrashp") 
catch <- vect(catch)


WL_catch <- region_series(
  bio_variables = c("bio01","bio12"),
  dataset = "Beyer2020",
  crop = catch)

WL_catch_df <- df_from_region_series(WL_catch)

WL_catch_df <- WL_catch_df%>% 
  group_by(time_bp) %>%
  summarise_all(mean, na.rm = TRUE)
WL_catch_df$time_bp <- WL_catch_df$time_bp * -1
WL_catch_df <- WL_catch_df %>% arrange(time_bp)

WL_catch_df <- WL_catch_df[which(WL_catch_df$time_bp<=60000),]



setwd("~/Documents/Projekte/Willandra Lakes/Modelling")

locations <- data.frame(
  name = c("Willandra"), 
  longitude = c(143), latitude = c(-33),
  time_bp = seq(0,-60000,-2000)
)
#locations

WL_ts <- location_series(
  x = locations,
  bio_variables = c("bio01","bio12","temperature_01" ,  "temperature_02" ,  "temperature_03"  , "temperature_04" ,  "temperature_05" ,  "temperature_06"  
                    ,"temperature_07",  "temperature_08" ,  "temperature_09" ,  "temperature_10" ,  "temperature_11",   "temperature_12","precipitation_01","precipitation_02"
                    ,"precipitation_03","precipitation_04","precipitation_05","precipitation_06","precipitation_07","precipitation_08","precipitation_09",
                    "precipitation_10","precipitation_11","precipitation_12","altitude","wind_speed_01","wind_speed_02","wind_speed_03","wind_speed_04"
                    ,"wind_speed_05","wind_speed_06","wind_speed_07","wind_speed_08","wind_speed_09","wind_speed_10","wind_speed_11","wind_speed_12","cloudiness_01"
                    ,"cloudiness_02","cloudiness_03","cloudiness_04","cloudiness_05","cloudiness_06","cloudiness_07","cloudiness_08","cloudiness_09"
                    ,"cloudiness_10","cloudiness_11","cloudiness_12","relative_humidity_01","relative_humidity_02","relative_humidity_03","relative_humidity_04"
                    ,"relative_humidity_05","relative_humidity_06","relative_humidity_07","relative_humidity_08","relative_humidity_09","relative_humidity_10"
                    ,"relative_humidity_11","relative_humidity_12"),
  dataset = "Beyer2020")

WL_ts <- WL_ts %>%  distinct()
WL_ts$time_bp <- WL_ts$time_bp * -1
WL_ts <- WL_ts %>% arrange(time_bp)

WL_ts <- WL_ts[which(WL_ts$time_bp<=60000),]

# Calculate R_sw per month 
# Berger, A., Loutre, M.F. and Yin Q. (2010), Total irradiation during any time interval of the year us-ing elliptic integrals, Quaternary Science Reviews, 29, 1968 - 1982, doi:10.1016/j.quascirev.2010.05.007
library(palinsol)
df_ins <- data.frame(matrix(nrow = 42, ncol = 12))
for(i in 1:9){colnames(df_ins)[i]=paste("insolation_0",i,sep="")}
for(i in 10:12){colnames(df_ins)[i]=paste("insolation_",i,sep="")}
lat_will = -33 * pi / 180
for (i in 1:42){
  orbit<- la04(t=WL_ts$time_bp[i]*-1,degree=FALSE)
  for(j in 0:11){
    day1= (j*30)+1
    day2= (j*30)+30
    df_ins[i,j+1] <- Insol_d1d2(orbit,d1=day1,d2=day2,lat=lat_will,avg=TRUE)# W/m2 
  }}
####



for (i in 0:11){
  if(i<9) {et_name <- paste("evaporation_0",i+1,sep="")}
  if(i>=9) {et_name <- paste("evaporation_",i+1,sep="")}

  for (j in 1:length(WL_ts$time_bp))
  {
    ws_wl <- WL_ts[j,32+i]
    z0_l <- 0.0002 #roughness length lake
    z0       <- z0_l
    u_a      <- (0.4*ws_wl)/ log(10/z0) #friction velocity (windspeed =1.87m/s at 10 m, karman konstant = 0.4)
    r_a_a    <- log(10/z0)/(u_a * 0.4) #atmospheric resitance
    cd_a     <- 0.4*0.4 * (log(r_a_a /(z0))^-2) #surface drag coefficient
    cda_l   <- cd_a
    
    WL_ts[j,et_name] <-   ETa(              t_a_ld    = WL_ts[j,7+i]+273.15, 
                                            emis_ld   = 0.90, 
                                            albedo_ld = 0.06,
                                            rh        = WL_ts[j,56+i]/100,
                                            f_ld      = 1,
                                            cc        = WL_ts[j,44+i]/100,
                                            ws        = WL_ts[j,32+i],
                                            a         = 0.39,
                                            b         = 0.38, 
                                            a_2       = 0.22,
                                            b_2       = 2.00,
                                            cds       = cda_l,
                                            p         = 100612,
                                            r_swc     = df_ins[j,1+i])
    WL_ts[j,et_name] <- WL_ts[j,et_name]/12
  }
}
pcol <- rgb(31/255, 58/255, 147/255, alpha = 1)  # Precipitation
tcol <- rgb(214/255, 90/255, 49/255, alpha = 1)  # Temperature
wscol <- rgb(243/255, 195/255, 73/255, alpha = 1) # Wind Speed
rhcol <- rgb(49/255, 162/255, 189/255, alpha = 1) # Relative Humidity
cccol <- rgb(128/255, 128/255, 128/255, alpha = 1) # Cloud Cover
etcol <- rgb(186/255, 104/255, 200/255, alpha = 1) # Evaporation

par(mfrow=c(6,1))
par(oma=c(1,2,2,2))
par(mar = c(2, 4, 1, 1))  # The order is c(bottom, left, top, right)

WL_ts$prec  <- rowSums( WL_ts[,19:30])
WL_ts$temp  <- rowMeans(WL_ts[, 7:18])
WL_ts$ws    <- rowMeans(WL_ts[,32:43])
WL_ts$rh    <- rowMeans(WL_ts[,56:67])
WL_ts$cc    <- rowMeans(WL_ts[,44:55])
WL_ts$et    <- rowSums(WL_ts[,68:79])

plot(WL_catch_df$time_bp ,WL_catch_df$bio12 ,type="l",col = pcol
     ,ylab="Precipitation (mm)"
     ,axes=F)
axis(2,las=1)

plot(WL_ts$time_bp,WL_ts$temp ,type="l",col = tcol
     ,ylab="Temperature (°C)"
     ,axes=F)
axis(2,las=1)

plot(WL_ts$time_bp,WL_ts$ws ,type="l",col = wscol
     ,ylab="Wind Speed (m/s)"
     ,axes=F)
axis(2,las=1)

plot(WL_ts$time_bp,WL_ts$rh ,type="l",col = rhcol
     ,ylab="Rel. Humidity (%)"
     ,axes=F,ylim = c(max(WL_ts$rh), min(WL_ts$rh)))
axis(2,las=1)

plot(WL_ts$time_bp,WL_ts$cc ,type="l",col = cccol
     ,ylab="Cloud Cover (%)"
     ,axes=F,ylim = c(max(WL_ts$cc), min(WL_ts$cc)))
axis(2,las=1)

plot(WL_ts$time_bp,WL_ts$et ,type="l",col = etcol
     ,ylab="Evaporation (mm)",xlab="Time BP"
     ,axes=F)
axis(2,las=1)
axis(1,las=1)

