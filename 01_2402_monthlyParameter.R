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


setwd("~/Documents/Projekte/Willandra Lakes/Modelling")

a <- get_vars_for_dataset(  dataset = "Beyer2020", details = TRUE)

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
#WL_ts <- cbind(WL_ts,df_ins)
####
setwd("~/Documents/Projekte/Willandra Lakes/climate_cru/data")
catch <- readOGR(".",layer = "willandrashp") 
catch <- vect(catch)


WL_catch <- region_series(
  bio_variables = c("bio01","bio12","temperature_01","temperature_02","temperature_03"  , "temperature_04" ,  "temperature_05" ,  "temperature_06"  
                    ,"temperature_07", "temperature_08" , "temperature_09","temperature_10" ,  "temperature_11",   "temperature_12","precipitation_01","precipitation_02"
                    ,"precipitation_03","precipitation_04","precipitation_05","precipitation_06","precipitation_07","precipitation_08","precipitation_09",
                    "precipitation_10","precipitation_11","precipitation_12","altitude"),
  dataset = "Beyer2020",
  crop = catch)

WL_catch_df <- df_from_region_series(WL_catch)
WL_catch_df$lake <- "No Lake and/or No Data"
WL_catch_df$time_bp <- WL_catch_df$time_bp*-1
WL_catch_df <- WL_catch_df[which(WL_catch_df$time_bp<=60000),]
WL_catch_df <- WL_catch_df %>%
  group_by(time_bp) %>%
  summarize_all(mean, na.rm = TRUE)

colnames(WL_catch_df)[18]
colnames(WL_catch_df)[29]

setwd("~/Documents/Projekte/Willandra Lakes/Modelling")





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


par(mfrow=c(7,1))
par(oma=c(1,2,2,2))
par(mar = c(2, 4, 1, 1))  # The order is c(bottom, left, top, right)
mon <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

pvec  <- WL_catch_df[1,18:29]
tvec  <- WL_ts[1, 7:18]
wsvec <- WL_ts[1,32:43]
rhvec <- WL_ts[1,56:67]
ccvec <- WL_ts[1,44:55]
etvec <- WL_ts[1,68:79]
insvec<- df_ins[1,1:12]

colnames(WL_ts)[79]

pcol  <- rgb(31/255, 58/255, 147/255, alpha = 0.25)  # Precipitation
tcol  <- rgb(214/255, 90/255, 49/255, alpha = 0.25)  # Temperature
wscol <- rgb(243/255, 195/255, 73/255, alpha = 0.25) # Wind Speed
rhcol <- rgb(49/255, 162/255, 189/255, alpha = 0.25) # Relative Humidity
cccol <- rgb(128/255, 128/255, 128/255, alpha = 0.25) # Cloud Cover
etcol <- rgb(186/255, 104/255, 200/255, alpha = 0.25) # Evaporation
inscol<- rgb(255/255, 140/255, 0/255, alpha = 0.25)



plot(1:12,pvec,type="l",ylim=c(0,100),col = pcol
     ,ylab="Precipitation (mm)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  pvec  <- WL_catch_df[i,18:29]
  lines(1:12,pvec,type="l",col=pcol,lwd=1)
}
axis(2,las=1)

plot(1:12,tvec,type="l",ylim=c(5,30),col = tcol
     ,ylab="Temperature (°C)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  tvec <- WL_ts[i, 7:18]
  lines(1:12,tvec,type="l",col=tcol,lwd=1)
}
axis(2,las=1)

plot(1:12,wsvec,type="l",ylim=c(1,5),col = wscol
     ,ylab="Wind Speed (m/s)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  wsvec <- WL_ts[i,32:43]
  lines(1:12,wsvec,type="l",col=wscol,lwd=1)
}
axis(2,las=1)

plot(1:12,rhvec,type="l",ylim=c(20,80),col = rhcol
     ,ylab="Relative Humidity (%)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  rhvec <- WL_ts[i,56:67]
  lines(1:12,rhvec,type="l",col=rhcol,lwd=1)
}
axis(2,las=1)

plot(1:12,ccvec,type="l",ylim=c(0,70),col = cccol
     ,ylab="Cloud Cover (%)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  ccvec <- WL_ts[i,44:55]
  lines(1:12,ccvec,type="l",col=cccol,lwd=1)
}
axis(2,las=1)

plot(1:12,insvec,type="l",ylim=c(150,500),col = inscol
     ,ylab="Insolation (w/m^2)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  ccvec <- df_ins[i,1:12]
  lines(1:12,ccvec,type="l",col=inscol,lwd=1)
}
axis(2,las=1)

plot(1:12,etvec,type="l",ylim=c(0,250),col = etcol
     ,ylab="Evaporation (mm)"
     ,xlab="Month of the Year"
     ,axes=F)
for (i in 1:length(WL_ts$time_bp)){
  etvec <- WL_ts[i,68:79]
  lines(1:12,etvec,type="l",col=etcol,lwd=1)
}
axis(2,las=1)
axis(1,las=1,labels=mon,at=1:12)


colnames(WL_ts[68])
ETm<-   c(mean(WL_ts[,68]))
for(i in 69:79){
  ETm<-  c(ETm,mean(WL_ts[,i]))
  }
ETm
save(ETm, file= "ETm.Rdata")
###
colnames(WL_ts[19])
Pm<-   c(mean(WL_ts[,19]))
for(i in 20:30){
  Pm<-  c(Pm,mean(WL_ts[,i]))
}
Pm
pvec <- as.vector(t(pvec))
save(pvec, file= "Pm.Rdata")


###
colnames(WL_ts[30])
c(min(WL_catch_df[,18:29]))
c(max(WL_catch_df[,18:29]))

c(min(WL_catch_df[1,18:29]))
c(max(WL_catch_df[1,18:29]))

WL_catch_df[1,18:29]

colnames(WL_ts[7])
colnames(WL_ts[18])
c(min(WL_ts[1,7:18]))
c(max(WL_ts[1,7:18]))

colnames(WL_ts[68])
colnames(WL_ts[79])
c(min(WL_ts[,7:68]))
c(max(WL_ts[,7:79]))

colnames(WL_ts[56])
colnames(WL_ts[67])
c(min(WL_ts[,56:67]))
c(max(WL_ts[,56:67]))

colnames(WL_ts[44])
colnames(WL_ts[55])
c(min(WL_ts[,44:55]))
c(max(WL_ts[,44:55]))

colnames(WL_ts[44])
colnames(WL_ts[55])
c(min(df_ins[1,]))
c(max(df_ins[1,]))

colnames(WL_ts[32])
colnames(WL_ts[43])
c(min(WL_ts[,32:43]))
c(max(WL_ts[,32:43]))
