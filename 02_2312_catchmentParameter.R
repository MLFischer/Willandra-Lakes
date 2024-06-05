################
# Paleoclim data access
# ML Fischer 2023/03
################

remove(list=ls()) 
library(raster)
library(rgdal)
library(pastclim)
library(ggplot2)

setwd("~/Documents/Projekte/Willandra Lakes/climate_cru/data")
catch <- readOGR(".",layer = "willandrashp") 
catch <- vect(catch)


 WL_catch <- region_series(
              bio_variables = c("bio01","bio12","temperature_01" ,  "temperature_02" ,  "temperature_03"  , "temperature_04" ,  "temperature_05" ,  "temperature_06"  
                     ,"temperature_07" ,  "temperature_08" ,  "temperature_09" ,  "temperature_10" ,  "temperature_11",   "temperature_12","precipitation_01","precipitation_02"
                     ,"precipitation_03","precipitation_04","precipitation_05","precipitation_06","precipitation_07","precipitation_08","precipitation_09",
                     "precipitation_10","precipitation_11","precipitation_12","altitude"),
              dataset = "Beyer2020",
              crop = catch)
 
 WL_catch_df <- df_from_region_series(WL_catch)
 WL_catch_df$lake <- "No Lake and/or No Data"
 WL_catch_df$time_bp <- WL_catch_df$time_bp*-1
 
 WL_catch_df <- WL_catch_df[which(WL_catch_df$time_bp<=60000),]
 
 
 WL_catch_df$lake[WL_catch_df$time_bp>18400 &WL_catch_df$time_bp<25200 ] <- "Oscillating Lake"
 WL_catch_df$lake[WL_catch_df$time_bp>22700 &WL_catch_df$time_bp<24700 ] <- "Mega-lake"
 WL_catch_df$lake[WL_catch_df$time_bp>32000 &WL_catch_df$time_bp<41200 ] <- "Oscillating Lake"
 WL_catch_df$lake[WL_catch_df$time_bp>41300 &WL_catch_df$time_bp<61100 ] <- "Permanent Lake"
 
  beautiful_gradient <- c("#9ec5ab", "#c5d8b5", "#e5ddb3", "#f4d198", "#e1c09e", "#bf7c6b")

 ggplot(WL_catch_df) +
   geom_point(aes(x = precipitation_01, y = temperature_01, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_02, y = temperature_02, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_03, y = temperature_03, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_04, y = temperature_04, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_05, y = temperature_05, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_06, y = temperature_06, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_07, y = temperature_07, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_08, y = temperature_08, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_09, y = temperature_09, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_10, y = temperature_10, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_11, y = temperature_11, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   geom_point(aes(x = precipitation_12, y = temperature_12, color = altitude), shape = 16, size = 1, alpha = 0.5) +
   labs(x = "Monthly Precipitation", y = "Monthly Temperature") +
   scale_color_gradientn(name = "Elevation", colours = beautiful_gradient,
                         values = scales::rescale(c(0,500, max(WL_catch_df$altitude, na.rm = TRUE)))) +
   facet_wrap(~lake) +
   theme_minimal()
 


