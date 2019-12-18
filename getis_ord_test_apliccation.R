library(raster)
library(rgeos)
library(rgdal)
library(tmap)
library(tidyverse)

"
***************************************************************************
getis_ord_test_apliccation.R
---------------------
Date                 : December 2019
Copyright            : (C) 2019 by Felipe Carranza
Email                : fcarranza@protonmail.com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the MIT  License *
*                                                                         *
*                                                                         *
***************************************************************************

__author__ = 'Felipe Carranza'
__date__ = 'December 2019'
__copyright__ = '(C) 2019, Felipe Carranza'
"
"Real case applied to El Salvador census 2007"

#Read your dataset with muncipio, and your variable to test, in this example is a proper value of life expectancy
esperanza_municipios_m = data.frame(read.csv("esperanza_municipios_mujeres.csv"))
#library(raster)
esperanza_municipios_m$Studysection.Studysection.munic_f= as.character(esperanza_municipios_m$Studysection.Studysection.munic)

#Adjust the municipal code
esperanza_municipios_m = esperanza_municipios_m %>% 
  mutate(COD_MUN4.C.254_qgis = case_when( nchar(esperanza_municipios_m$Studysection.Studysection.munic_f) < 4 ~ paste0("0",esperanza_municipios_m$Studysection.Studysection.munic_f), TRUE ~ esperanza_municipios_m$Studysection.Studysection.munic_f)) 

#Read the shape with latittude and longitud
s2 <-readOGR(".", "LIM_MUNICIPAL")
s2 <- s2[(s2$NA3 != "0000"),]

length(s$NA3)
#Get de lattitude and longitud and merge with your data, exclude island if you want
s2 <- s2[(s2$NA3 != "1410"),]
OA.census4 <- merge(esperanza_municipios_m,s2, by.x="COD_MUN4.C.254_qgis", by.y="NA3")

#custom your distance
nb <-dnearneigh(coordinates(s2),0,10009)
nb_lw <- knearneigh(coordinates(s2), k=4)
#Create the Spatial weights for neighbours lists, read an test the style that give you the  best result
nb_lw <-nb2listw(nb, style ='W',zero.policy = TRUE)

summary(nb_lw)

#plot with nivel confidence
OA.census4$ex.Gi = localG(OA.census4$ex, listw=nb_lw,zero.policy = TRUE)
OA.census4$ex.Gi[is.na(OA.census4$ex.Gi)] = 0
plot(s2, main="Local Getis-Ord Municipios", cex.main=1.0, font.main=1, axes=TRUE)
zkrit10 = qnorm(0.975) #confidence level
OA.census4$ex.GisignH = OA.census4$ex.Gi>zkrit10
plot(s2[OA.census4$ex.GisignH, ], col="red", add=TRUE)
OA.census4$ex.GisignL = -zkrit10>OA.census4$ex.Gi
plot(s2[OA.census4$ex.GisignL, ], col="BLUE", add=TRUE)

