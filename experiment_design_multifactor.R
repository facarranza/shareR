"
***************************************************************************
experiment_design_multifactor.R
---------------------
Date                 : April 2020
Copyright            : (C) 2020 by Felipe Carranza
Email                : fcarranza@protonmail.com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the MIT  License *
*                                                                         *
*                                                                         *
***************************************************************************
"

#multifactor experiment
exa = read.csv("bd.csv")
head(exa)
"
FIELDS:
Teaching ID:Teacher identification number
Turn:Shift taught by the teacher in the study center.
Modality:Study center modality (General, Private, Indigenous)
Gender:Teacher's gender
Function:Role of the teacher in the study center
Degree:Degree taught by the teacher
Career:Indicates whether the teacher enrolled in the 2011-2012 school year in the Teaching Career
Category:Teacher category to take the 2020 Training Course
Area:Stratum of the locality where the work center is located
Note: (score) Successes obtained by the teacher in the evaluation exam 2019
"


#exploring Note (score), like dependent variable,

hist(exa$Nota, breaks=100)
#attach(exa)
#install.packages("rpart")
"library(rpart)
(tmod <- rpart(Nota ~ Grado, data=exa))

summary(tmod)
plot(tmod)
text(tmod)
tmod
"
############################################

# It was decided to group the nocturnal and Saturday cat
#in the wakeups
exa_temp= exa %>% 
  mutate(cat_turno = case_when( Turno  == "Vespertino" ~  "Vespertino_otro",
                                Turno  == "Matutino" ~  "Matutino", 
                                Turno  == "Nocturno" ~  "Vespertino_otro",
                                Turno  == "Sabatino" ~  "Vespertino_otro",
                                TRUE ~ "Vespertino_otro") ) 


##############################################
#Finding the number of samples by levels
library(daewr)

exa_temp=exa
exa_temp$cat_alumno = factor(exa_temp$cat_alumno)
#medias
a =tapply(exa_temp$Nota,list(exa_temp$Grado, exa_temp$Carrera,exa_temp$Area, exa_temp$cat_turno), mean)
#count
b =tapply(exa_temp$Nota,list(exa_temp$Grado, exa_temp$Carrera,exa_temp$Area, exa_temp$cat_turno), length)
a
b

dim(a) = NULL
a
#MIN MAX
summary(a)
#summary(a)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#42.55   46.72   53.34   52.15   55.75   61.95

Prom = 61.95 - 42.55 #19.4
(sd_power =sd(exa_temp$Nota))
#11.6433
# 48 levels by multiplying all categories of variables
#selected
(p <- power.anova.test(groups = 48, 
                       between.var = Prom, within.var =sd_power, 
                       n=2))

# power = 0.9999882 con 2 minimo

########################################################################

#Choosing sample
# more than two can be chosen but for this experiment
# 2 have been chosen
#ordering the dataset
exa_temp= exa_temp[with(exa_temp, order(Grado,Carrera,Area,cat_turno,Nota)), ]

#sample variables
muestra_total=data.frame()
estrato=data.frame
h=1

str(exa_temp)
exa_temp$Grado = factor(exa_temp$Grado)

xturno= unique(factor(exa_temp$cat_turno))
xturno = xturno[!is.na(xturno)]

xarea= unique(exa_temp$Area)
xarea = xarea[!is.na(xarea)]

xgrado = unique(factor(exa_temp$Grado))
xgrado = xgrado[!is.na(xgrado)]


#Grado


#selecting randomly in each of the 48 levels
for(i in unique(xarea)){
  for(j in unique(xturno)){
    for(l in unique(xgrado)){
    estrato = exa_temp[exa_temp$Area== i & exa_temp$cat_turno == j  & exa_temp$Grado ==l &exa_temp$Carrera== "No",]
    estrato$estrato = h
    muestra =estrato[sample(dim(estrato)[1],2,replace=FALSE),]
    muestra_total = rbind(muestra_total,muestra)
    h=h+1
    
    estrato = exa_temp[exa_temp$Area== i & exa_temp$cat_turno == j  & exa_temp$Grado ==l &exa_temp$Carrera == "Si",]
    estrato$estrato = h
    muestra =estrato[sample(dim(estrato)[1],2,replace=FALSE),]
    muestra_total = rbind(muestra_total,muestra)
    h=h+1
    
    
    }
    
  }
  
  
}

muestra_total
write.csv(muestra_total,"muestra_total.csv")
###########################################################################
# the 4 variables were assumed to be random
#to ensure you don't leave any as a block
#including the grade, by way of determining
#your significant influences and interactions
# affecting teachers' grades



experimento = aov(Nota ~ Grado*Area*cat_turno*Carrera, data=muestra_total)
summary(experimento)
"                             Df Sum Sq Mean Sq F value  Pr(>F)
Grado                         5   2022   404.4   4.322 0.00251
Area                          1     50    49.6   0.530 0.47009
cat_turno                     1    207   207.1   2.214 0.14333
Carrera                       1    907   906.5   9.690 0.00312
Grado:Area                    5    476    95.3   1.019 0.41725
Grado:cat_turno               5    190    38.0   0.407 0.84181
Area:cat_turno                1     10    10.0   0.107 0.74500
Grado:Carrera                 5   1003   200.7   2.145 0.07599
Area:Carrera                  1     50    49.6   0.530 0.47009
cat_turno:Carrera             1     19    19.3   0.206 0.65206
Grado:Area:cat_turno          5     21     4.2   0.044 0.99877
Grado:Area:Carrera            5   1109   221.8   2.371 0.05305
Grado:cat_turno:Carrera       5     33     6.7   0.071 0.99622
Area:cat_turno:Carrera        1      0     0.1   0.001 0.97488
Grado:Area:cat_turno:Carrera  5   1336   267.3   2.857 0.02450
Residuals                    48   4490    93.6   "

"significativas:"
"
Variables significativas
                             Df Sum Sq Mean Sq F value  Pr(>F)   
Grado                         5   2022   404.4   4.322 0.00251 **
Carrera                       1    907   906.5   9.690 0.00312 **
Grado:Area:cat_turno:Carrera  5   1336   267.3   2.857 0.02450 *



"
###########################################################################
#################################################################

#suppositions
install.packages("nortest")
library(nortest)
install.packages("lmtest")
library(lmtest)
install.packages('agricolae')
library(agricolae)



anova= experimento

#normal
#complies normality, is not rejected ho
shapiro.test(anova$residuals)
#W = 0.99437, p-value = 0.9618

#independence
#complies independence, is not rejected ho
dwtest(anova)
#DW = 3.2073, p-value = 0.9461

#homocesdasticity
# homoskedasticity assumption is met
library(car)
leveneTest(anova$residuals, muestra_total$Nota)
#Df F value Pr(>F)
#group 38  0.9181 0.6049


################################################################
#ADHOC TEST
TukeyHSD(experimento)$Grado

"4-1 12.8750   2.72583679 23.024163 0.005728765
"

TukeyHSD(experimento)$Carrera

"          diff      lwr     upr       p adj
Si-No 6.145833 2.176164 10.1155 0.003119463"
an=TukeyHSD(experimento)
datos= data.frame(an$`Grado:Area:cat_turno:Carrera`)

names(datos) = c("tipo", "diff","lwr","upr","p.adj")
str(datos)

datos =datos %>% add_rownames()

datos_p = datos %>%
            filter(p.adj < 0.05)

datos_p
# Turned out to be significant:

"6:Urbana:Vespertino_otro:Si-1:Rural:Vespertino_otro:No"

"  rowname                                diff   lwr   upr  p.adj
  <chr>                                 <dbl> <dbl> <dbl>  <dbl>
 6:Urbana:Vespertino_otro:Si-1:Rural:~  41.5 0.440  82.6 0.0443"
#######################################
#interaccion

#interacciones
par(mfrow=c(1,1))
interaction.plot(muestra_total$Area, muestra_total$cat_turno, muestra_total$Nota, col=c("blue","red"))
interaction.plot(muestra_total$Area, muestra_total$Carrera, muestra_total$Nota,col=c("blue","red"))
interaction.plot(muestra_total$Area, muestra_total$Grado, muestra_total$Nota,col=c("blue","red","yellow","black","brown","green"))
interaction.plot(muestra_total$cat_turno, muestra_total$Grado, muestra_total$Nota,col=c("blue","red","yellow","black","brown","green"))
interaction.plot(muestra_total$Carrera, muestra_total$Grado, muestra_total$Nota,col=c("blue","red","yellow","black","brown","green"))
interaction.plot(muestra_total$Carrera, muestra_total$cat_turno, muestra_total$Nota)

###############################################

# Tukey interaction



#tukeyadd
library("asbio")

with(muestra_total,tukey.add.test(Nota,Area,cat_turno))
#F = 0.003289   Denom df = 92    p-value = 0.9543909
#No significant evidence of interaction

with(muestra_total,tukey.add.test(Nota,Area,Carrera))
#F = 0.0173361   Denom df = 92    p-value = 0.8955354
#No significant evidence of interaction

with(muestra_total,tukey.add.test(Nota,Area,Grado))
#F = 0.0010797   Denom df = 88    p-value = 0.9738614
#No significant evidence of interaction

with(muestra_total,tukey.add.test(Nota,cat_turno,Grado))
#F = 0.1533769   Denom df = 88    p-value = 0.6962757
#No significant evidence of interaction

with(muestra_total,tukey.add.test(Nota,Carrera,Grado))
#F = 0.0774232   Denom df = 88    p-value = 0.781473
#No significant evidence of interaction

with(muestra_total,tukey.add.test(Nota,Carrera,cat_turno))
#F = 0.00683   Denom df = 92    p-value = 0.9343141
#No significant evidence of interaction

"As they are paired tests, they cannot be verified
the resulting interaction in ANOVA and tukey that includes
the 4 variables "
#######################################################
#Finally means plot

install.packages("gplots")
library(gplots)
par(mfrow=c(1,1))
plotmeans(muestra_total$Nota ~ muestra_total$Grado,xlab="Grado",
          ylab="Nota", main="Mean Plot\nwith 95% CI")

