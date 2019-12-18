"
***************************************************************************
transversal_into_cohorttable.R
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
"
death= read.csv("BaseTotalMorta.csv")
#Adaptation to R from  stata file proper of Economist Manuel Chavarria 
#Using transversal deaths cenus people into cohort method table 
library(tidyverse)
deathH=death  %>%
  stats::filter(death$d02=="Hombre")
  dplyr::select(death$d02)


str(death$d02)
nrow(deathH)
nrow(death)
#Filtering deaths
death2=filter(death,death$d01 !="Sin informaci\xf3n")
totaln=nrow(death)

#factors conversion
death2$d01 <- factor(death2$d01)
death2=death2[order(death2$d01),]
death2$d01 <- as.numeric(as.character(death2$d01))
death2$d01

#Find the age groups

death3 = death2 %>%
  mutate(getareo= case_when(death2$d01 >= 0 & death2$d01 <= 4  ~ 1,death2$d01 >= 5 & death2$d01 <= 9  ~ 2,
                        death2$d01 >= 10 & death2$d01 <=14   ~ 3,death2$d01 >= 15 & death2$d01 <= 19  ~ 4,
                        death2$d01 >= 20 & death2$d01 <= 24  ~ 5,death2$d01 >= 25 & death2$d01 <= 29  ~ 6,
                        death2$d01 >= 30 & death2$d01 <= 34  ~ 7,death2$d01 >= 35 & death2$d01 <= 39  ~ 8,
                        death2$d01 >= 40 & death2$d01 <= 44  ~ 9,death2$d01 >= 45 & death2$d01 <= 49  ~ 10,
                        death2$d01 >= 50 & death2$d01 <= 54  ~ 11,death2$d01 >= 55 & death2$d01 <= 59  ~ 12,
                        death2$d01 >= 60 & death2$d01 <= 64  ~ 13,death2$d01 >= 65 & death2$d01 <= 69  ~ 14,
                        death2$d01>= 70 & death2$d01 <= 74  ~ 15,death2$d01 >= 75 & death2$d01 <= 79  ~ 16,
                        death2$d01 >= 80 & death2$d01 <= 84  ~ 17,death2$d01 >= 85 & death2$d01 <= 89  ~ 18,
                        death2$d01 >= 90 & death2$d01<= 94  ~ 19,death2$d01 >= 95 & death2$d01 <= 99  ~ 20,
                        death2$d01 >= 100   ~ 21
  )) %>% 
  #order
  arrange((getareo)) %>%
  group_by((getareo)) %>%
  mutate(Freq=sum(getareo)/getareo)  %>%
  mutate(percent = Freq/totaln) %>%
  mutate(acum = percent/Freq) %>%
  #Acummulate
  ungroup() %>%
  mutate(acum2 = cumsum(c(acum[1],acum[-1]))) %>%
  #the las group age
  group_by(getareo) %>%
  mutate(acum3 = last(acum2)) %>%
  mutate(total = totaln) %>%
  mutate(total2 = totaln -Freq) %>%
  mutate(X = (getareo*5) - 5) %>%
  mutate(N = 5) 
  
death3  
tablaBase=death3
tablaBase$lx=totaln

#lx
for(i in 2:nrow(tablaBase)){
  if(tablaBase$getareo[i]==tablaBase$getareo[i-1]){
    tablaBase$lx[i] = tablaBase$lx[i-1]
  }
  else{
    
    tablaBase$lx[i] = tablaBase$lx[i-1]-tablaBase$Freq[i-1]
  }
}

Freqfinal=tablaBase$Freq[nrow(tablaBase)]
#rx
tablaBase$rx=100000
tablaBase$hx=0
#hx
for(i in 1:20){
  tablaBase[tablaBase$getareo==(i),]$hx= (log(mean(tablaBase[tablaBase$getareo==(i+1),]$lx)/tablaBase[tablaBase$getareo==2,]$Freq) - log(mean(tablaBase[tablaBase$getareo==1,]$lx)/tablaBase[tablaBase$getareo==(i),]$Freq)      )/5
}

tablaBase[tablaBase$getareo==(i),]$hx
tablaBaseet1=tablaBase[tablaBase$getareo==1,]
tablaBaseet2=tablaBase[tablaBase$getareo==2,]
nrow(tablaBaseet1)


for(i in 2:nrow(tablaBase)){
  if(tablaBase$getareo[i]==tablaBase$getareo[i-1]){
    tablaBase$rx[i] = tablaBase$rx[i-1]
  }
  else{  
     
    tablaBase$hx[i] = log(tablaBase$lx[i]-(tablaBase$percent[i-1]*tablaBase$rx[i-1]))
  }
}


#finding : nqx,ndx,nax,nLx.nmx
tablaBase2 = tablaBase %>%
   # group_by(getareo) %>%
  ungroup() %>%
  mutate(nnnqx= (lead(lx))) %>%
  mutate(nnnqx=replace_na(nnnqx,1)) %>%
  mutate(nnqx=1 - (lead(lx)/lx)) %>% 
  mutate(nnqx=replace_na(nnqx,1)) %>%
  mutate(nndx=lx - (lead(lx))) %>%
  mutate(nndx=replace_na(nndx,Freqfinal)) %>%
  mutate(nnLx=N*(lx+lead(lx))/2) %>%
  mutate(nnLx=replace_na(nnLx,nnLx[nrow(tablaBase)-2]/2)) %>%
  
  group_by(getareo) %>%
    mutate(nqx = max(nnqx)) %>%
    mutate(ndx=max(nndx)) %>%
    mutate(nax= N/2) %>%
    mutate(nLx = min(nnLx)) %>%
    mutate(nmx = (ndx/nLx)) %>%
  ungroup() %>%
  mutate(Txn = rev(cumsum(rev(nLx/Freq))))  %>%
  group_by(getareo) %>%
    mutate(Tx = max(Txn))  %>%
    mutate(ex= Tx/lx)  %>%
    mutate(exx= ex + X)  %>%
  ungroup() %>%
    mutate(promxx= mean(exx))  %>%
  group_by(getareo) %>%
  mutate()
  summarise(getare_s=mean(getareo),Freq_s=mean(Freq),percent_s=mean(percent),acum_s=mean(acum),acum2_s=mean(acum2),acum3_s=mean(acum3),X_s=mean(X),N_s=mean(N),lx_s=mean(lx),rx_s=mean(rx),nqx_s=mean(nqx),nndx_s=mean(nndx),ndx_s=mean(ndx),nax_s=mean(nax),nLx_s=mean(nLx),nmx_s=mean(nmx),Tx_s=mean(Tx),ex_s=mean(ex),exx_s=mean(exx),promxx_s=mean(promxx))
#saving  
tablaBase2
write.csv(death3, file="morta.csv")







  