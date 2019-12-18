library(foreign)
#Read the  census microdata more than 5 Millions of rows
poblation <- read.dta("BaseTotalPer.dta",convert.dates = T,convert.factors = F)
str(poblation)

#Show the variables
library(tibble)
glimpse(poblation) #alternativ to str

view(poblation)
A=data.frame(head(poblation[1:10,]))

library(tidyverse)
#select  the variables that you need, in this case is munic code, Age, Sex
Studysection = poblation %>%
  group_by(munic) %>%
  select(munic,p02,p03a,p09)

#saving
write.csv(Studysection,"investigacion_demo2.csv")

##############################################
#FInding population by munic code, Age, Sex
Studysection <- read.csv("investigacion_demo2.csv")
Studysection = data.frame(Studysection$munic,Studysection$p02, Studysection$p03a)
vivos=NULL

vivos = Studysection %>%
  group_by(Studysection$Studysection.munic,Studysection$Studysection.p02, Studysection$Studysection.p03a) %>%
  count(n())
Studysection=NULL

write.csv(vivos, "vivos.csv")
#Finding deaths by munic code, Age, Sex
library(foreign)
#reading microdata census of mortality section
poblation <- read.dta("BaseTotalMorta.dta",convert.dates = T,convert.factors = F)
library(tibble)
glimpse(poblation) #alternati to str

#selecting munic, age, sex
Studysection = poblation %>%
  group_by(munic) %>%
  select(munic,d01,d02)

#saving
write.csv(Studysection,"investigacion_demo2_muertos.csv")

poblation=NULL
#validating NA cases
Studysection <- Studysection[!is.na(Studysection$d01),]
#counting
table(Studysection$munic,Studysection$d01, Studysection$d02)
#new dataframe
Studysection = data.frame(Studysection$munic,Studysection$d01, Studysection$d02)
muerto=NULL

#counting death number by munix, age, sex
muertos = Studysection %>%
  group_by(Studysection$Studysection.munic,Studysection$Studysection.d01, Studysection$Studysection.d02) %>%
  count(n())
#saving
write.csv(muertos,"muertos.csv")

#rename cols life people
colnames(vivos)[colnames(vivos) == "n()"] <- "vivos_count"
colnames(vivos)[colnames(vivos) == "n"] <- "vivos_coun_auxiliar"
colnames(vivos)[colnames(vivos) == "Studysection$Studysection.p02"] <- "sexo"
colnames(vivos)[colnames(vivos) == "Studysection$Studysection.p03a"] <- "edad"

#rename cols death people
colnames(muertos)[colnames(muertos) == "n()"] <- "muertos_count"
colnames(muertos)[colnames(muertos) == "n"] <- "muertos_coun_auxiliar"
colnames(muertos)[colnames(muertos) == "Studysection$Studysection.d02"] <- "sexo"
colnames(muertos)[colnames(muertos) == "Studysection$Studysection.d01"] <- "edad"

#joining
vivos_muertos=NULL
vivos_muertos = vivos %>%
  left_join(muertos)

#saving
write.csv(vivos_muertos,"vivos_muertos.csv")

muertos=NULL
poblation=NULL
Studysection=NULL
vivos=NULL

#finding mortality life table by munic, sex

#Munic man age
vivos_muertos=data.frame(vivos_muertos)

vivos_muertos_hombres = vivos_muertos %>%
  dplyr::filter(vivos_muertos$sexo == 1)

write.csv(vivos_muertos_hombres,"vivos_muertos_hombres.csv")

#Munic woman age
vivos_muertos_mujeres = vivos_muertos %>%
  dplyr::filter(vivos_muertos$sexo == 2)
write.csv(vivos_muertos_mujeres,"vivos_muertos_mujeres.csv")


municipios = vivos_muertos$Studysection.Studysection.munic
municipios = data.frame(municipios)
municipios_list= municipios %>%
  group_by(municipios) %>%
  count()

write.csv(municipios_list,"Municipios_list.csv")
mun
#expecting life, for man, repeat the same procedure for woman
#NA issue
#TODO: Adjust for ages wihtout mortality cases
vivos_muertos_hombres = vivos_muertos_hombres  %>%
  mutate_all(funs(replace(., is.na(.), 0)))

#saving 
write.csv(vivos_muertos_hombres,"vivos_muertos_hombres.csv")

municipios_list[1,1]
municipios_list = data.frame(municipios_list)
vivos_muertos_hombres =data.frame(vivos_muertos_hombres)


data11 =  vivos_muertos_hombres %>%
  filter(vivos_muertos_hombres$Studysection.Studysection.munic == municipios_list[10,1])


library(tidyverse)
data11=data.frame(data11)
dat11a = data11 %>%
  mutate(mdx = data11$muertos_count/ data11$vivos_count ) %>%
  mutate(qx = (2*mdx) / (2 + mdx)) %>% # death probability
  mutate(px=1-qx)  #survival probability

dat11a$lx=0
dat11a$lx[1]=100000  #hipotetical cohort
for(i in 2:nrow(dat11a)){
  #lx 
  dat11a$lx[i] = dat11a$px[i-1]*dat11a$lx[i-1]
}

dat11a = dat11a %>%
  mutate(Lx= (0.5)*(lx + lead(lx)) )   %>%  #Age persons lifed LX 
  mutate(Lx=  if_else(is.na(Lx),lx,Lx))%>%
  mutate(Tx= rev(cumsum(rev(Lx)))) %>% # Age for to live
  mutate(ex= Tx/lx)# %>% # Life expectancy
#  mutate(exsum= ex + x) %>%   #  Life expectancy other alternative
# mutate(hx = (log(SupPcohortes) -log(lead(SupPcohortes) ))/n) #Hazard ratio if you need it
