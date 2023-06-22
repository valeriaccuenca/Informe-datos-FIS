library(plyr)
library(tidyverse)
library(scales)
library(broom)
library(survey)
library(lubridate)
library(lme4)
library(broom.mixed)
library(glmmTMB)
library(tidyverse)
library(openxlsx)
library(haven)
library(sf)
library(rgdal)
library(broom)
library(summarytools)
library(table1)
library(cowplot)
library(lpSolve)
library(irr)
library(readr)


rm(list=ls())

theme_fis<-  theme(axis.text=element_text(size=10, color="black"),
                   axis.title=element_text(size=10, face="bold", color="black"),
                   strip.text = element_text(size=10, face="bold", color="black"),
                   legend.text=element_text(size=10, color="black"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(color="black", size=10),
                   axis.text.y = element_text(color="black", size=10),
                   legend.position="bottom")

load("Luis_estandarizacion/sedentarismo_prevalencias_informes.RData")
load("Luis_estandarizacion/sedentarismo_rii_informes.RData")
desigualdades_rii_spain <- read.csv("Luis_estandarizacion/rii_spain.csv")
desigualdades_rii_ccaa <- read.csv("Luis_estandarizacion/rii_ccaa.csv")
prevalencias_spain <- read.csv("Luis_estandarizacion/prevalencias_spain.csv")
prevalencias_ccaa <- read.csv("Luis_estandarizacion/prevalencias_ccaa.csv")


#Creamos base de datos conjunta de RII para informes#
#####################################################

#####################################################
#####################################################
#### CAMBIAR "ESPANA" POR "MEDIA NACIONAL" ##########
#####################################################
#####################################################


ccaa_nombres <- sedentarismo_rii %>% 
  select(ccaa, nombre_notilde) %>% 
  filter(ccaa==0)

desigualdades_rii_spain <- desigualdades_rii_spain %>% 
  rename(fr=risk_factor) %>% 
  select(-c(X)) %>% 
  filter(encuesta!="2009-01-01" & fr!="sedentario") %>% 
  mutate(abreviatura="ES",
         nombre_notilde="Espana")

desigualdades_rii_ccaa <- desigualdades_rii_ccaa %>% 
  select(-c(X, id_mapa, nombre)) %>%
  mutate(encuesta = recode(encuesta, "2001-01-01"="2001", "2003-01-01"="2003", "2006-01-01"="2006", "2011-01-01"="2011", "2014-01-01"="2014", "2017-01-01"="2017", "2020-01-01"="2020")) %>% 
  filter(encuesta!="2009-01-01" & fr!="sedentario")

rii <- desigualdades_rii_spain %>% 
  rbind(desigualdades_rii_ccaa) %>% 
  rbind(sedentarismo_rii) %>% 
  filter(encuesta!=2001) %>% 
  filter(encuesta!=2009)

rii$ccaa <- as.factor(rii$ccaa)

save(rii, file = "Luis_estandarizacion/RII_informes.RData")
save(rii, file = "Informes CCAA/RII_informes.RData")

rm(desigualdades_rii_ccaa, desigualdades_rii_spain)

##########################
  
  
prevalencias_spain <- prevalencias_spain %>% 
  select(-c(X)) %>% 
  mutate(sexo=(case_when(sexo==0~"Mujeres", sexo==1~"Hombres", sexo=="Overall"~"Global")),
         abreviatura="ES",
         nombre_notilde="Espana",
         ccaa=0)
  
prevalencias_ccaa <- prevalencias_ccaa %>% 
  select(-c(X, id_mapa, nombre)) %>% 
  mutate(sexo=(case_when(sexo==0~"Mujeres", sexo==1~"Hombres", sexo=="Overall"~"Global")))

prevalencias <- prevalencias_spain %>% 
  rbind(prevalencias_ccaa) %>% 
  rename(Sedentarismo=sedentario,
         sedentarismo_low=sedentario_low,
         sedentarismo_upp=sedentario_upp)

prevalencias$ccaa <- as.factor(prevalencias$ccaa)

prevalencias <- prevalencias %>% 
  filter(encuesta != 2009) %>% 
  filter(encuesta != 2001)

save(prevalencias, file = "Luis_estandarizacion/prevalencias_informes.RData")
save(prevalencias, file = "Informes CCAA/prevalencias_informes.RData")

rm(ccaa_nombres, prevalencias_ccaa, prevalencias_spain, sedentarismo_prevalencias, sedentarismo_rii)

#################
## MERGEO VARIABLES PARA QUE CORRA EL BUCLE
#################

ccaa <- data.frame(abreviatura = c("ES", "AN", "AR", "AS", "IB", "CN", "CB", "CM", "CL", "CT", "VC", "EX", "GA",
                                   "RI", "MD", "MC", "NC", "PV"),
                   nombre_ccaa = c("España", "Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                                   "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                   "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                                   "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco"))

rii <- rii %>% 
left_join(ccaa)

prevalencias <- prevalencias %>% 
  left_join(ccaa)

save(prevalencias, file = "Luis_estandarizacion/prevalencias_informes.RData")
save(prevalencias, file = "Informes CCAA/prevalencias_informes.RData")
save(rii, file = "Luis_estandarizacion/RII_informes.RData")
save(rii, file = "Informes CCAA/RII_informes.RData")


#Código diseño figuras

theme_fis<-  theme(axis.text=element_text(size=10, color="black"),
                   axis.title=element_text(size=10, face="bold", color="black"),
                   strip.text = element_text(size=10, face="bold", color="black"),
                   legend.text=element_text(size=10, color="black"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(color="black", size=10),
                   axis.text.y = element_text(color="black", size=10),
                   legend.position="bottom")

fig_rii <-  
  rii %>% 
  mutate(filtro=case_when((nombre_notilde=="Espana" | nombre_notilde=="Andalucia")~1)) %>%
  filter(fr=="Sedentarismo", filtro==1) %>% 
  ggplot(aes(x=as.numeric(encuesta), y=rii, ymin=rii_infci, ymax=rii_supci), group=ccaa) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=ccaa))+
  facet_grid(cols = vars(sexo))+
  geom_line(aes(color=ccaa)) +
  scale_y_continuous(trans="log",
                     breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        axis.title=element_text(size=10, face="bold", color="black"),
        strip.text = element_text(size=10, face="bold", color="black"),
        legend.text=element_text(size=10, color="black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black", size=10),
        legend.position="bottom")

fig_rii



fig_prevalencias <-  
  prevalencias %>%
  mutate(filtro=case_when((nombre_notilde=="Espana" | nombre_notilde=="Andalucia")~1),
         Nombre="Andalucía") %>%
  filter(filtro==1) %>% 
  ggplot(aes(x=encuesta, y=(diabetes*100), ymin=(diabetes_low*100), ymax=(diabetes_upp*100), group=education_3)) +
  geom_line(aes(color=as.factor(education_3)))+  
  geom_ribbon(alpha=0.3, aes(fill=as.factor(education_3)))+
  facet_grid(rows = vars(sexo),cols = vars(nombre_notilde))+
  scale_y_continuous(breaks = c(20, 40, 60, 100))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="Prevalencia de sedentarismo (95% IC)")+
  scale_fill_discrete(name="Nivel Educativo",
                      labels=c("Bajo", "Medio", "Alto"))+
  scale_color_discrete(guide="none")+
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"),
        axis.title=element_text(size=10, face="bold", color="black"),
        strip.text = element_text(size=10, face="bold", color="black"),
        legend.text=element_text(size=10, color="black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black", size=10),
        legend.position="bottom")+
  labs( title = "Prevalencia de diabetes por nivel educativo y sexo en el período 2003-2020")

fig_prevalencias

##################################################


fig_rii <-  
  rii %>%
  filter(fr=="sedentario", ccaa==0) %>% 
  ggplot(aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci), group=sexo) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  labs(x="", y="RII (95% CI)")+
  scale_y_continuous(trans="log",
                     breaks=c(-1, 0, 1, 1.5, 2, 2.5, 3, 4, 5))+
  ylim(-1, 5.5)+
  theme_bw()+
  theme_fis+
  theme()

fig_rii


##################################################
#Figura para prevalencias

fig_des_sedentario <-  
  sedentarismo_prevalencias %>% 
  filter(ccaa==5, sexo!="Overall") %>% 
  ggplot(aes(x=encuesta, y=(sedentario*100), ymin=(sedentario_low*100), ymax=(sedentario_upp*100))) +
  geom_line(aes(color=as.factor(education_3)))+  
  geom_ribbon(alpha=0.3, aes(fill=as.factor(education_3)))+
  facet_grid(cols = vars(sexo))+
  scale_y_continuous(breaks = c(20, 40, 60, 100))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="Prevalence of sedentarism (95% CI)")+
  scale_color_discrete(guide='none')+
  labs(fill='Education level')+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Prevalence of sedentarism by educational level and sex between 2001-2020")

fig_des_sedentario


#Figura RRI


fig_rii <-  
  rii %>% 
  filter(fr=="Sedentarismo") %>% 
  ggplot(aes(x=encuesta, y=(rii*100), ymin=(sedentario_low*100), ymax=(sedentario_upp*100))) +
  geom_line(aes(color=sexo))+  
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  facet_grid(cols = vars(sexo))+
  scale_y_continuous(breaks = c(20, 40, 60, 100))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="Prevalence of sedentarism (95% CI)")+
  scale_color_discrete(guide='none')+
  labs(fill='Education level')+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Prevalence of sedentarism by educational level and sex between 2001-2020")

fig_rii


fig_rii_luis <-  
  sedentarismo_prevalencias %>% 
  filter(ccaa==0) %>% 
  ggplot(aes(x=encuesta, y=(sedentario*100), ymin=(sedentario_low*100), ymax=(sedentario_upp*100))) +
  geom_line(aes(color=as.factor(education_3)))+  
  geom_ribbon(alpha=0.3, aes(fill=as.factor(education_3)))+
  facet_grid(cols = vars(sexo))+
  scale_y_continuous(breaks = c(20, 40, 60, 100))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="Prevalence of sedentarism (95% CI)")+
  scale_color_discrete(guide='none')+
  labs(fill='Education level')+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Prevalence of sedentarism by educational level and sex between 2001-2020")


fig_rii_luis

plot <- plot_grid(fig_rii_pedro, fig_rii_luis, nrow = 2)

plot

##Problema con análisis de sedentarismo por CCAA:

# -Los análisis de la base del informe duplica sedentarismo en mujeres (por error en labelled de food. Corregido) (1.png).
# -Los análisis de mujeres no convergen si incluimos 2001. Excluimos 2001 (1.png y 2.png).




#Creación bases de datos únicas




