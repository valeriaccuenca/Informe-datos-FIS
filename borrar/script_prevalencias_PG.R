library(tidyverse)

dta <- read_csv("prevalencias_ccaa.csv")


dta_asturias <- dta %>%
  filter(sexo=="Overall", ccaa==3) %>%
  mutate(educa=case_when(education_3==1~"Bajo", 
                         education_3==2~"Medio", 
                         education_3==3~"Alto"), 
         educa=factor(educa, levels=c("Bajo", "Medio", "Alto")))

figura_asturias <- 
  ggplot(dta_asturias, aes(x=encuesta, y=diabetes*100, color=educa, 
             ymin=diabetes_low*100, ymax=diabetes_upp*100))+
  geom_point()+
  geom_errorbar(alpha=0.3)+
  geom_line()+
  #scale_x_date(date_labels = "%Y")+
  labs(x="Año", y="Prevalencia de diabetes (IC 95%)")+
  guides(color=guide_legend(title="Nivel educativo"))+
  theme_bw()
figura_asturias
