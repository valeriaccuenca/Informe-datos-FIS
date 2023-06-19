######################################

## Script para sacar los informes de las comunidades autónomas en html 
## y para sacar un ppt con las figuras de cada ccaa

## Cargamos librerías

library(tidyverse)
library(readxl)
library(ggplot2)
library(egg)

## Primero, vamos a cargar las bases de datos que necesitamos

prevalencias_ccaa <- read.csv("prevalencias_ccaa.csv")
prevalencias_spain <- read.csv("prevalencias_spain.csv")
rii_ccaa <- read.csv("rii_ccaa.csv")
rii_spain <- read.csv("rii_spain.csv")

## Creamos un objeto con las CCAA con los nombres como la variable abreviatura y añadimos el nombre oficial del INE
## para hacer merge (usaremos el nombre oficial para el bucle de informes)

ccaa <- data.frame(abreviatura = c("AN", "AR", "AS", "IB", "CN", "CB", "CM", "CL", "CT", "VC", "EX", "GA",
                                   "RI", "MD", "MC", "NC", "PV"),
                   nombre_ccaa = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                                   "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                   "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                                   "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco"))

prevalencias_ccaa <- left_join(prevalencias_ccaa, ccaa, by = "abreviatura")
prevalencias_spain <- left_join(prevalencias_ccaa, ccaa, by = "abreviatura")
rii_ccaa <- left_join(rii_ccaa, ccaa, by = "abreviatura")
rii_spain <- left_join(rii_ccaa, ccaa, by = "abreviatura")

nombre_ccaa <- c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                               "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                               "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                               "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco")

## Preparamos las bases para correr el código de los informes

prevalencias_ccaa <- prevalencias_ccaa %>%
  mutate(educa=case_when(education_3==1~"Bajo", 
                         education_3==2~"Medio", 
                         education_3==3~"Alto"), 
         educa=factor(educa, levels=c("Bajo", "Medio", "Alto"))) %>%
  mutate(sexo=case_when(sexo=="0"~"Hombre",
                        sexo=="1"~"Mujeres",
                        sexo=="Overall"~"Overall"),
         sexo=factor(sexo, levels=c("Overall", "Hombre", "Mujeres")))

prevalencias_spain <- prevalencias_spain %>%
  mutate(educa=case_when(education_3==1~"Bajo", 
                         education_3==2~"Medio", 
                         education_3==3~"Alto"), 
         educa=factor(educa, levels=c("Bajo", "Medio", "Alto"))) %>%
  mutate(sexo=case_when(sexo=="0"~"Hombre",
                        sexo=="1"~"Mujeres",
                        sexo=="Overall"~"Overall"),
         sexo=factor(sexo, levels=c("Overall", "Hombre", "Mujeres")))

## Corremos el script de rmarkdown que contiene el bucle para que salgan los informes en html

for (v in 1:length(nombre_ccaa)) {

print(paste0("Informe ", nombre_ccaa[v]))
rmarkdown::render(input = paste0("informes_ccaa_html.Rmd"),
                  output_dir = "Informes CCAA",
                  output_file = paste0("Informe_", nombre_ccaa[v], ".html"))
  
}



files <- list.files(path = "scripts html", all.files = T)

for (f in 1:length(files)) {
  if (grepl("Rmd", files[f])){
    
  rmarkdown::render(input = paste0("scripts html/", files[f]), 
                    output_dir = "scripts html/informes",
                    output_file = paste0(tools::file_path_sans_ext(basename(files[f])),".html"))
    
  }
}

