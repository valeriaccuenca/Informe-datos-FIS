library(tidyverse)
library(readxl)
library(ggplot2)

###Cargamos las dos bases de datos

rm(list=ls())


setwd("C:/Users/luis_/OneDrive - Universidad de Alcala/Investigacion/FIS_CVDinequities/R/Informe-datos-FIS/Informes CCAA")

load("RII_informes.RData")
load("prevalencias_informes.RData")



## Creamos un objeto con las CCAA con los nombres 

nombre_ccaa <- c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                 "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco")



## Corremos el script de rmarkdown que contiene el bucle para que salgan los informes en html

for (v in 1:length(nombre_ccaa)) {
  
  print(paste0("Informe ", nombre_ccaa[v]))
  rmarkdown::render(input = paste0("informe_madre.Rmd"),
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