######################################

## Este script es para correr todos los informes html de las CCAA

files <- list.files(path = "scripts html", all.files = T)

for (f in 1:length(files)) {
  if (grepl("Rmd", files[f])){
    
  rmarkdown::render(input = paste0("scripts html/", files[f]), 
                    output_dir = "scripts html/informes",
                    output_file = paste0(tools::file_path_sans_ext(basename(files[f])),".html"))
    
  }
}

