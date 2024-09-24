library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(scales)
library(fs)
library(purrr)

# Importar metadatos
metadata <- read_csv("12_26_ago_2024_images_metadata.csv")

# Imágenes para entrenamiento. Solo las diurnas entre 6:00h y 18:00h
todas <- metadata|>
  filter(File_date <= "2024-08-26")|>
  filter(File_hour >= 6 & File_hour <= 18)|>
  select(1,3,4)

# total de imágenes
num.images <- metadata|>
  filter(File_date <= "2024-08-26")|>
  filter(File_hour >= 6 & File_hour <= 18)|>
  group_by(Directory)|>
  summarise(images = length(Directory))

# selección del 50 % de imágenes
samp.size <- round(0.5*(mean(num.images$images)),0)

train.images <- todas|>
  group_by(Directory)|>
  reframe(FileName = sample(FileName, samp.size))

write_csv(train.images, file = "ago_2024_images_sampled.csv")

# train.images <- read.csv("ago_2024_images_sampled.csv")

# Definir el directorio destino
dest_dir <- "C:/train.images_ago_2024"

# Crear la carpeta destino si no existe
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir)
}

# Asegúrate de que el DataFrame `train.images` tenga las columnas `Directory` y `FileName`
if (!all(c("Directory", "FileName") %in% colnames(train.images))) {
  stop("El DataFrame 'train.images' debe contener las columnas 'Directory' y 'FileName'")
}

# Seleccionar las imágenes a copiar 

# Recorrer cada fila del DataFrame para copiar las imágenes
for (i in 1:nrow(train.images)) {
  # Construir la ruta completa del archivo fuente
  source_file <- file.path(train.images$Directory[i], train.images$FileName[i])
  
  # Construir la ruta completa del archivo destino
  dest_file <- file.path(dest_dir, train.images$FileName[i])
  
  # Copiar el archivo al directorio destino
  file.copy(source_file, dest_file, overwrite = TRUE)
}

# Mensaje de finalización
cat("Copia de imágenes completada. Todas las imágenes seleccionadas se encuentran en:", dest_dir)
