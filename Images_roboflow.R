library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(scales)
library(fs)
library(purrr)


# 12 a 26 de agosto ---------------------------------------------------------------------------------------------------------------------------------------
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

table(table(train.images$FileName) == 2) # Se confirma que no se repitieron imágenes

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


# Base de datos para control de procesamiento

train.images|>
  select(FileName)|>
  mutate(Annotation = NA, fish = NA, turtle = NA)|>
  write_csv(file = "images_ago_2024.csv", na = "")



# 28 agosto - 20 septiembre -------------------------------------------------------------------------------------------------------------------------------
# Importar metadatos
metadata <- read_csv("8ago_20sep_2024_images_metadata.csv")


metadata|>
  filter(File_date >= "2024-08-28")|>
  filter(File_date <= "2024-09-20")|>
  ggplot(aes(x = File_hour, y = bLuma))+
  geom_point(aes(colour = Camera))+
  facet_wrap(~Camera)

# Imágenes para entrenamiento. Solo las diurnas entre 6:00h y 18:00h
todas <- metadata|>
  filter(File_date >= "2024-08-28")|>
  filter(File_date <= "2024-09-20")|>
  filter(File_hour >= 6 & File_hour <= 18)|>
  select(1,3,4)

# total de imágenes
num.images <- metadata|>
  filter(File_date >= "2024-08-27")|>
  filter(File_date <= "2024-09-20")|>
  filter(File_hour >= 6 & File_hour <= 18)|>
  group_by(Directory)|>
  summarise(images = length(Directory))

# selección del 50 % de imágenes
samp.size <- round(0.5*(mean(num.images$images)),0)

train.images <- todas|>
  group_by(Directory)|>
  reframe(FileName = sample(FileName, samp.size))

table(table(train.images$FileName) == 2) # Se confirma que no se repitieron imágenes

write_csv(train.images, file = "8ago_20sep_2024_images_sampled.csv")

# train.images <- read.csv("ago_2024_images_sampled.csv")

# Definir el directorio destino
dest_dir <- "C:/train.images_ago_sep_2024"

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


# Base de datos para control de procesamiento
# Eliminar los primeras 77 registros ya que son imágenes de la cámara dentro del saco
# Eliminar los últimos 178 registros ya que son imágenes de la cámara dentro del saco
# Estas imágenes ya fueron removidas de train.images_ago_seop_2024

list.files(dest_dir)

tibble(
  FileName = list.files(dest_dir),
  Annotation = NA, fish = NA, turtle = NA)|>
  write_csv(file = "images_ago_sep_2024.csv", na = "")


# xx <- train.images|>
#   left_join(metadata)|>
#   select(FileName, File_date, File_hour, File_minute)|>
#   arrange(FileName)|>
#   #arrange(File_date, File_hour, File_minute)|>
#   mutate(Annotation = NA, fish = NA, turtle = NA)
  
  



