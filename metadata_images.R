library(exifr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(fs)

# 12 a 26 de agosto ---------------------------------------------------------------------------------------------------------------------------------------
# Definir el directorio principal donde están las carpetas de las cámaras con imágenes renombradas
main_dir <- "C:/rename_IA_fish_12_26_ago_2024"  # Cambia esto por la ruta correcta a las imágenes renombradas

# Obtener la lista de carpetas de cámaras (cada cámara en una carpeta)
cameras <- list.dirs(main_dir, recursive = FALSE)

# Función para extraer metadatos de todas las imágenes de una cámara
extract_metadata <- function(camera_dir) {
  # Obtener el nombre de la cámara a partir del nombre de la carpeta
  camera_name <- basename(camera_dir)
  
  # Listar todas las imágenes en la carpeta de la cámara (sin subcarpetas)
  images <- list.files(camera_dir, full.names = TRUE, pattern = "\\.JPG$|\\.PNG$")
  
  # Leer metadatos de todas las imágenes en la carpeta
  if (length(images) > 0) {
    metadata <- read_exif(images)
    
    # Añadir columna con el nombre de la cámara
    metadata$Camera <- camera_name
    
    return(metadata)
  } else {
    return(data.frame())  # Devolver un DataFrame vacío si no hay imágenes
  }
}

# Extraer metadatos de todas las cámaras y combinarlos en un solo DataFrame
all_cameras_metadata <- bind_rows(lapply(cameras, extract_metadata))

# función para extraer fecha y hora de la imagen con información en el nombre del archivo
image_dttm <- function(x){
  sub = str_sub(x, 1, 19)
  dttm = ymd_hms(str_replace_all(sub, c("-" = ":", "_" = " ")))
  returnValue(dttm)
}
# Función para dividir la columna 'UserComment' en múltiples columnas
parse_user_comment <- function(df) {
  if ("UserComment" %in% colnames(df) && any(!is.na(df$UserComment))) {
    # Dividir la columna UserComment en variables separadas
    # Si hay varias combinaciones de variables en UserComment, necesitamos un método más dinámico
    
    # Crear un DataFrame temporal para almacenar las variables desglosadas
    user_comment_data <- df |>
      mutate(UserComment = strsplit(UserComment, ",")) |>
      unnest(UserComment) |>
      separate_wider_delim(
        UserComment,
        delim = ":",
        names = c("key", "value"),
        too_few = "align_start"
      ) |>
      pivot_wider(
        names_from = key,
        values_from = value,
        values_fn = list(value = ~ paste(unique(.), collapse = ","))
      ) |>
      mutate(Image_dttm = image_dttm(FileName))|>
      mutate(File_hms = format(Image_dttm, "%H:%M:%S"),
             File_hour = hour(Image_dttm),
             File_minute = minute(Image_dttm),
             File_date = date(Image_dttm))|>
      relocate(c(Image_dttm, File_date, File_hms, File_hour, File_minute), .after = FileModifyDate)|>
      select(-c(FileModifyDate, FileAccessDate, FileInodeChangeDate))
    
    # Combinar el DataFrame original con el desglosado
    df <- df |>
      select(-UserComment) |>  # Eliminar la columna original UserComment
      left_join(user_comment_data)
  }
  
  return(df)
}

# Aplicar la función para parsear UserComment
all_cameras_metadata <- parse_user_comment(all_cameras_metadata)

# Guardar el DataFrame como archivo CSV para referencia
write_csv(x = all_cameras_metadata, 
          file = "12_26_ago_2024_images_metadata.csv")


# 28 agosto - 20 septiembre -------------------------------------------------------------------------------------------------------------------------------

# Definir el directorio principal donde están las carpetas de las cámaras con imágenes renombradas
main_dir <- "C:/rename_IA_fish_28go_20sep_2024"  # Cambia esto por la ruta correcta a las imágenes renombradas

# Obtener la lista de carpetas de cámaras (cada cámara en una carpeta)
cameras <- list.dirs(main_dir, recursive = FALSE)

# Función para extraer metadatos de todas las imágenes de una cámara
extract_metadata <- function(camera_dir) {
  # Obtener el nombre de la cámara a partir del nombre de la carpeta
  camera_name <- basename(camera_dir)
  
  # Listar todas las imágenes en la carpeta de la cámara (sin subcarpetas)
  images <- list.files(camera_dir, full.names = TRUE, pattern = "\\.JPG$|\\.PNG$")
  
  # Leer metadatos de todas las imágenes en la carpeta
  if (length(images) > 0) {
    metadata <- read_exif(images)
    
    # Añadir columna con el nombre de la cámara
    metadata$Camera <- camera_name
    
    return(metadata)
  } else {
    return(data.frame())  # Devolver un DataFrame vacío si no hay imágenes
  }
}

# Extraer metadatos de todas las cámaras y combinarlos en un solo DataFrame
all_cameras_metadata <- bind_rows(lapply(cameras, extract_metadata))

# función para extraer fecha y hora de la imagen con información en el nombre del archivo
image_dttm <- function(x){
  sub = str_sub(x, 1, 19)
  dttm = ymd_hms(str_replace_all(sub, c("-" = ":", "_" = " ")))
  returnValue(dttm)
}
# Función para dividir la columna 'UserComment' en múltiples columnas
parse_user_comment <- function(df) {
  if ("UserComment" %in% colnames(df) && any(!is.na(df$UserComment))) {
    # Dividir la columna UserComment en variables separadas
    # Si hay varias combinaciones de variables en UserComment, necesitamos un método más dinámico
    
    # Crear un DataFrame temporal para almacenar las variables desglosadas
    user_comment_data <- df |>
      mutate(UserComment = strsplit(UserComment, ",")) |>
      unnest(UserComment) |>
      separate_wider_delim(
        UserComment,
        delim = ":",
        names = c("key", "value"),
        too_few = "align_start"
      ) |>
      pivot_wider(
        names_from = key,
        values_from = value,
        values_fn = list(value = ~ paste(unique(.), collapse = ","))
      ) |>
      mutate(Image_dttm = image_dttm(FileName))|>
      mutate(File_hms = format(Image_dttm, "%H:%M:%S"),
             File_hour = hour(Image_dttm),
             File_minute = minute(Image_dttm),
             File_date = date(Image_dttm))|>
      relocate(c(Image_dttm, File_date, File_hms, File_hour, File_minute), .after = FileModifyDate)|>
      select(-c(FileModifyDate, FileAccessDate, FileInodeChangeDate))
    
    # Combinar el DataFrame original con el desglosado
    df <- df |>
      select(-UserComment) |>  # Eliminar la columna original UserComment
      left_join(user_comment_data)
  }
  
  return(df)
}

# Aplicar la función para parsear UserComment
all_cameras_metadata <- parse_user_comment(all_cameras_metadata)

# Guardar el DataFrame como archivo CSV para referencia
write_csv(x = all_cameras_metadata, 
          file = "8ago_20sep_2024_images_metadata.csv")

