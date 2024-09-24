install.packages("exifr")
library(exifr)


# Definir el directorio principal donde están las carpetas de las cámaras
main_dir <- "C:/IA_fish_12_26_ago_2024"  # Cambiar esto por la ruta correcta a las imágenes originales

# Definir el directorio donde se almacenarán las imágenes renombradas
output_dir <- "C:/rename_IA_fish_12_26_ago_2024"  # Cambia esto por la ruta a las imágenes renombradas

# Obtener la lista de carpetas de cámaras (cada cámara en una carpeta)
cameras <- list.dirs(main_dir, recursive = FALSE)

# Función para copiar y renombrar archivos sin modificar los originales
rename_images_with_date <- function(camera_dir) {
  # Obtener el nombre de la cámara a partir del nombre de la carpeta
  camera_name <- basename(camera_dir)
  
  # Crear una carpeta en el directorio de salida para esta cámara
  camera_output_dir <- file.path(output_dir, camera_name)
  if (!dir.exists(camera_output_dir)) {
    dir.create(camera_output_dir)
  }
  
  # Obtener todas las subcarpetas de cada cámara
  subfolders <- list.dirs(camera_dir, recursive = FALSE)
  
  for (subfolder in subfolders) {
    # Listar todas las imágenes en la subcarpeta
    images <- list.files(subfolder, full.names = TRUE)
    
    for (image in images) {
      # Leer los metadatos EXIF de la imagen
      exif_data <- read_exif(image)
      
      # Extraer la fecha de la imagen si está disponible
      if (!is.null(exif_data$FileModifyDate[1])) {
        date_taken <- exif_data$FileModifyDate[1]
        # Limpiar el formato de la fecha para que sea adecuado para un nombre de archivo
        date_taken <- gsub(":", "-", date_taken)
        date_taken <- gsub(" ", "_", date_taken)
      } else {
        # Si no se encuentra la fecha, se utiliza una marca de tiempo genérica
        date_taken <- "unknown_date"
      }
      
      # Obtener el nombre original del archivo
      image_name <- basename(image)
      
      # Crear el nuevo nombre incorporando la fecha, el nombre de la cámara y la subcarpeta
      new_name <- paste(date_taken, camera_name, basename(subfolder), image_name, sep = "_")
      
      # Crear la ruta completa del nuevo archivo en el directorio de salida
      new_path <- file.path(camera_output_dir, new_name)
      
      # Copiar el archivo original al nuevo directorio con el nuevo nombre
      file.copy(image, new_path)
    }
  }
}

# Aplicar la función a cada carpeta de cámara
for (camera in cameras) {
  rename_images_with_date(camera)
}
