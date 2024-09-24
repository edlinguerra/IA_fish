
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

metadata|>
  filter(File_date <= "2024-08-27")|>
  ggplot(aes(x = Image_dttm, y = temp))+
  geom_line(aes(colour = Camera))+
  facet_wrap(~Camera)

datebreaks <- seq(min(metadata$File_date), max(metadata$File_date), by = "1 day")

metadata|>
  filter(File_date <= "2024-08-26")|>
  #filter(File_hour >= 6 & File_hour <= 18)|>
  ggplot(aes(x = Image_dttm, y = temp))+
  geom_line(aes(colour = Camera))+
  geom_point()+
  facet_wrap(~Camera)+
  scale_x_datetime(breaks = "1 day", date_labels = "%d %b")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#scale_x_datetime(date_breaks = "1 hour")

# Imágenes para entrenamiento. Solo las dirunas entre 6:00h am y 18:00h
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

# Copiar imágenes
