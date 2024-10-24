
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

metadata|>
  filter(File_date <= "2024-08-27")|>
  ggplot(aes(x = Image_dttm, y = bLuma, colour))+
  geom_line(aes(colour = Camera))+
  facet_wrap(~Camera)

metadata|>
  filter(File_date <= "2024-08-27")|>
  ggplot(aes(x = File_hour, y = bLuma))+
  geom_point(aes(colour = Camera))+
  facet_wrap(~Camera)

metadata|>
  filter(File_date <= "2024-08-27")|>
  filter(bLuma >= 250)|>
  ggplot(aes(x = Image_dttm, y = bLuma))+
  geom_point(aes(colour = Camera))+
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


