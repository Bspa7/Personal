# Configuración inicial
#main_root <- "D:/Steban Pineda/OneDrive/Escritorio/DIME/Transportation and health/data/Shapefiles/Bogota"

setwd("D:/Steban Pineda/OneDrive/Escritorio/DIME/Transportation and health/data/Shapefiles/Bogota")

# Cargar bibliotecas
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(marmap)

# Cargar shapefiles
base_map   <- st_read("Mgn_Bogota/11_BOGOTA/MGN/MGN_URB_SECTOR.shp")
troncal_routes <- st_read("Transporte/Troncal_transmilenio_corredor/Rutas_Troncales_de_TRANSMILENIO.shp")
troncal_stations <- st_read("Transporte/Troncal_transmilenio_estaciones/Estaciones_Troncales_de_TRANSMILENIO.shp")
zonal_routes <- st_read("Transporte/Zonal_SITP_corredor/Rutas_Zonales_SITP.shp")

Entidades_prestadoras_salud        <- st_read("Salud_y_proteccion_social/Entidades_prestadoras_salud/shape/eps.shp")
Institucion_presadora_salud        <- st_read("Salud_y_proteccion_social/Institucion_presadora_salud/ips/ips.shp")
Instituciones_prestadoras_de_salud <- st_read("Salud_y_proteccion_social/Instituciones_prestadoras_de_salud/ips/ips.shp")
Red_adscrita_salud_Bogota          <- st_read("Salud_y_proteccion_social/Red adscrita de salud para Bogota D.C/RASA.shp")
#Red_adscrita_salud                 <- st_read("Salud_y_proteccion_social/Red_adscrita_salud/RASA.shp")

base_map1 <- base_map  %>% 
  mutate(centroid = st_centroid(geometry))

# Crear una nueva variable para la latitud
base_map1 <- base_map1 %>%
  mutate(latitud = st_coordinates(centroid)[, "Y"])

# Crear una nueva variable para la longitud
base_map1 <- base_map1 %>%
  mutate(longitud = st_coordinates(centroid)[, "X"])

base_map1 <- base_map1 %>% 
  filter(latitud> 4.4)


# Crear una nueva variable para la latitud
Red_adscrita_salud_Bogota <- Red_adscrita_salud_Bogota  %>% 
  mutate(centroid = st_centroid(geometry))

Red_adscrita_salud_Bogota <- Red_adscrita_salud_Bogota %>%
  mutate(latitud = st_coordinates(centroid)[, "Y"])

# Crear una nueva variable para la longitud
Red_adscrita_salud_Bogota <- Red_adscrita_salud_Bogota %>%
  mutate(longitud = st_coordinates(centroid)[, "X"])

Red_adscrita_salud_Bogota <- Red_adscrita_salud_Bogota %>% 
  filter(latitud> 4.4)


Instituciones_prestadoras_de_salud <- Instituciones_prestadoras_de_salud  %>% 
  mutate(centroid = st_centroid(geometry))

# Crear una nueva variable para la latitud
Instituciones_prestadoras_de_salud <- Instituciones_prestadoras_de_salud %>%
  mutate(latitud = st_coordinates(centroid)[, "Y"])

# Crear una nueva variable para la longitud
Instituciones_prestadoras_de_salud <- Instituciones_prestadoras_de_salud %>%
  mutate(longitud = st_coordinates(centroid)[, "X"])

Instituciones_prestadoras_de_salud <- Instituciones_prestadoras_de_salud %>% 
  filter(latitud> 4.4)


# Corregir errores topológicos en las geometrías
troncal_routes <- st_make_valid(troncal_routes)

# Verificar las geometrías corregidas
plot(troncal_routes)


## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL --------------------------------------
mapa_line <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2"),
                     labels = c("TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1),
                    labels = c("TM")) 

mapa_line <- mapa_line +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

ggsave(plot=mapa_line  , "Map_1.png",
       width = 7, height = 8, units = "in", dpi = 300)




## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL + ESTACIONES -------------------------

mapa_stations <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.5) +
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2", "Stations" = "firebrick4"),
                     labels = c("Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 1),
                    labels = c("TM", "Stations")) 

mapa_stations <- mapa_stations +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

ggsave(plot=mapa_stations, "Map_2.png",
       width = 7, height = 8, units = "in", dpi = 300)


## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL + HOSPITALES -------------------------

mapa_hospital <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.5) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), shape = "H", size = 3) + # Cambiar el símbolo aquí
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2", "Hospital" = "blue", "Stations" = "firebrick4"),
                     labels = c("Hospital" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 1, 1),
                    labels = c("Hospital", "TM", "Stations")) 

mapa_hospital <- mapa_hospital +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

ggsave(plot=mapa_hospital, "Map_3.png",
       width = 7, height = 8, units = "in", dpi = 300)


## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL + IPS --------------------------------
mapa_ips <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.5) +
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2", "IPS" = "royalblue2", "Stations" = "firebrick4"),
                     labels = c("IPS" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 1, 1),
                    labels = c("IPS", "TM", "Stations")) 

mapa_ips <- mapa_ips +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

#mapa_ips
ggsave(plot=mapa_ips, "Map_4.png",
       width = 7, height = 8, units = "in", dpi = 300)



## MAPA DEL SISTEMA DE TRANSPORTE ZONAL + EL HOSPITALES EN BOGOTA --------------

mapa_hospital_sitp <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = zonal_routes, aes(color = "SITP"), size = 1) +  
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), shape = "H", size = 3) + # Cambiar el símbolo aquí
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("SITP" = "darkseagreen", "Hospital" = "blue"),
                     labels = c("Hospital", "SITP")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 1, 1),
                    labels = c("Hospital", "SITP")) 

mapa_hospital_sitp <- mapa_hospital_sitp +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
#mapa_hospital_sitp

ggsave(plot=mapa_hospital_sitp, "SITP_Map_1.png",
       width = 7, height = 8, units = "in", dpi = 300)


## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL + IPS --------------------------------
mapa_ips_sitp <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray65", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +  
  geom_sf(data = zonal_routes, aes(color = "SITP"), size = 1) +  
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("SITP" = "darkseagreen", "IPS" = "blue"),
                     labels = c("IPS", "SITP")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 1, 1),
                    labels = c("IPS", "SITP")) 

mapa_ips_sitp <- mapa_ips_sitp +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
#mapa_ips_sitp

ggsave(plot=mapa_ips_sitp, "SITP_Map_2.png",
       width = 7, height = 8, units = "in", dpi = 300)










































































install.packages("ggrepel")
install.packages("marmap")

library(tidyverse)
library(ggrepel)
library(sf)
library(marmap)

# Add scale and North arrow
mapa_hospital_sitp +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )





























mapa_line <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray75", size = 0.8) +
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2"),
                     labels = c("TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1),
                    labels = c("TM")) 
mapa_line


Grafico1 = mapa_line + 
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
Grafico1





## MAPA DEL SISTEMA DE TRANSPORTE TRONCAL + EL DE SALUD EN BOGOTA
mapa_troncal <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray75", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.2) +  
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "orange", "IPS" = "green", "Hospital" = "blue", "Stations" = "red"),
                     labels = c("Hospital","IPS" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 0.8, 1.4, 1.2),
                    labels = c("IPS", "Hospital", "TM", "Stations")) 
mapa_troncal

# COLORES PARA DALTONICOS AQUI -------------------------------------------------
mapa_troncal <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray75", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.5) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), shape = "H", size = 3) + # Cambiar el símbolo aquí
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "lightsalmon2", "IPS" = "darkseagreen", "Hospital" = "blue", "Stations" = "firebrick4"),
                     labels = c("Hospital","IPS" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 0.8, 1.4, 1.2),
                    labels = c("IPS", "Hospital", "TM", "Stations")) 

mapa_troncal

# ------------------------------------------------------------------------------










mapa_troncal <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray75", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.2) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), shape = "H", size = 3) + # Cambiar el símbolo aquí
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "orange", "IPS" = "green", "Hospital" = "blue", "Stations" = "red"),
                     labels = c("Hospital","IPS" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 0.8, 1.4, 1.2),
                    labels = c("IPS", "Hospital", "TM", "Stations")) 

mapa_troncal


mapa_troncal <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray75", size = 0.8) +
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), size = 0.8) +  
  geom_sf(data = troncal_routes, aes(color = "TM"), size = 1) +  
  geom_sf(data = troncal_stations, aes(color = "Stations"), size = 1.2) +
  geom_sf(data = Red_adscrita_salud_Bogota, aes(color = "Hospital"), shape = "H", size = 3) + # Cambiar el símbolo aquí
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("TM" = "orange", "IPS" = "green", "Hospital" = "blue", "Stations" = "red"),
                     labels = c("Hospital","IPS" , "Stations", "TM")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 0.8, 1.4, 1.2),
                    labels = c("IPS", "Hospital", "TM", "Stations")) 

mapa_troncal






#ggsave(plot=mapa_troncal, "Transmilenio_map.png",
  #       width = 7, height = 8, units = "in", dpi = 300)






## MAPA DEL SISTEMA DE TRANSPORTE ZONAL + EL DE SALUD EN BOGOTA
mapa_zonal <- ggplot() +
  geom_sf(data = base_map1, fill = "white", color = "gray85", size = 0.8) +
  geom_sf(data = zonal_routes, aes(color = "SITP"), size = 1) +  
  geom_sf(data = Instituciones_prestadoras_de_salud, aes(color = "IPS"), size = 0.8) +    
  labs(title = "") +
  theme_minimal() +
  coord_sf(ylim = c(4.45, 4.85)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(name = "Legend Title",
                     values = c("SITP" = "#87CEFA", "IPS" = "green"),
                     labels = c("IPS", "SITP")) +
  scale_size_manual(name = "Legend Title",
                    values = c(1, 0.8, 1.2),
                    labels = c("IPS", "SITP")) 
#mapa_zonal

#ggsave(plot=mapa_zonal, "SITP_map.png",
 #      width = 7, height = 8, units = "in", dpi = 300)







