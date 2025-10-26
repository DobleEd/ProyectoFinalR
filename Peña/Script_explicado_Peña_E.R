#PAQUETES NECESARIOS
library(readxl)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(writexl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(ggspatial)
#Peña López Edgar Eduardo
#25/10/2025
#Analisis de componentes principales y mapeo a partir de variable acústica

archivo_excel <- "tablas_apiladas.xlsx" #Cargamos el archivo/base de datos
datos <- read_excel(archivo_excel, sheet = 1) #Leemos la primera hoja
datos_filtrados <- datos %>%
  filter(Nota != "Completo") #Filtramos los datos donde la variable Nota se encuentra numerada

columnas_numericas <- datos_filtrados %>%
  dplyr::select(where(is.numeric)) %>%
  names() #Separamos las columnas númericas

columnas_categoricas <- datos_filtrados %>%
  dplyr::select(where(is.character)) %>%
  names() #Separamos las columnas categóricas

promedios_audio <- datos_filtrados %>%
  mutate(across(c( "Low_Freq", "High_Freq", "Max_Freq", "Max_Amp", "Peak_Freq", "Delta_Time", "Min_Amp", "Peak_Time", "Length_frames", "Delta_Freq", "Avg_Power_Density", "Nota"), as.numeric)) %>%
  group_by(Audio) %>%
  summarise(across(c( "Low_Freq", "High_Freq", "Max_Freq", "Max_Amp", "Peak_Freq", "Delta_Time", "Min_Amp", "Peak_Time", "Length_frames", "Delta_Freq", "Avg_Power_Density", "Nota"), ~ mean(., na.rm = TRUE)))
#<agrupamos las medidas según el audio y sacamos el promedio total de cada audio

variables_numericas <- c(
  "Low_Freq", "High_Freq", "Max_Freq", "Max_Amp", "Peak_Freq", "Delta_Time", 
  "Min_Amp", "Peak_Time", "Length_frames", "Delta_Freq", "Avg_Power_Density"
) #Nuevamente seleccionamos las variables númericas
variables_excluir <- c("Nota", "ID", "Begin_Time", "End_Time","Pais") #Seleccionamos las variables de identificación que no queremos usar

variables <- variables_numericas[
  variables_numericas %in% names(promedios_audio) & 
    !variables_numericas %in% variables_excluir
] #Nos aseguramos de excluir las variables categóricas

print(variables)#Comprobamos

datos_pca <- promedios_audio %>%
  dplyr::select(all_of(variables)) %>% 
  dplyr::select(where(~ {
    var_calculada <- var(., na.rm = TRUE)
    !is.na(var_calculada) && var_calculada > 0
  })) #Seleccionamos la variable con varianza 0, es decir aquellas que no varían entre audios, esto para aumentar la confianza del PCA

datos_pca_numeric <- as.data.frame(lapply(datos_pca, function(x) {
  if (is.character(x) || is.factor(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
})) #Forzamos las columnas a númericas, para leerlas adecuadamente

PCA <- prcomp(datos_pca_numeric, scale = TRUE, center = TRUE) #Realizamos el Analisis de Componentes principales
PCA #Lo visualizamos
pca_df <- as.data.frame(PCA$x) %>%
  mutate(Audio = promedios_audio$Audio) #Lo convertimos a data frame y respetamos la agrupación de la variable "Audio"

pca_df <- pca_df %>%
  left_join(promedios_audio %>% dplyr::select(Audio, any_of(columnas_categoricas)), by = "Audio")#Unimos el data frame a la variable Audio

pca_df <- data.frame(PCA$x, Audio = promedios_audio$Audio) %>%
  left_join(distinct(datos_filtrados, Audio, Pais), by = "Audio") #Unimos también la variable Pais

ggplot(pca_df, aes(x = PC1, y = PC2, color = Pais)) +#Generamos el mapa
  geom_point(size = 3) + #Geometria de puntos
  stat_ellipse(level = 0.95) + #Elipses
  theme_minimal() +
  labs(title = "PCA - Agrupación por País",
       x = paste("PC1 (", round(summary(PCA)$importance[2,1]*100, 1), "%)"),
       y = paste("PC2 (", round(summary(PCA)$importance[2,2]*100, 1), "%)"))  #Generamos la leyenda con el porcentaje correspondiente al componente

localidades_unicas <- datos_filtrados %>%
  group_by(Audio) %>%
  summarise(Localidad = first(Localidad)) #Nos aseguramos de no repetir localidades

pca_df_loc <- data.frame(PCA$x, Audio = promedios_audio$Audio) %>%
  left_join(localidades_unicas, by = "Audio") #Agregamos la variable localidades

ggplot(pca_df_loc, aes(x = PC1, y = PC2, color = Localidad)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.2, "cm"),
    legend.box.spacing = unit(0.1, "cm"),
    legend.margin = margin(0, 0, 0, 0 )) +
  labs(title = "PCA - Agrupación por Localidad",
       x = paste("PC1 (", round(summary(PCA)$importance[2,1]*100, 1), "%)"),
       y = paste("PC2 (", round(summary(PCA)$importance[2,2]*100, 1), "%)")) #Generamos el mapa, agrupando los datos según la localidad

contrib_pc1 <- fviz_contrib(PCA, choice = "var", axes = 1, top = 15) #Analizamos la importancia de cada variable para el eje x
print(contrib_pc1) #Imprimimos el histograma

contrib_pc2 <- fviz_contrib(PCA, choice = "var", axes = 2, top = 15) #Analizamos la importancia de cada variable para el eje y
print(contrib_pc2) #Imprimimos el histograma

contrib_total <- fviz_contrib(PCA, choice = "var", axes = 1:2, top = 15) #Analizamos la importancia de cada variable para ambos ejes
print(contrib_total) #Imprimimos el histograma

world <- ne_countries(scale = "medium", returnclass = "sf") #Cargamos el mapa, del paquete rnaturalearth

datos_mapa <- pca_df %>% 
  left_join(datos_filtrados %>% 
              dplyr::select(Audio, Latitud, Longitud, Delta_Freq) %>% 
              distinct(Audio, .keep_all = TRUE), 
            by = "Audio") %>% 
  filter(!is.na(Latitud) & !is.na(Longitud)) %>% 
  mutate(Delta_Freq = as.numeric(Delta_Freq)) #Agregamos las columnas Latitud y Longitud, agrupando según el audio. Nos aseguramos que Delta_Freq sea númerica

mapa_delta <- ggplot() + 
  geom_sf(data = world, fill = "lightgreen", color = "grey") + 
  geom_point(data = datos_mapa, aes(x = Longitud, y = Latitud, color = Delta_Freq), size = 4, alpha = 0.8, shape = 19) + 
  scale_color_viridis(name = "Delta Frecuencia", option = "plasma", direction = -1) +  
  theme_minimal() + 
  labs(title = "Distribución Geográfica - Color por Delta Frecuencia", 
       subtitle = paste("Número de registros utilizados:", nrow(datos_mapa)), 
       x = "Longitud", y = "Latitud") + 
  theme(panel.background = element_rect(fill = "lightblue"), 
        panel.grid = element_line(color = "grey"), 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5)) #Generamos el mapa, coloreando los registros según la variable Delta_Freq

print(mapa_delta) #Imprimimos

ggsave("mapa_delta_freq.png", mapa_delta, width = 12, height = 8, dpi = 300) #Guardamos

mapa_norte_america <- ggplot() +
  geom_sf(data = world, fill = "#99CC66", color = "black") +
  geom_point(data = datos_mapa, aes(x = Longitud, y = Latitud, color = Delta_Freq), size = 4, alpha = 0.8, shape = 19) +
  scale_color_viridis(name = "Delta Frecuencia", option = "plasma", direction = -1) +
  coord_sf(xlim = c(-120, -30), ylim = c(-10, 35), expand = FALSE) + 
  theme_minimal() +
  labs(title = "Distribución en América del Norte y Centro América - Color por Delta Frecuencia", subtitle = paste("Número de Registros:", nrow(datos_mapa)), x = "Longitud", y = "Latitud")
#Generamos el mapa, coloreando los registros según la variable Delta_Freq, pero haciendo enfasis en America Central

print(mapa_norte_america) #Imprimimos

ggsave("mapa_norte_america.png", mapa_norte_america, width = 10, height = 8, dpi = 300) 
mapa_con_relieve <- ggplot() +  
  geom_sf(data = world, fill = "#99CC66", color = "black") + 
  geom_point(data = datos_mapa, aes(x = Longitud, y = Latitud, color = Delta_Freq), size = 4, alpha = 0.9, shape = 19) +
  geom_point(data = datos_mapa, aes(x = Longitud, y = Latitud), size = 4.2, color = "white", shape = 1, stroke = 0.8) +
  scale_color_viridis(name = "Delta Frecuencia (Hz)", option = "plasma", direction = -1) +
  coord_sf(xlim = c(-120, -30), ylim = c(-10, 35), expand = FALSE) +
  theme_void() + 
  labs(title = "Distribución Geográfica - Delta Frecuencia", subtitle = paste("Número de registros:", nrow(datos_mapa))) +
  annotation_scale(location = "br", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) + 
  annotation_north_arrow(location = "tr", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  theme( plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "#D4F1F9"), plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 10)), plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)), plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)), legend.position = "right", legend.title = element_text(face = "bold") )
#Generamos el mapa, coloreando los registros según la variable Delta_Freq, pero haciendo enfasis en America Central, agregamos escala y flecha norte

print(mapa_con_relieve) #Imprimimos
ggsave("mapa_con_relieve.png", mapa_con_relieve, width = 12, height = 9, dpi = 300) #Guardamos