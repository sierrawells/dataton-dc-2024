#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, ggtext, mxmaps, ggrepel)

# Archivos locales
rutas <- list(
  output = here("descriptives/output/"),
  tema = here("descriptives/tema.R"))

source(rutas$tema)

# Definir archivos de Drive
drive_auth(email = "sierra.wells@datacivica.org")
drive_in <- drive_ls(as_id("1iUGz7rHB6dfmcHZTvWBUpsyPmtPlxEkU"))

defun_id <- drive_in %>% 
  filter(str_detect(name, "homicidios")) %>% 
  pull(id)

pob_id <- drive_in %>% 
  filter(str_detect(name, "poblacion")) %>% 
  pull(id)

stopifnot(length(defun_id) == 1,
          length(pob_id) == 1)

# Leer archivos de Drive ====
# Homicidios
drive_download(as_id(defun_id),
               path = paste0(tempdir(), "/defun.csv"),
               overwrite = TRUE)

defun <- read_csv(paste0(tempdir(), "/defun.csv"))

file.remove(paste0(tempdir(), "/defun.csv"))

# Población
drive_download(as_id(pob_id),
               path = paste0(tempdir(), "/pob.csv"),
               overwrite = TRUE)

pob <- read_csv(paste0(tempdir(), "/pob.csv"))

file.remove(paste0(tempdir(), "/pob.csv"))

# PREP PARA GRAFS ====
min_year <- 2005
max_year <- max(defun$anio_ocur, na.rm = TRUE)

fuente_cap <- "Fuente: Elaboración por Data Cívica a partir de los Registros de Mortalidad del INEGI"

pob_edad <- pob %>% 
  filter(nivel == "nacional_c_edad",
         sexo != "total",
         anio %in% min_year:max_year) %>% 
  select(anio, sexo, grupo_edad, pob) %>%
  complete(anio = min_year:max_year, sexo, grupo_edad) %>%
  # Complete missing years between min_year and max_year with most recent pob
  arrange(sexo, grupo_edad, anio) %>%
  group_by(sexo, grupo_edad) %>%
  fill(pob, .direction = "downup") %>%
  ungroup()
  
# TASAS POR EDAD ====
tasas_edad <- defun %>% 
  filter(anio_ocur >= min_year,
         !is.na(edad),
         !is.na(sexo)) %>%
  mutate(grupo_edad = case_when(
    edad < 10 ~ "0-9",
    edad < 20 ~ "10-19",
    edad < 30 ~ "20-29",
    edad < 40 ~ "30-39",
    edad < 50 ~ "40-49",
    edad < 60 ~ "50-59",
    edad >= 60 ~ "60+"),
    sexo = case_when(
      sexo == "Hombre" ~ "hombres",
      sexo == "Mujer" ~ "mujeres")) %>% 
  group_by(anio_ocur, grupo_edad, sexo) %>% 
  summarise(defun = n()) %>% 
  left_join(pob_edad, by = c("anio_ocur" = "anio", "grupo_edad", "sexo")) %>% 
  mutate(tasa = defun / pob * 100000)

tasas_edad_graf <- ggplot(tasas_edad, 
                          aes(x = anio_ocur, y = tasa, color = grupo_edad)) + 
  facet_wrap(~sexo, labeller = as_labeller(c(hombres = "Hombres", mujeres = "Mujeres"))) + 
  geom_line() + 
  geom_point() +
  labs(title = "Tasa de violencia letal por sexo y edad de la víctima",
       subtitle = "Homicidios y feminicidios por cada 100,000 habitantes",
       x = "", y = "", color = "Edad (años)", caption = fuente_cap) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
  scale_y_continuous(limits = c(0, NA)) +
  tema +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1))

for (device in devices){
  ggsave(
    paste0(rutas$output, "3_fiebre_tasa_edad", device),
    tasas_edad_graf,
    width = 10, height = 5, dpi = 300)
}

# DISTRIBUCIÓN DE EDADES ====
dist_edad <- defun %>% 
  filter(anio_ocur >= min_year,
         !is.na(edad),
         !is.na(sexo)) %>%
  mutate(grupo_edad = case_when(
    edad < 10 ~ "0-9",
    edad < 20 ~ "10-19",
    edad < 30 ~ "20-29",
    edad < 40 ~ "30-39",
    edad < 50 ~ "40-49",
    edad < 60 ~ "50-59",
    edad >= 60 ~ "60+"),
    sexo = case_when(
      sexo == "Hombre" ~ "hombres",
      sexo == "Mujer" ~ "mujeres")) %>% 
  group_by(anio_ocur, grupo_edad, sexo) %>% 
  summarise(defun = n()) %>% 
  group_by(anio_ocur, sexo) %>% 
  mutate(defun_total = sum(defun),
         perc = defun / defun_total,
         grupo_edad = factor(grupo_edad,
                             levels = c("0-9", "10-19", "20-29", "30-39",
                                        "40-49", "50-59", "60+")))

dist_edad_graf <- ggplot(dist_edad, 
                         aes(x = anio_ocur, y = perc, fill = fct_rev(grupo_edad))) +  
  facet_wrap(~sexo, labeller = as_labeller(c(hombres = "Hombres", mujeres = "Mujeres"))) +
  geom_area() +
  labs(title = "Distribución de edades de víctimas de homicidios y feminicidios",
       x = "", y = "", fill = "Edad (años)", caption = fuente_cap) +
  scale_fill_manual(values = rev(pal[1:7])) +
  scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  tema +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1))

for (device in devices){
  ggsave(
    paste0(rutas$output, "3_area_dist_edad", device),
    dist_edad_graf,
    width = 10, height = 5, dpi = 300)
} 

# done.
