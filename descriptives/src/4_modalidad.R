#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggrepel, googledrive, ggtext)

# Archivos locales
rutas <- list(
  output = here("descriptives/output/"),
  tema = here("descriptives/tema.R"))

source(rutas$tema)

# Definir archivos de Drive
drive_auth(email = "sierra.wells@datacivica.org")
drive_in <- drive_ls(as_id("1iUGz7rHB6dfmcHZTvWBUpsyPmtPlxEkU"))

pob_id <- drive_in %>% 
  filter(str_detect(name, "poblacion")) %>% 
  pull(id)

defun_id <- drive_in %>% 
  filter(str_detect(name, "homicidios")) %>% 
  pull(id)

stopifnot(length(pob_id) == 1,
          length(defun_id) == 1)

# Leer archivos de Drive ====
# Población
drive_download(as_id(pob_id),
               path = paste0(tempdir(), "/pob.csv"),
               overwrite = TRUE)

pob <- read_csv(paste0(tempdir(), "/pob.csv"))

file.remove(paste0(tempdir(), "/pob.csv"))

# Homicidios
drive_download(as_id(defun_id),
               path = paste0(tempdir(), "/defun.csv"),
               overwrite = TRUE)

defun <- read_csv(paste0(tempdir(), "/defun.csv"))

file.remove(paste0(tempdir(), "/defun.csv"))

# PREP PARA GRAFS ====
min_year <- 2005
max_year <- max(defun$anio_ocur, na.rm = TRUE)

fuente_cap <- "Fuente: Elaboración por Data Cívica a partir de los Registros de Mortalidad del INEGI"

# TASA POR MODALIDAD + SEXO ====
# pob_sexo <- pob %>% 
#   filter(nivel == "nacional",
#          sexo != "total",
#          anio >= min_year) %>% 
#   select(anio, sexo, pob) %>% 
#   complete(anio = min_year:max_year,
#            sexo = c("hombres", "mujeres")) %>%
#   arrange(sexo, anio) %>%
#   fill(pob)
# 
# tasa_mod <- defun %>% 
#   filter(anio_ocur >= min_year,
#          !is.na(causa_hom),
#          !is.na(sexo)) %>% 
#   group_by(anio_ocur, causa_hom, sexo) %>% 
#   summarise(defun = n()) %>%
#   mutate(sexo = case_when(sexo == "Hombre" ~ "hombres",
#                           sexo == "Mujer" ~ "mujeres")) %>%
#   left_join(pob_sexo, by = c("anio_ocur" = "anio", "sexo")) %>% 
#   mutate(tasa = defun / pob * 100000)
# 
# tasa_mod_graf <- ggplot(tasa_mod, aes(x = anio_ocur, y = tasa,
#                                       color = fct_reorder(causa_hom, -tasa))) +
#   facet_wrap(~sexo, labeller = labeller(sexo = c("hombres" = "Hombres", "mujeres" = "Mujeres"))) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Tasa de violencia letal por modalidad",
#        subtitle = "Homicidios y feminicidios por cada 100,000 habitantes",
#        x = "", y = "", color = "", caption = fuente_cap) +
#   scale_color_manual(values = pal) +
#   scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
#   scale_y_continuous(limits = c(0, NA)) +
#   tema +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1))
# 
# for (device in devices){
#   ggsave(
#     paste0(rutas$output, "4_fiebre_tasa_mod_sexo", device),
#     tasa_mod_graf,
#     width = 10, height = 6, dpi = 300)
# }

# DISTRIBUCIÓN DE MODALIDADES POR SEXO ====
dist_mod <- defun %>% 
  filter(anio_ocur >= min_year,
         !is.na(causa_hom),
         !is.na(sexo)) %>% 
  group_by(anio_ocur, causa_hom, sexo) %>% 
  summarise(defun = n()) %>% 
  group_by(anio_ocur, sexo) %>% 
  mutate(defun_total = sum(defun),
         perc = defun / defun_total)

dist_mod_graf <- ggplot(dist_mod, aes(x = anio_ocur, y = perc,
                                     fill = fct_reorder(causa_hom, perc))) +
  facet_wrap(~sexo, labeller = labeller(sexo = c("Hombre" = "Hombres", "Mujer" = "Mujeres"))) +
  geom_area() +
  labs(title = "Causa de lesión letal por sexo de la víctima",
       x = "", y = "", fill = "", caption = fuente_cap) +
  scale_fill_manual(values = rev(pal[1:6])) +
  scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
  # Make y axis percentages
  scale_y_continuous(labels = scales::percent_format()) +
  tema +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1))

for (device in devices){
  ggsave(
    paste0(rutas$output, "4_area_dist_mod_sexo", device),
    dist_mod_graf,
    width = 10, height = 6, dpi = 300)
}
  
# done.
