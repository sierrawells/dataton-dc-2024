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
femic_dc_cap <- paste0("\n* Clasificamos los homicidios de mujeres como feminicidios si por lo menos una de las siguientes condiciones se cumple:",
                  "\ni) la víctima fue asesinada en el hogar, ii) la cuasa de muerte fue agresión sexual, iii) hubo violencia familiar,",
                  "\niv) hubo algún tipo de parentesco con el agresor, v) la causa de defunción fue maltrato o abandono.")

# TASA DE VIOLENCIA LETAL ====
pob_gen <- pob %>% 
  filter(nivel == "nacional",
         sexo == "total",
         anio >= min_year) %>% 
  select(anio, pob) %>% 
  # Llenar años faltantes con la población del año más reciente
  complete(anio = min_year:max_year) %>%
  fill(pob)

tasa_viol_letal <- defun %>% 
  filter(anio_ocur >= min_year) %>%
  group_by(anio_ocur) %>% 
  summarize(defun = n()) %>% 
  left_join(pob_gen, by = c("anio_ocur" = "anio")) %>% 
  mutate(tasa = defun / pob * 100000)

tasa_viol_letal_graf <- ggplot(tasa_viol_letal, aes(x = anio_ocur, y = tasa)) +
  geom_line(color = pal[1]) +
  geom_point(color = pal[1]) +
  labs(title = "Tasa de violencia letal a nivel nacional",
       subtitle = "Homicidios y feminicidios por cada 100,000 habitantes",
       x = "", y = "", caption = fuente_cap) +
  tema +
  scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
  scale_y_continuous(limits = c(0, NA))

for (device in devices){
  ggsave(
    paste0(rutas$output, "1_fiebre_viol_letal", device),
    tasa_viol_letal_graf,
    width = 10, height = 5, dpi = 300)
}

# TASA DE VIOLENCIA LETAL POR SEXO ====
pob_sexo <- pob %>% 
  filter(nivel == "nacional",
         sexo != "total",
         anio >= min_year) %>% 
  select(anio, sexo, pob) %>% 
  complete(anio = min_year:max_year,
           sexo = c("hombres", "mujeres")) %>%
  arrange(sexo, anio) %>%
  fill(pob)

tasa_viol_letal_sexo <- defun %>%
  filter(anio_ocur >= min_year,
         !is.na(tipo_hom)) %>%
  mutate(
    tipo_hom = case_when(
      str_detect(tipo_hom, "Homicidio") ~ "Homicidios",
      tipo_hom == "Feminicidio" ~ "Feminicidios DC*"),
    sexo = case_when(
      sexo == "Mujer" ~ "mujeres",
      sexo == "Hombre" ~ "hombres")) %>% 
  group_by(anio_ocur, tipo_hom, sexo) %>% 
  summarize(defun = n()) %>% 
  left_join(pob_sexo, by = c("anio_ocur" = "anio", "sexo")) %>% 
  mutate(tasa_tipo_hom = defun / pob * 100000) %>% 
  group_by(anio_ocur, sexo) %>% 
  mutate(tasa_sexo = sum(tasa_tipo_hom),
         tipo_hom = factor(tipo_hom,
                           levels = c("Homicidios", "Feminicidios DC*")))

tasa_viol_letal_sexo_graf <- ggplot(tasa_viol_letal_sexo,
                                    aes(x = anio_ocur, y = tasa_tipo_hom,
                                        color = tipo_hom, fill = tipo_hom)) +
  facet_wrap(~sexo, 
             labeller = labeller(sexo = c("hombres" = "Hombres", "mujeres" = "Mujeres"))) +
  geom_area(alpha = 0.5) +
  # Point: tasas totales por sexo
  geom_point(aes(y = tasa_sexo)) +
  # Point: tasa de feminicidios
  geom_point(data = tasa_viol_letal_sexo %>% 
               filter(tipo_hom == "Feminicidios DC*"),
             aes(y = tasa_tipo_hom)) +
  labs(
    title = "Tasa de violencia letal a nivel nacional",
    subtitle = paste("<span style='color:", pal[1],
                     "; font-family:\"Barlow Bold\";'>Homicidios</span> y <span style='color:",
                     pal[2], "; font-family:\"Barlow Bold\";'>feminicidios*</span> por cada 100,000 habitantes"),
    x = "", y = "",
    caption = paste0(fuente_cap, femic_dc_cap)
  ) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(min_year, max_year, 1)) +
  scale_y_continuous(limits = c(0, NA)) +
  tema +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.subtitle = element_markdown())

for (device in devices){
  ggsave(
    paste0(rutas$output, "1_fiebre_viol_letal_sexo", device),
    tasa_viol_letal_sexo_graf,
    width = 10, height = 8, dpi = 300)
}

# done.

