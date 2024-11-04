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

pob_id <- drive_in %>% 
  filter(str_detect(name, "poblacion")) %>% 
  pull(id)

defun_id <- drive_in %>% 
  filter(str_detect(name, "homicidios")) %>% 
  pull(id)

abrevs_id <- drive_ls(as_id("1XAqIRtOpWm7GL3XMqZln9NC4Tv9Y_nkx")) %>% 
  filter(name == "munis_ents_catag.csv") %>% 
  pull(id)

stopifnot(length(pob_id) == 1,
          length(defun_id) == 1,
          length(abrevs_id) == 1)

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

# Abreviaciones
drive_download(as_id(abrevs_id),
               path = paste0(tempdir(), "/abrevs.csv"),
               overwrite = TRUE)

abrevs <- read_csv(paste0(tempdir(), "/abrevs.csv")) %>% 
  select(cve_ent, abrev_ent) %>% 
  distinct()

file.remove(paste0(tempdir(), "/abrevs.csv"))

# PREP PARA GRAFS ====
min_year <- 2005
max_year <- max(defun$anio_ocur, na.rm = TRUE)

fuente_cap <- "Fuente: Elaboración por Data Cívica a partir de los Registros de Mortalidad del INEGI"
femic_dc_cap <- paste0("\n* Clasificamos los homicidios de mujeres como feminicidios si por lo menos una de las siguientes condiciones se cumple:",
                       "\ni) la víctima fue asesinada en el hogar, ii) la cuasa de muerte fue agresión sexual, iii) hubo violencia familiar,",
                       "\niv) hubo algún tipo de parentesco con el agresor, v) la causa de defunción fue maltrato o abandono.")

# Tasa de violencia letal a nivel nacional
pob_nac <- pob %>% 
  filter(nivel == "nacional",
         sexo == "total",
         anio == max(anio)) %>% 
  select(anio, pob) %>% 
  mutate(anio = max_year)

tasa_nac <- defun %>% 
  filter(anio_ocur == max_year) %>%
  group_by(anio_ocur) %>% 
  summarize(defun = n()) %>% 
  left_join(pob_nac, by = c("anio_ocur" = "anio")) %>% 
  mutate(tasa = defun / pob * 100000) %>% 
  pull(tasa) %>%
  round(1)

cambio_tasa_nac <- defun %>% 
  filter(anio_ocur %in% (max_year - 1):max_year) %>%
  group_by(anio_ocur) %>%
  reframe(defun = n()) %>%
  mutate(cambio = defun[anio_ocur == max_year] - defun[anio_ocur == (max_year - 1)]) %>% 
  filter(anio_ocur == max_year) %>% 
  left_join(pob_nac, by = c("anio_ocur" = "anio")) %>% 
  mutate(cambio_tasa = cambio / pob * 100000) %>% 
  pull(cambio_tasa) %>%
  round(1)

# TASA DE VIOLENCIA LETAL A NIVEL ENTIDAD (MAX_YEAR) ====
pob_ent_gen <- pob %>% 
  filter(nivel == "entidad",
         sexo == "total",
         anio == max(anio)) %>% 
  select(anio, ent, nom_ent, pob) %>% 
  mutate(anio = max_year)

tasa_viol_letal_ent <- defun %>% 
  filter(anio_ocur == max_year,
         !is.na(ent_ocurr)) %>%
  group_by(anio_ocur, ent_ocurr) %>% 
  summarize(defun = n()) %>% 
  mutate(ent_ocurr = str_pad(ent_ocurr, 2, pad = "0")) %>%
  left_join(pob_ent_gen, by = c("anio_ocur" = "anio",
                                "ent_ocurr" = "ent")) %>% 
  mutate(tasa = defun / pob * 100000,
         nom_ent = case_when(
           nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
           nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
           nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
           nom_ent == "Ciudad de México" ~ "CDMX",
           TRUE ~ nom_ent))

stopifnot(all(!is.na(tasa_viol_letal_ent$tasa)))

tasa_viol_letal_ent_graf <- ggplot(data = tasa_viol_letal_ent,
                                   aes(x = tasa, y = reorder(nom_ent, tasa))) +
  geom_col(fill = pal[1]) + 
  geom_vline(xintercept = tasa_nac, linetype = "dashed") + 
  geom_text(aes(label = round(tasa, 1)), hjust = -0.1,
            family = tipo, fontface = "bold", color = texto) +
  geom_label(aes(x = tasa_nac, y = 16, label = paste0("Nacional: ", tasa_nac)),
           hjust = -0.05, family = tipo, fontface = "bold",
           color = texto, fill = fondo, label.size = 0) +
  labs(title = paste0("Tasa de violencia letal en ", max_year),
       subtitle = "Homicidios y feminicidios por cada 100,000 habitantes",
       x = "", y = "", caption = fuente_cap) +
  tema +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
for (device in devices){
  ggsave(
    paste0(rutas$output, "2_barras_viol_letal_ent", device),
    tasa_viol_letal_ent_graf,
    width = 10, height = 8, dpi = 300)
}

# TASA DE VIOLENCIA LETAL POR SEXO A NIVEL ENTIDAD (MAX_YEAR) ====
pob_ent_sexo <- pob %>% 
  filter(nivel == "entidad",
         sexo != "total",
         anio == max(anio)) %>% 
  select(anio, ent, nom_ent, sexo, pob) %>% 
  mutate(anio = max_year)

tasa_viol_letal_ent_sexo <- defun %>%
  filter(anio_ocur == max_year,
         !is.na(ent_ocurr),
         !is.na(tipo_hom)) %>%
  mutate(
    tipo_hom = case_when(
      str_detect(tipo_hom, "Homicidio") ~ "Homicidios",
      tipo_hom == "Feminicidio" ~ "Feminicidios DC*"),
    sexo = case_when(
      sexo == "Mujer" ~ "mujeres",
      sexo == "Hombre" ~ "hombres"),
    ent_ocurr = str_pad(ent_ocurr, 2, pad = "0")) %>%
  group_by(ent_ocurr, tipo_hom, sexo) %>% 
  summarize(defun = n()) %>% 
  left_join(pob_ent_sexo, by = c("ent_ocurr" = "ent", "sexo")) %>% 
  mutate(tasa_tipo_hom = defun / pob * 100000,
         nom_ent = case_when(
           nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
           nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
           nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
           nom_ent == "Ciudad de México" ~ "CDMX",
           TRUE ~ nom_ent)) %>% 
  group_by(nom_ent, ent_ocurr, sexo) %>%
  mutate(tasa_sexo = sum(tasa_tipo_hom),
         tipo_hom = factor(tipo_hom,
                           levels = c("Homicidios", "Feminicidios DC*")))

stopifnot(all(!is.na(tasa_viol_letal_ent_sexo$tasa_sexo)),
          all(!is.na(tasa_viol_letal_ent_sexo$tasa_tipo_hom)),
          all(!is.na(tasa_viol_letal_ent_sexo$nom_ent)))

tasa_viol_letal_ent_sexo_orden <- tasa_viol_letal_ent_sexo %>% 
  filter(sexo == "hombres") %>%
  arrange(tasa_sexo) %>%
  pull(nom_ent)

tasa_viol_letal_ent_sexo_graf <- ggplot(data = tasa_viol_letal_ent_sexo %>% 
                                          mutate(nom_ent = factor(nom_ent,
                                                                  levels = tasa_viol_letal_ent_sexo_orden)),
                                   aes(x = tasa_tipo_hom, y = nom_ent)) +
  facet_wrap(~sexo, scales = "free_x",
             labeller = labeller(sexo = c("hombres" = "Hombres", "mujeres" = "Mujeres"))) +
  geom_col(aes(fill = tipo_hom), position = position_stack()) + 
  geom_text(aes(label = round(tasa_sexo, 1), x = tasa_sexo), hjust = -0.1,
            family = tipo, fontface = "bold", color = texto) +
  labs(title = paste0("Tasa de violencia letal en ", max_year),
       subtitle = paste("<span style='color:", pal[1],
                        "; font-family:\"Barlow Bold\";'>Homicidios</span> y <span style='color:",
                        pal[2], "; font-family:\"Barlow Bold\";'>feminicidios*</span> por 100,000 habitantes"),
       x = "", y = "", caption = paste0(fuente_cap, femic_dc_cap)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = pal) +
  tema +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

for (device in devices){
  ggsave(
    paste0(rutas$output, "2_barras_viol_letal_ent_sexo", device),
    tasa_viol_letal_ent_sexo_graf,
    width = 10, height = 8, dpi = 300)
}

# CAMBIO VIOLENCIA LETAL A NIVEL ENTIDAD (MAX_YEAR - 1 --> MAX_YEAR) ====
cambio_viol_letal_ent <- defun %>% 
  filter(anio_ocur %in% (max_year - 1):max_year,
         !is.na(ent_ocurr)) %>%
  group_by(anio_ocur, ent_ocurr) %>% 
  summarize(defun = n()) %>% 
  mutate(ent_ocurr = str_pad(ent_ocurr, 2, pad = "0")) %>%
  left_join(pob_ent_gen %>% select(-anio), by = c("ent_ocurr" = "ent")) %>% 
  mutate(tasa = defun / pob * 100000,
         nom_ent = case_when(
           nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
           nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
           nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
           nom_ent == "Ciudad de México" ~ "CDMX",
           TRUE ~ nom_ent)) %>% 
  group_by(ent_ocurr, nom_ent) %>% 
  summarize(cambio = tasa[anio_ocur == max_year] - tasa[anio_ocur == (max_year - 1)])

stopifnot(all(!is.na(cambio_viol_letal_ent$cambio)))

cambio_viol_letal_ent_graf <- ggplot(data = cambio_viol_letal_ent,
                                    aes(x = cambio, y = reorder(nom_ent, cambio),
                                        fill = (cambio > 0))) +
  geom_col() + 
  geom_vline(xintercept = cambio_tasa_nac, linetype = "dashed") +
  geom_text(aes(x = cambio_tasa_nac, y = 32,
                 label = paste0("Nacional: ", cambio_tasa_nac)),
             hjust = 1.1, family = tipo, fontface = "bold", color = texto) +
  geom_label(aes(label = round(cambio, 1),
                x = ifelse(cambio > 0, cambio + 1.5, cambio - 1.5)),
            family = tipo, fontface = "bold", color = texto, fill = fondo,
            label.size = 0, label.padding = unit(0.05, "lines")) +
  labs(title = paste0("¿Como cambió la tasa de violencia letal entre ", max_year - 1, " y ", max_year, "?"),
       subtitle = "Cambio en homicidios y feminicidios por 100,000 habitantes",
       x = "", y = "", caption = fuente_cap) +
  scale_fill_manual(values = pal) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

for (device in devices){
  ggsave(
    paste0(rutas$output, "2_barras_cambio_ent", device),
    cambio_viol_letal_ent_graf,
    width = 10, height = 8)
}

# SCATTER ENTIDADES: TASA VS. CAMBIO ====
ents_para_scatter <- cambio_viol_letal_ent %>% 
  left_join(tasa_viol_letal_ent, by = c("ent_ocurr", "nom_ent")) %>% 
  left_join(abrevs, by = c("ent_ocurr" = "cve_ent")) %>%
  ungroup() %>% 
  select(abrev_ent, cambio, tasa) %>% 
  mutate(
    group = case_when(
      cambio > 0 & tasa > tasa_nac ~ "caliente",
      cambio <= 0 & tasa < tasa_nac ~ "frío",
      T ~ "neutro"),
    abrev_ent = ifelse(abrev_ent == "DF", "CDMX", abrev_ent))

ents_scatter <- ggplot(data = ents_para_scatter,
                       aes(x = tasa, y = cambio, label = abrev_ent)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = ejes) +
  geom_vline(xintercept = tasa_nac, linetype = "dashed", linewidth = 0.5, color = ejes) +
  geom_point(aes(color = group), size = 5) + 
  # Chance habrá que ajustar los valores de x e y para que no se empalmen
  geom_label(aes(label = "Tasa menor que nacional\ny aumentó",
                  x = tasa_nac/2, y = 10), color = texto, family = tipo,
             fontface = "bold", fill = fondo) +
  geom_label(aes(label = "Tasa mayor que nacional\ny disminuyó",
                  x = tasa_nac * 1.5, y = -15), color = texto, family = tipo,
             fontface = "bold", fill = fondo) +
  geom_label(aes(label = "Tasa mayor que nacional\ny aumentó",
                  x = tasa_nac*1.5, y = 10), color = pal[2], family = tipo,
             fontface = "bold", fill = fondo) +
  geom_label(aes(label = "Tasa menor que nacional\ny disminuyó",
                  x = tasa_nac/2, y = -15), color = pal[1], family = tipo,
            fontface = "bold", fill = fondo) +
  geom_text_repel(aes(label = abrev_ent, color = group), family = tipo, fontface = "bold",
                  min.segment.length = 0.5, max.overlaps = Inf, segment.color = texto) +
  labs(title = paste0("Tasa de violencia letal (", max_year, ") vs. cambio en tasa (",
                      max_year - 1, " - ", max_year, ")"),
       subtitle = "Homicidios y feminicidios por 100,000 habitantes",
       x = paste0("Tasa ", max_year), y = paste0("Cambio ", max_year - 1, " - ", max_year),
       caption = fuente_cap) +
  scale_color_manual(values = c("caliente" = pal[2], "frío" = pal[1], "neutro" = texto)) +
  tema +
  theme(legend.position = "none")

for (device in devices){
  ggsave(
    paste0(rutas$output, "2_scatter_ents", device),
    ents_scatter,
    width = 10, height = 6)
}

# MUNICIPIOS MÁS VIOLENTOS (MAX_YEAR) ====
pob_mun <- pob %>% 
  filter(nivel == "municipal",
         anio == max(anio),
         sexo == "total",
         grupo_edad == "total") %>% 
  select(anio, ent, nom_ent, mun, nom_mun, pob) %>% 
  mutate(anio = max_year)

munis_mas_violentos <- defun %>% 
  filter(anio_ocur == max_year,
         !is.na(ent_ocurr),
         !is.na(mun_ocurr)) %>%
  group_by(anio_ocur, ent_ocurr, mun_ocurr) %>%
  summarize(defun = n()) %>% 
  mutate(ent_ocurr = str_pad(ent_ocurr, 2, pad = "0"),
         mun_ocurr = str_pad(mun_ocurr, 3, pad = "0"),
         mun = paste0(ent_ocurr, mun_ocurr)) %>% 
  left_join(pob_mun, by = c("anio_ocur" = "anio", "mun")) %>% 
  left_join(abrevs, by = c("ent_ocurr" = "cve_ent")) %>% 
  ungroup() %>% 
  mutate(
    tasa = defun / pob * 100000,
    nom_mun = paste0(nom_mun, ", ", abrev_ent)) %>% 
  filter(pob >= 5000) %>% 
  slice_max(order_by = tasa, n = 20)
  
stopifnot(nrow(munis_mas_violentos) == 20)

munis_mas_violentos_graf <- ggplot(data = munis_mas_violentos,
                                   aes(x = tasa, y = reorder(nom_mun, tasa))) +
  geom_col(fill = pal[1]) + 
  geom_text(aes(label = round(tasa, 0)), hjust = -0.1,
            family = tipo, fontface = "bold", color = texto) +
  labs(title = paste0("Municipios con más violencia letal en ", max_year),
       subtitle = "Homicidios y feminicidios por 100,000 habitantes",
       x = "", y = "", caption = paste0(fuente_cap,
                                        "\nSe excluyen municipios con poblaciones menores de 5 mil personas.")) +
  tema +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

for (device in devices){
  ggsave(
    paste0(rutas$output, "2_barras_munis_mas_violentos", device),
    munis_mas_violentos_graf,
    width = 10, height = 8)
}

# MAPA: TASA DE VIOLENCIA LETAL POR MUNICIPIO (MAX_YEAR) ====
data("df_mxmunicipio_2020")
munis_para_mapa <- defun %>% 
  filter(anio_ocur == max_year,
         !is.na(ent_ocurr),
         !is.na(mun_ocurr)) %>%
  group_by(anio_ocur, ent_ocurr, mun_ocurr) %>%
  summarize(defun = n()) %>% 
  mutate(ent_ocurr = str_pad(ent_ocurr, 2, pad = "0"),
         mun_ocurr = str_pad(mun_ocurr, 3, pad = "0"),
         mun = paste0(ent_ocurr, mun_ocurr)) %>% 
  right_join(pob_mun, by = c("anio_ocur" = "anio", "mun")) %>% 
  replace_na(list(defun = 0)) %>%
  ungroup() %>% 
  mutate(
    tasa = defun / pob * 100000) %>% 
  right_join(df_mxmunicipio_2020, by = c("mun" = "region")) %>% 
  rename(
    value = tasa,
    region = mun)

munis_mapa <- mxmunicipio_choropleth(munis_para_mapa, num_colors = 1) +
  labs(title = paste0("Tasa de violencia letal en ", max_year),
       subtitle = "Homicidios y feminicidios por 100,000 habitantes",
       caption = paste0(fuente_cap),
       fill = "", x = "", y = "") +
  scale_fill_gradient2(low = grad[2], high = grad[1],
                       transform = "sqrt") +
  theme_gray() +
  tema +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        axis.line = element_blank())

for (device in devices){
  ggsave(
    paste0(rutas$output, "2_mapa_munis", device),
    munis_mapa,
    width = 10, height = 8)
}

# done.