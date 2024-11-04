#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, data.table,
               readxl, janitor, digest)

# Archivos locales
rutas <- list(
  output = here("clean/output/"))

# Definir archivos de Drive
drive_auth(email = "sierra.wells@datacivica.org")

pob_mun_2020_id <- drive_ls(as_id("1V9YIoqPa-2iceZQ6l50oG3Swn0liHqKR")) %>% 
  filter(name %in% "pob_loc_sexo_edad.csv") %>% 
  pull(id)

pob_ent_1990_2020_id <- drive_ls(as_id("1V9YIoqPa-2iceZQ6l50oG3Swn0liHqKR")) %>% 
  filter(name %in% "pob_ent_1990_2020.xlsx") %>% 
  pull(id)

stopifnot(length(pob_mun_2020_id) == 1,
          length(pob_ent_1990_2020_id) == 1)

drive_out <- as_id("1iUGz7rHB6dfmcHZTvWBUpsyPmtPlxEkU")

# Leer archivos de Drive ====
# Población municipal 2020
drive_download(
  as_id(pob_mun_2020_id),
  paste0(tempdir(), "/pob_mun_2020.csv"),
  overwrite = TRUE)

pob_mun_2020 <- fread(paste0(tempdir(), "/pob_mun_2020.csv"))

file.remove(paste0(tempdir(), "/pob_mun_2020.csv"))

# Población entidad 1990-2020
drive_download(
  as_id(pob_ent_1990_2020_id),
  paste0(tempdir(), "/pob_ent_1990_2020.xlsx"),
  overwrite = TRUE)

pob_ent_1990_2020_raw <- read_xlsx(paste0(tempdir(), "/pob_ent_1990_2020.xlsx"))

# Limpiar datos (solo población entidad 1990-2020) ====
nom_cols_pre <- pob_ent_1990_2020_raw %>% 
  slice(4) %>% 
  unlist()

nom_cols <- paste0(
  nom_cols_pre, 
  c(rep("", 2),
    rep("_1990", 3),
    rep("_1995", 3),
    rep("_2000", 3),
    rep("_2005", 3),
    rep("_2010", 3),
    rep("_2020", 3)))

names(pob_ent_1990_2020_raw) <- nom_cols

pob_ent_1990_2020 <- pob_ent_1990_2020_raw %>% 
  clean_names() %>% 
  rename(grupo_edad = grupo_quinquenal_de_edad) %>% 
  slice(-c(1:4)) %>% 
  pivot_longer(cols = -c(entidad_federativa, grupo_edad),
               names_to = c(".value", "anio"),
               names_pattern = "(.*)_([0-9]{4})")

# Resumir datos =====

# Total nacional, por sexo y en general (2000-2020)
total_nac <- pob_ent_1990_2020 %>% 
  filter(entidad_federativa == "Estados Unidos Mexicanos" & 
           grupo_edad == "Total") %>% 
  pivot_longer(cols = c(total, hombres, mujeres),
               names_to = "sexo",
               values_to = "pob") %>% 
  group_by(anio, sexo) %>% 
  summarize(pob = sum(as.numeric(pob))) %>%
  mutate(
    nivel = "nacional",
    nom_ent = NA,
    mun = NA,
    grupo_edad = "total")

# Total nacional por sexo y edad (2000-2020)
total_nac_edad <- pob_ent_1990_2020 %>% 
  filter(entidad_federativa == "Estados Unidos Mexicanos" & 
           grupo_edad != "Total") %>% 
  pivot_longer(cols = c(total, hombres, mujeres),
               names_to = "sexo",
               values_to = "pob") %>% 
  mutate(
    grupo_edad = case_when(
      grupo_edad %in% c("0 a 4 años", "5 a 9 años") ~ "0-9",
      grupo_edad %in% c("10 a 14 años", "15 a 19 años") ~ "10-19",
      grupo_edad %in% c("20 a 24 años", "25 a 29 años") ~ "20-29",
      grupo_edad %in% c("30 a 34 años", "35 a 39 años") ~ "30-39",
      grupo_edad %in% c("40 a 44 años", "45 a 49 años") ~ "40-49",
      grupo_edad %in% c("50 a 54 años", "55 a 59 años") ~ "50-59",
      grupo_edad %in% c("60 a 64 años", "65 a 69 años",
                        "70 a 74 años", "75 a 79 años",
                        "80 a 84 años", "85 a 89 años",
                        "90 a 94 años", "95 a 99 años") ~ "60+",
      T ~ NA_character_)) %>% 
  group_by(anio, sexo, grupo_edad) %>% 
  summarize(pob = sum(as.numeric(pob))) %>% 
  mutate(
    nivel = "nacional_c_edad",
    nom_ent = NA,
    mun = NA) %>% 
  filter(!is.na(grupo_edad))

# Total entidad, por sexo y en general (2000-2020)
total_ent <- pob_ent_1990_2020 %>% 
  filter(entidad_federativa != "Estados Unidos Mexicanos",
         grupo_edad == "Total") %>%
  pivot_longer(cols = c(total, hombres, mujeres),
               names_to = "sexo",
               values_to = "pob") %>% 
  rename(nom_ent = entidad_federativa) %>%
  group_by(anio, sexo, nom_ent) %>% 
  summarize(pob = sum(as.numeric(pob))) %>%
  mutate(
    nivel = "entidad",
    mun = NA,
    grupo_edad = "total")

# Total municipal, por sexo y en general (2020)
total_muni <- pob_mun_2020 %>% 
  filter(nivel == "muni") %>% 
  select(nom_ent, mun, nom_mun, pobtot, pobfem, pobmas) %>% 
  mutate(across(.cols = c(pobtot, pobfem, pobmas),
                .fns = as.numeric)) %>%
  pivot_longer(cols = c(pobtot, pobfem, pobmas),
               names_to = "sexo",
               values_to = "pob") %>% 
  mutate(sexo = case_when(
    sexo == "pobtot" ~ "total",
    sexo == "pobfem" ~ "mujeres",
    sexo == "pobmas" ~ "hombres"),
    anio = 2020,
    nivel = "municipal",
    grupo_edad = "total")

# Crear catálogo de claves y nombres para entidades ====
cat_ent <- pob_mun_2020 %>% 
  filter(nivel == "entidad") %>% 
  rename(ent = entidad) %>%
  select(ent, nom_ent) %>% 
  distinct()

all(cat_ent$nom_ent %in% total_ent$nom_ent)

# Juntar datos ====
pobs_para_juntar <- list(total_nac, total_nac_edad, total_ent, total_muni) %>% 
  map(~mutate(.x, across(everything(), as.character)))

pob_juntos <- bind_rows(pobs_para_juntar) %>% 
  left_join(cat_ent, by = "nom_ent") %>% 
  mutate(
    ent = str_pad(ent, width = 2, side = "left", pad = "0"),
    mun = str_pad(mun, width = 5, side = "left", pad = "0"))

# Subir a Drive ====
write_csv(pob_juntos, paste0(tempdir(), "/poblacion.csv"))

drive_upload(paste0(tempdir(), "/poblacion.csv"),
             path = drive_out,
             overwrite = T)

file.remove(paste0(tempdir(), "/poblacion.csv"))

# Escribir hash ====
hash <- digest(pob_juntos, algo = "sha256")

writeLines(hash, paste0(rutas$output, "2_clean_pob.txt"))

# done.
