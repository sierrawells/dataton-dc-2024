#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO
# Agregar grupo_edad
# CHECAR QUE LOS CÓDIGOS PARA 2023 NO HAYAN CAMBIADO
# (esp. vio_fami/par_agre, edo_civil, escolarida)

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, yaml, googledrive, digest, data.table, upstartr)

# Archivos locales
rutas <- list(
  output = here("clean/output/"),
  patrones = here("clean/hand/patrones_causas.yaml"))

# Definir carpetas de Drive
drive_auth(email = "sierra.wells@datacivica.org")

drive_in <- drive_ls(as_id("1VqSBZsIn2g9BJ3iTE2L4g7sMBWK2zjrK"))
drive_out <- as_id("1iUGz7rHB6dfmcHZTvWBUpsyPmtPlxEkU")

stopifnot(nrow(drive_in) == 1)

# Leer lista de patrones ====
patrones <- read_yaml(rutas$patrones)

pat_vivienda <- patrones$vivienda %>% 
  str_flatten("|")
pat_inst_residencial <- patrones$inst_residencial %>% 
  str_flatten("|")
pat_via_publica <- patrones$via_publica %>% 
  str_flatten("|")
pat_otro_lugar <- patrones$otro_lugar %>% 
  str_flatten("|")
pat_lugar_na <- patrones$lugar_na %>% 
  str_flatten("|")

pat_arma_fuego <- patrones$arma_fuego %>% 
  str_flatten("|")
pat_arma_blanca <- patrones$arma_blanca %>%
  str_flatten("|")
pat_ahorcar <- patrones$ahorcar %>% 
  str_flatten("|")
pat_veneno <- patrones$veneno %>%
  str_flatten("|")
pat_fuerza_corporal <- patrones$fuerza_corporal %>%
  str_flatten("|")
pat_sexual <- patrones$sexual %>%
  str_flatten("|")
pat_maltrato <- patrones$maltrato %>%
  str_flatten("|")

# Leer datos de Drive ====
id_para_leer <- drive_in %>% 
  pull(id)

drive_download(
  as_id(id_para_leer),
  paste0(tempdir(), "/defunciones.csv"),
  overwrite = TRUE)

defun_para_limpiar <- fread(paste0(tempdir(), "/defunciones.csv"))

file.remove(paste0(tempdir(), "/defunciones.csv"))

# Limpiar datos =====
defun_limpio <- defun_para_limpiar %>%
  mutate(
    ent_ocurr = ifelse(as.numeric(ent_ocurr) == 99, NA, ent_ocurr),
    mun_ocurr = ifelse(as.numeric(mun_ocurr) == 999, NA, mun_ocurr),
    ent_resid = ifelse(as.numeric(ent_resid) == 99, NA, ent_resid),
    mun_resid = ifelse(as.numeric(mun_resid) == 999, NA, mun_resid),
    loc_ocurr = ifelse(as.numeric(loc_ocurr) %in% c(7777, 9999), NA, loc_ocurr),
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      T ~ NA_character_),
    edad = case_when(
      as.numeric(edad) < 4000 ~ 0,
      as.numeric(edad) %in% 4001:4120 ~ as.numeric(edad) - 4000,
      T ~ NA_real_),
    dia_ocurr = ifelse(as.numeric(dia_ocurr) == 99, NA, dia_ocurr),
    mes_ocurr = ifelse(as.numeric(mes_ocurr) == 99, NA, mes_ocurr),
    anio_ocur = ifelse(as.numeric(anio_ocur) == 9999, NA, anio_ocur),
    escolarida = as.integer(escolarida),
    escolarida = case_when(anio_df < 2012 & escolarida == 1 ~  "Sin escolaridad",
                           anio_df < 2012 & escolarida %in% 2:3 ~ "Preescolar",
                           anio_df < 2012 & escolarida == 4 ~ "Primaria",
                           anio_df < 2012 & escolarida == 5 ~ "Secundaria",
                           anio_df < 2012 & escolarida == 6 ~ "Preparatoria", 
                           anio_df < 2012 & escolarida == 7 ~ "Licenciatura o más",
                           anio_df < 2012 & escolarida %in% c(8,9) ~ NA_character_,
                           anio_df >= 2012 & escolarida == 1 ~ "Sin escolaridad",
                           anio_df >= 2012 & escolarida %in% 2:3 ~ "Preescolar",
                           anio_df >= 2012 & escolarida %in% 4:5 ~ "Primaria",
                           anio_df >= 2012 & escolarida %in% 6:7 ~ "Secundaria",
                           anio_df >= 2012 & escolarida == 8 ~ "Preparatoria",
                           anio_df >= 2012 & escolarida %in% 9:10 ~ "Licenciatura o más", 
                           anio_df >= 2012 & escolarida %in% c(88, 99) ~ NA_character_,
                           T ~ NA_character_),
    edo_civil = case_when(anio_df <= 2003 & edo_civil==1 ~ "Soltero",
                          anio_df <= 2003 & edo_civil==2 ~ "Casado",
                          anio_df <= 2003 & edo_civil==3 ~ "Unión Libre",
                          anio_df <= 2003 & edo_civil %in% 4:5 ~ "Separado o Divorciado",
                          anio_df <= 2003 & edo_civil==6 ~ "Viudo",
                          anio_df <= 2003 & edo_civil==9 ~ NA_character_,
                          anio_df %in% 2004:2011 & edo_civil==1 ~ "Soltero",
                          anio_df %in% 2004:2011 & edo_civil==2 ~ "Viudo",
                          anio_df %in% 2004:2011 & edo_civil==3 ~ "Separado o Divorciado",
                          anio_df %in% 2004:2011 & edo_civil==4 ~ "Unión Libre",
                          anio_df %in% 2004:2011 & edo_civil==5 ~ "Casado",
                          anio_df %in% 2004:2011 & edo_civil %in% c(8, 9) ~ NA_character_,
                          anio_df >= 2012 & edo_civil==1 ~ "Soltero",
                          anio_df >= 2012 & edo_civil %in% c(2,6) ~ "Separado o Divorciado",
                          anio_df >= 2012 & edo_civil==3 ~ "Viudo",
                          anio_df >= 2012 & edo_civil==4 ~ "Unión Libre",
                          anio_df >= 2012 & edo_civil==5 ~ "Casado",
                          anio_df >= 2012 & edo_civil %in% c(8, 9) ~ NA_character_,
                          T ~ NA_character_),
    rel_emba = case_when(
      rel_emba == 1 ~ T,
      rel_emba == 2 ~ F,
      T ~ NA), 
    ocurr_trab = case_when(
      ocurr_trab == 1 ~ T,
      ocurr_trab == 2 ~ F,
      T ~ NA),
    lengua = case_when(
      lengua == 1 ~ T,
      lengua == 2 ~ F,
      T ~ NA),
    afromex = case_when(
      afromex == 1 ~ T,
      afromex == 2 ~ F,
      T ~ NA),
    conindig = case_when(
      conindig == 1 ~ T,
      conindig == 2 ~ F,
      T ~ NA),
    # TODO: checar códigos para 2023
    vio_fami = case_when(
      # vio_fami no está disponible a partir de 2022 --> usar par_agre (parentesco)
      vio_fami == 1 | (anio_df >= 2022 & as.numeric(par_agre) %in% c(1:26, 37:52)) ~ T,
      vio_fami == 2 | (anio_df >= 2022 & as.numeric(par_agre) %in% c(27:36, 53:71)) ~ F,
      T ~ NA),
    algun_parentesco = case_when(
      !is.na(parentesco_descrip) & 
        parentesco_descrip %in% c("Sin parentesco", "Ninguno",
                                  "No aplica") ~ F,
      !is.na(parentesco_descrip) & parentesco_descrip != "No especificado" ~ T,
      T ~ NA),
    nacionalid = case_when(nacionalid==1 ~ "Mexicana",
                             nacionalid==2 ~ "Extranjera",
                             T ~ NA_character_),
    tloc_ocurr = case_when(
      as.numeric(tloc_ocurr) <= 7 ~ "Hasta 19,999 habitantes",
      as.numeric(tloc_ocurr) %in% 8:12 ~ "De 20,000 a 99,999 habitantes",
      as.numeric(tloc_ocurr) %in% 13:17 ~ "Más de 100,000 habitantes",
      T ~ NA_character_),
    loc_ocurr = ifelse(as.numeric(loc_ocurr) == 99, NA, loc_ocurr),
    causa_descrip = tolower(unaccent(causa_descrip)),
    lugar = case_when(
      str_detect(causa_descrip, pat_vivienda) ~ "Vivienda",
      str_detect(causa_descrip, pat_via_publica) ~ "Vía pública",
      str_detect(causa_descrip, pat_otro_lugar) ~ "Otro",
      T ~ NA_character_),
    sexual = case_when(
      str_detect(causa_descrip, pat_sexual) ~ T,
      T ~ NA),
    maltrato = case_when(
      str_detect(causa_descrip, pat_maltrato) ~ T,
      T ~ NA),
    causa_hom = case_when(
      str_detect(causa_descrip, pat_arma_fuego) ~ "Arma de fuego",
      str_detect(causa_descrip, pat_arma_blanca) ~ "Arma blanca",
      str_detect(causa_descrip, pat_ahorcar) ~ "Ahorcamiento",
      str_detect(causa_descrip, pat_veneno) ~ "Envenenamiento",
      str_detect(causa_descrip, pat_fuerza_corporal) ~ "Fuerza corporal",
      !is.na(causa_descrip) ~ "Otro",
      T ~ NA_character_),
    # Todas las defunciones en la base son homicidios
    homicidio = ifelse(tipo_defun == 2, T, F),
    feminicidio = case_when(
      sexo == "Mujer" & 
        (lugar == "Vivienda" |
           sexual |
           vio_fami |
           maltrato |
           algun_parentesco) ~ T,
      T ~ F),
    tipo_hom = case_when(
      feminicidio ~ "Feminicidio",
      homicidio & sexo == "Mujer" ~ "Homicidio de mujer",
      homicidio & sexo == "Hombre" ~ "Homicidio de hombre"),
    derechohab = case_when(
      as.numeric(derechohab) == 1 ~ F,
      as.numeric(derechohab) %in% 2:98 ~ T,
      T ~ NA),
    ocupacion = ifelse(
      str_sub(as.character(ocupacion), -2)  == "99", NA, ocupacion)) %>% 
  # Quitar columnas que por el momento no estamos utilizando
  select(-c(rel_emba, cond_act, par_agre, cve_lengua,
            nacesp_cve, causa_descrip))

# Subir a Drive ====
write_csv(defun_limpio, paste0(tempdir(), "/homicidios_limpios.csv"))

drive_upload(paste0(tempdir(), "/homicidios_limpios.csv"),
             path = drive_out,
             overwrite = T)

file.remove(paste0(tempdir(), "/homicidios_limpios.csv"))

# Escribir hash ====
hash <- digest(defun_limpio, algo = "sha256")

writeLines(hash, paste0(rutas$output, "1_clean_homics.txt"))

# done.
