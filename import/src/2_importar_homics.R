#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, yaml, googledrive, janitor, sf, digest)

# Archivos locales
rutas <- list(
  vars_importar = here("import/hand/vars_para_importar.yaml"),
  vars_renombrar = here("import/hand/vars_renombrar.yaml"),
  output = here("import/output/"))

# Definir carpetas de Drive
drive_auth(email = "sierra.wells@datacivica.org")

drive_in <- drive_ls(as_id("1nfUnukWNpM1Ft0viVhjNxJ5ODfI1JsDd"))
drive_out <- as_id("1VqSBZsIn2g9BJ3iTE2L4g7sMBWK2zjrK")

# Leer lista de variables para importar
vars_all <- read_yaml(rutas$vars_importar)$all # A fuerza deben estar todos los años
vars_any <- read_yaml(rutas$vars_importar)$any # Solo aplican para ciertos años

# Leer lista de variables para renombrar
vars_renombrar_defun <- read_yaml(rutas$vars_renombrar)[["defun"]] %>% 
  unlist()

vars_renombrar_catminde <- read_yaml(rutas$vars_renombrar)[["catminde"]] %>%
  unlist()

vars_renombrar_parentesco <- read_yaml(rutas$vars_renombrar)[["parentesco"]] %>%
  unlist()

# Leer tablas crudas de Drive ====
for (i in seq_len(nrow(drive_in))){
  nombre <- drive_in$name[i]
  id <- drive_in$id[i]
  
  drive_download(
    as_id(id),
    paste0(tempdir(), "/", nombre),
    overwrite = TRUE)
  
  guess_encoding(paste0(tempdir(), "/", nombre))
  
  tempo <- st_read(paste0(tempdir(), "/", nombre), quiet = TRUE)

  nuevo_nombre <- nombre %>% 
    tolower() %>% 
    str_remove(".dbf")
  
  assign(nuevo_nombre, tempo)
  
  file.remove(paste0(tempdir(), "/", nombre))
                 
}

# Crear lista de tablas para cada año ====
anios <- drive_in$name %>% 
  map_chr(~str_extract(.x, "\\d{4}")) %>% 
  unique()

tablas_por_anio <- list()

for (anio in anios){
  defun <- get(paste0("defun", anio))
  catminde <- get(paste0("catminde", anio))
  
  tablas_por_anio[[anio]] <- list(
    defun = defun,
    catminde = catminde
  )
  
  # Tabla de parentesco solo existe a partir de 2012
  if (as.numeric(anio) > 2011){
    parentesco <- get(paste0("parentesco", anio))
    
    tablas_por_anio[[anio]]$parentesco <- parentesco
  }
  
}

# Juntar tablas + filtrar para homicidios en cada año ====
tablas_juntas_por_anio <- list()

for (anio in names(tablas_por_anio)){
  message(paste0("Juntando tablas para ", anio))
  
  tablas_juntas_anio <- tablas_por_anio[[anio]]$defun %>% 
    clean_names() %>% 
    rename(any_of(vars_renombrar_defun)) %>% 
    select(all_of(vars_all),
           any_of(vars_any)) %>% 
    # QUEDAR ÚNICAMENTE HOMICIDIOS !!!
    filter(as.numeric(tipo_defun) == 2) %>% 
    left_join(
      tablas_por_anio[[anio]]$catminde %>% 
        clean_names() %>% 
        rename(any_of(vars_renombrar_catminde)),
      by = c("causa_def" = "cve"))
  
  if (as.numeric(anio) > 2011){
    tablas_juntas_anio <- tablas_juntas_anio %>% 
      left_join(
        tablas_por_anio[[anio]]$parentesco %>% 
          clean_names() %>% 
          mutate(clave = as.numeric(clave)) %>%
          rename(any_of(vars_renombrar_parentesco)),
        by = c("par_agre" = "clave"))
  }
  
  tablas_juntas_por_anio[[anio]] <- tablas_juntas_anio %>% 
    mutate(across(everything(), as.character))
}

# Juntar anios ====
homic_anios_juntos <- bind_rows(tablas_juntas_por_anio, .id = "anio_df")

# Subir a Drive ====
write_csv(homic_anios_juntos, paste0(tempdir(), "/homicidios_importados.csv"))

drive_upload(paste0(tempdir(), "/homicidios_importados.csv"),
             path = drive_out,
             overwrite = T)

file.remove(paste0(tempdir(), "/homicidios_importados.csv"))

# Escribir hash ====
hash <- digest(homic_anios_juntos, algo = "sha256")

writeLines(hash, paste0(rutas$output, "2_importar_homics.txt"))

# done.
  
