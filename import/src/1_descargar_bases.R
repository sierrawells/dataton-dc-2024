#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Paquetería
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, yaml, googledrive, ggrepel, digest)

options(timeout = 600)
# Archivos locales
rutas <- list(
  anios = here("import/hand/anios_para_importar.yaml"),
  tablas = here("import/hand/tablas_para_importar.yaml"),
  output = here("import/output/"))

# Definir carpetas de Drive
drive_auth(email = "sierra.wells@datacivica.org")

drive_out_id <- as_id("1nfUnukWNpM1Ft0viVhjNxJ5ODfI1JsDd")
drive_out <- drive_ls(drive_out_id)

# Leer lista de anios y tablas para importar
anios_para_bajar <- read_yaml(rutas$anios) %>% 
  # Convertir a vector de caracteres
  map_chr(as.character)

tablas_para_bajar <- read_yaml(rutas$tablas) %>%
  map_chr(as.character)

# ITERAR SOBRE LOS AÑOS PARA IMPORTAR (DESCARGAS DE UN SOLO AÑO: A PARTIR DE 2020) ====
anios_uno_solo <- anios_para_bajar[!str_detect(anios_para_bajar, "_")]

for (anio in anios_uno_solo) {
  
  # Checar si ese año ya está en Drive
  anio_ya_en_drive <- all(paste0(tablas_para_bajar, anio, ".dbf")
                          %in% drive_out$name)
  
  if (anio_ya_en_drive) {
    message(paste0(anio, " ya está en Drive"))
    next
  }
  
  # Si ese año no está en Drive:
  
  # Bajar el archivo .zip de la página del INEGI
  url <- paste0("https://www.inegi.org.mx/contenidos/programas/edr/microdatos/defunciones/",
                anio, "/defunciones_base_datos_", anio, "_dbf.zip")
  
  message(paste0("Descargando ", anio))
  download.file(url, paste0(tempdir(), "/tempo.zip"))
  
  # Descomprimir el archivo .zip
  message(paste0("Descomprimiendo ", anio))
  unzip(paste0(tempdir(), "/tempo.zip"), exdir = paste0(tempdir(), "/tempo"))
  
  # Definir tablas de interés para ese año
  anio_abrev <- str_remove(anio, "20")
  
  tablas_anio <- tablas_para_bajar %>% 
    str_replace("DEFUN", paste0("DEFUN", anio_abrev))
  
  # Checar que las tablas de interés estén en el directorio descomprimido
  archivos_tempo <- list.files(paste0(tempdir(), "/tempo"), full.names = TRUE)
  
  # Para cada tabla de interés...
  for (tabla in tablas_anio) {
    # Checar que la tabla esté en el directorio que descargamos
    message(paste0("Checando ", tabla, " para ", anio))
    stopifnot(any(str_detect(toupper(archivos_tempo), tabla)))
    
    # Cambiar nombre de la tabla
    tabla_nombre_orig <- archivos_tempo[str_detect(toupper(archivos_tempo), tabla)]
    
    tabla_nuevo_nombre <- str_remove_all(tabla_nombre_orig, ".dbf") %>%
      str_remove_all(".DBF") %>%
      str_remove_all("\\d+$") %>%
      str_replace("(?<=tempo/)(.*)", function(x) toupper(x)) %>%
      paste0(anio, ".dbf")
    
    file.rename(tabla_nombre_orig, tabla_nuevo_nombre)
    
    # Subir la tabla a Drive
    drive_upload(tabla_nuevo_nombre,
                 path = drive_out_id, 
                 overwrite = TRUE)
    }
    
  message(paste0("Borrando ", anio))
  file.remove(paste0(tempdir(), "/tempo.zip"))
  unlink(paste0(tempdir(), "/tempo"), recursive = TRUE)
  }

# ITERAR SOBRE LOS AÑOS PARA IMPORTAR (DESCARGAS DE VARIOS AÑOS: HASTA 2019) ====
anio_sets <- anios_para_bajar[str_detect(anios_para_bajar, "_")]

min_anio <- anio_sets %>% 
  str_split("_") %>%
  unlist() %>% 
  as.numeric() %>% 
  min()

for (anio_set in anio_sets) {
  anio_rango <- as.numeric(str_split(anio_set, "_")[[1]][1]):as.numeric(str_split(anio_set, "_")[[1]][2]) %>% 
    as.character()
  
  # Checar si esos años ya están en Drive
  tablas_para_checar <- expand.grid(tablas_para_bajar, anio_rango) %>%
    apply(1, function(x) paste0(x[1], x[2])) %>% 
    .[!. %in% paste0("PARENTESCO", min_anio:2011)] %>% 
    paste0(".dbf")
  
  anios_ya_en_drive <- all(tablas_para_checar %in% drive_out$name)
  
  if (anios_ya_en_drive) {
    message(paste0(anio_set, " ya está en Drive"))
    next
  }
  
  # Si esos años no están en Drive:
  
  # Bajar el archivo .zip de la página del INEGI
  url <- paste0("https://www.inegi.org.mx/contenidos/programas/edr/microdatos/defunciones/datos/defunciones_generales_base_datos_",
  anio_set, "_dbf.zip")
  
  message(paste0("Descargando ", anio_set))
  download.file(url, paste0(tempdir(), "/tempo.zip"))
  
  # Descomprimir el archivo .zip
  message(paste0("Descomprimiendo ", anio_set))
  unzip(paste0(tempdir(), "/tempo.zip"), exdir = paste0(tempdir(), "/tempo"))
  
  # Iterar sobre cada año de esa descarga
  for (anio in anio_rango){
    # Definir archivos en el directorio para ese año
    dir_tempo <- list.files(paste0(tempdir(), "/tempo"), full.names = TRUE) %>%
      .[str_detect(., anio)]
    dir_tempo_nombre_base <- paste0(basename(dir_tempo), "/")
    
    stopifnot(length(dir_tempo) == 1)
    
    archivos_dir <- list.files(dir_tempo, full.names = TRUE)
    
    # Definir tablas de interés para ese año
    anio_abrev <- str_remove(anio, "20")
    
    tablas_anio <- tablas_para_bajar %>% 
      str_replace("DEFUN", paste0("DEFUN", anio_abrev))
    
    if(as.numeric(anio) <= 2011){
      # Tabla de parentesco no existe antes de 2012
      tablas_anio <- tablas_anio %>% 
        .[. !="PARENTESCO"]
    }
    
    for (tabla in tablas_anio) {
      # Checar que la tabla esté en el directorio
      message(paste0("Checando ", tabla, " para ", anio))
      stopifnot(any(str_detect(toupper(archivos_dir), tabla)))
      
      # Cambiar nombre de la tabla
      tabla_nombre_orig <- archivos_dir[str_detect(toupper(archivos_dir), tabla)]
      
      tabla_nuevo_nombre <- str_remove(tabla_nombre_orig, dir_tempo_nombre_base) %>%
        str_remove_all(".DBF") %>%
        str_remove_all(".dbf") %>%
        str_remove_all("\\d+$") %>%
        str_replace("(?<=tempo/)(.*)", function(x) toupper(x)) %>%
        paste0(anio, ".dbf")
      
      file.rename(tabla_nombre_orig, tabla_nuevo_nombre)
      
      # Subir la tabla a Drive
      drive_upload(tabla_nuevo_nombre,
                   path = drive_out_id, 
                   overwrite = TRUE)
    }
  }
  
  message(paste0("Borrando ", anio_set))
  file.remove(paste0(tempdir(), "/tempo.zip"))
  unlink(paste0(tempdir(), "/tempo"), recursive = TRUE)

}

# Escribir hash ====
# Aquí escribimos el hash para dejar registro de si el output de este script cambió
# El output solo debería cambiar si cambiamos los años o tablas que buscamos bajar
objeto_para_hash <- list(
  anios_para_bajar = anios_para_bajar,
  tablas_para_bajar = tablas_para_bajar)

hash <- digest(objeto_para_hash, algo = "sha256")
writeLines(hash, paste0(rutas$output, "1_descargar_bases.txt"))

# done.
