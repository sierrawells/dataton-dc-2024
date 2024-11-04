# SINAIS updates

## Descripción

Este repositorio está pensado para actualizar los datos de defunciones del SINAIS de forma automatizada, con particular enfoque en generar visualizaciones y sacar cifras respecto a las defunciones presuntas como homicidios.

El repositorio contiene scripts y datos organizados en tre principales directorios: `import`, `clean` y `descriptives`.

## Directorios principales

* `import`: Descarga los datos crudos de la página del INEGI, selecciona las variables de interés y junta las tablas y los años.
* `clean`: Limpia y transforma los valores (y algunos nombres) de las variables importadas
* `descriptives`: Genera visualizaciones y figuras descriptivas finales.

### Organización de los directorios principales

* Los tres directorios principales pueden contener algunos o todos de los siguientes subdirectorios o archivos.
* `src`: scripts utilizados para generar los datos y/o visualizaciones de `output`
  * En el caso de `import`, los scripts se deben correr en el orden indicado por el número al inciio de su nombre (`src/1_descagar_bases.R` y después `src/2_importar_homics.R`).
  * En el caso de `descriptives`, los números al inicio del nombre de cada script corresponden a los número al inicio de los nombres de cada visualización que genera (ej. `output/1_tasa_viol_letal.png` se genera en `src/1_tendencias_nacional.R`).
* `output`: visualizaciones y hashes de datos generados por los scripts de `src`
  * Los archivos `.txt` en los directorios `output` representan hashes de los datos generados en el script del mismo nombre. Los datos se guardan en Drive para ahorrar almacenamiento, pero los hashes funcionan como una "huella" única que nos permite saber si algo cambió en el contenido de los datos generados.
* `input`: atajos a los hashes de datos (guardados en el directorio `output` del proceso anterior) que se usarán como insumos en `src`
  * Por ejemplo: `clean/input` contiene atajos (técnicamente conocidos commo "symlinks") para `import/output`, ya que los datos generados por `import` sirven como insumos para `clean`
* `hand`: diccionarios o otros archivos utilizados en los scripts de `src`
* `Makefile`: archivos que sirven para detectar cambios a scripts o insumos y actualizar de forma automática los datos generados en ese subdirectorio
  * Para correr un `Makefile`, ejecuta el comando `make` o `make all` desde el directorio de ese proceso (ej. `import`, `clean`, etc.)
* **OJO**: los archivos cuyos nombres contienen `inst_res` respectan al análisis sobre muertes en instituciones residenciales. Este análisis se realizará en colaboración con la organización Disability Rights International. Los archivos cuyos nombres contienen `homics` respectan al análisis sobre homicidios más ampliamente, en cualquier lugar.

## Actualización de datos

Para agregar otro año a los datos de este repositorio e incorporarlo al proceso de importación, limpieza y visualización, se deben seguir los siguientes pasos.

1. Agregar el año al archivo `import/hand/anios_para_importar.yaml`
2. Revisar la documentación del INEGI para la base de este año para determinar si se han cambiado los códigos utilizados en los valores de las variables. Hay que prestar particular atención a:
  * **Parentesco** (estos valores se encontrarán en el catálogo de parentesco)
  * **Escolaridad**
  * **Estado civil**

Si sí hubo algún cambio a estos valores, habrá que editar el archivo `clean/src/1_clean_homics.R`.

3. Desde el directorio de inicio del proyecto de Git (ej. `~/git/sinais_updates`), correr `make clean_output`. Este paso es opcional pero preferible, ya que borra todos los datos y visualizaciones generados anteriormente que no incorporaban el año que buscamos agregar.
4. Desde el directorio de inicio del proyecto de Git, correr `make` o `make all` (estos comandos hacen la misma cosa).

## Licencia

Data Cívica 2024 ©

Para dudas sobre el contenido de este reposito, favor de contactar a Sierra Wells en sierra.wells@datacivica.org.

<!-- done -->
