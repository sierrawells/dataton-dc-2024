#
# Authors:     SW
# Maintainers: SW, AF, IS, PB
# Copyright:   2024, Data Cívica, GPL v2 or newer
# ===============================================

.PHONY: all \
	clean \

all: output/2_importar_homics.txt

clean: 
	rm -rf output/*

# Last step to first step 
output/2_importar_homics.txt: src/2_importar_homics.R \
	hand/vars_para_importar.yaml \
	hand/vars_renombrar.yaml \
	output/1_descargar_bases.txt
	Rscript --vanilla $<

output/1_descargar_bases.txt: src/1_descargar_bases.R \
	hand/anios_para_importar.yaml \
	hand/tablas_para_importar.yaml
	Rscript --vanilla $<

# done.

