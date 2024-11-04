#
# Authors:     SW
# Maintainers: SW, AF, IS, PB
# Copyright:   2024, Data Cívica, GPL v2 or newer
# ===============================================

.PHONY: all \
	clean

all: output/1_fiebre_viol_letal.svg \
	output/2_tasa_viol_letal_ent.svg \
	output/3_fiebre_area_dist_edad.svg \
	output/4_area_dist_mod_sexo.svg \
	output/5_vars_na.svg

clean: 
	rm -rf output/*

output/1_fiebre_viol_letal.svg: src/1_tendencias_nacional.R \
	input/1_clean_homics.txt
	Rscript --vanilla $<

output/2_tasa_viol_letal_ent.svg: src/2_ents_munis.R \
	input/1_clean_homics.txt
	Rscript --vanilla $<

output/3_fiebre_area_dist_edad.svg: src/3_edad.R \
	input/1_clean_homics.txt
	Rscript --vanilla $<

output/4_area_dist_mod_sexo.svg: src/4_modalidad.R \
	input/1_clean_homics.txt
	Rscript --vanilla $<

output/5_vars_na.svg: src/5_vars_na.R \
	input/1_clean_homics.txt
	Rscript --vanilla $<

# done.

