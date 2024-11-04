#
# Authors:     SW
# Maintainers: SW, AF, IS, PB
# Copyright:   2024, Data Cívica, GPL v2 or newer
# ===============================================

.PHONY: all \
	clean

all: output/1_clean_homics.txt \
	output/2_clean_pob.txt

clean: 
	rm -rf output/*

output/1_clean_homics.txt: src/1_clean_homics.R \
	hand/patrones_causas.yaml \
	input/2_importar_homics.txt
	Rscript --vanilla $<

output/2_clean_pob.txt: src/2_clean_pob.R
	Rscript --vanilla $<

# done.

