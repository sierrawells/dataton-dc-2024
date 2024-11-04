#
# Authors:     SW
# Maintainers: SW, AF, IS, PB
# Copyright:   2024, Data Cívica, GPL v2 or newer
# ===============================================

.PHONY: all \
	clean_output \
	clean \
	import

all: clean # UPDATE W DESCRIPTIVES 

clean_output:
	-rm -rf import/output/*

clean: import
	cd $@ && make

import: 
	cd $@ && make

# done.

