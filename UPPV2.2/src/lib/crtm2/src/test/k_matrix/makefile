#==============================================================================
#
# Makefile for JCSDA CRTM K_Matrix Test
#
# $Id:$
#
#==============================================================================

include ../../make.macros


all clean update:
	@echo
	@EXAMPLE_DIR=`ls -d Example*_*` && \
	for EXAMPLE in $$EXAMPLE_DIR; do \
	  cd $$EXAMPLE && \
	  $(MAKE) -s $@ && \
	  cd ..; \
	done || exit 1
	@if [ "$@" = "all" ]; then \
	  ../run_test.sh k-matrix; \
	fi
	@echo

include ../../make.rules
