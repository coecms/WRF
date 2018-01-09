#==============================================================================
#
# Makefile for JCSDA CRTM Test
#
# $Id:$
#
#==============================================================================

include ../make.macros


all: forward/all k_matrix/all

test: forward/test k_matrix/test

clean: forward/clean k_matrix/clean

update: forward/update k_matrix/update

forward/all     \
forward/test    \
forward/clean   \
forward/update  \
k_matrix/all    \
k_matrix/test   \
k_matrix/clean  \
k_matrix/update \
:
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) -s SUBDIR=$$subdir TARGET=$$target subdir_target


subdir_target:
	@echo
	@cd $(SUBDIR) && \
	    echo "Making \`$(TARGET)' in directory `pwd`" && \
	    echo && \
	    $(MAKE) -s $(TARGET) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

include ../make.rules
