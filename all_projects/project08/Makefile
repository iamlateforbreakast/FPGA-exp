SUBDIRS = spi_hardcaml

all: $(SUBDIRS)

$(SUBDIRS):
	echo "Building " $@
	$(MAKE) -C $@ $(MAKECMDGOALS)

$(SUBDIRS)_clean:
	echo "Cleaning " $@
	$(MAKE) -C $(SUBDIRS) clean

clean: $(SUBDIRS)_clean

.PHONY: all clean $(SUBDIRS) $(SUBDIRS)_clean
