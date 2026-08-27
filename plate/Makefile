# Plate — build the specimen page.
#
# Run from the workspace root (`make -C plate`) or from here; both work,
# because everything below is relative to the workspace root and this recipe
# changes to it first.

ROOT    := $(shell cd $(dir $(lastword $(MAKEFILE_LIST)))/.. && pwd)
MACHINES = core/machines plate/machines
OUT      = site/index.html
FRAGMENT = site/artifact.html

export PLATE_ASSETS = plate/assets
export PLATE_BUILT  = $(shell date +'%-d %B %Y')

SOURCES = $(wildcard plate/src/Plate/*.purs) plate/assets/plate.css plate/assets/plate.js
INPUTS  = $(wildcard core/machines/*.json) $(wildcard plate/machines/*.json)

.PHONY: all artifact open clean

all: $(OUT)

$(OUT): $(addprefix $(ROOT)/,$(SOURCES)) $(addprefix $(ROOT)/,$(INPUTS))
	cd $(ROOT) && mkdir -p site && spago run -p glassbox-plate --quiet -- $(OUT) $(MACHINES)

# The same page with no html/head/body of its own, for a host that wraps it.
artifact:
	cd $(ROOT) && mkdir -p site && PLATE_FRAGMENT=1 \
	  spago run -p glassbox-plate --quiet -- $(FRAGMENT) $(MACHINES)

open: all
	open $(ROOT)/$(OUT)

clean:
	rm -f $(ROOT)/$(OUT) $(ROOT)/$(FRAGMENT)
