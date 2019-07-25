
compiler: src/Doc.hs src/haskellEd.hs
	cd src && $(MAKE)
	echo '#!/bin/sh\nsrc/haskellEd' > hed
	chmod u+x hed
	echo 'Done'
clean:
	cd src && $(MAKE) clean
	rm hed *.dat

.PHONY: compliler
