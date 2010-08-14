PACKAGE_NAME := incf-cl
PACKAGE_VERSION := $(shell date +%Y%m%d)
PACKAGE_TARBALL := $(PACKAGE_NAME).tar.gz

SOURCES := $(shell git ls-files)

.PHONY: release tarball clean

release: index.html tarball
	scp index.html $(PACKAGE_TARBALL) $(PACKAGE_TARBALL).asc \
		jmbellorivas@superadditive.com:html/software/incf-cl/

index.html: incf-cl.org
	emacs --batch \
		--load=$(HOME)/.emacs.d/elpa/htmlize-1.37/htmlize.elc \
		--eval "(setq org-export-html-style-include-default nil)" \
		--eval "(setq org-export-htmlize-output-type 'css)" \
		--visit=$^ --funcall org-export-as-html-batch
	mv incf-cl.html index.html

tarball: $(PACKAGE_TARBALL) $(PACKAGE_TARBALL).asc

$(PACKAGE_TARBALL): $(SOURCES)
	git archive --verbose \
		--format tar --output $(PACKAGE_NAME).tar \
		--prefix $(PACKAGE_NAME)-$(PACKAGE_VERSION)/ HEAD
	gzip --force --best $(PACKAGE_NAME).tar

$(PACKAGE_TARBALL).asc: $(PACKAGE_TARBALL)
	gpg --armor --detach $(PACKAGE_TARBALL)

clean:
	rm -f $(PACKAGE_TARBALL) $(PACKAGE_TARBALL).asc
	rm -f *.fasl *.fas *.lib *.o *~
	rm index.html
