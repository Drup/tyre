.PHONY: default
default: build

.PHONY: build
build: 
	jbuilder build --dev @install

.PHONY: test
test:
	jbuilder runtest --dev

.PHONY: clean
clean:
	jbuilder clean

.PHONY: doc
doc:
	jbuilder build @doc

NAME=tyre
DOCDIR=.gh-pages

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages git@github.com:Drup/$(NAME).git . \
	)

gh-pages: $(DOCDIR)/.git doc
	cp -r _build/default/_doc/_html/* $(DOCDIR)/doc/dev/
	git -C $(DOCDIR) add --all 
	git -C $(DOCDIR) commit -a -m "gh-page updates"
	git -C $(DOCDIR) push origin gh-pages
