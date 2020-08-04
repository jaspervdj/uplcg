include config.mk

HS_SOURCES=$(shell find server/lib/ server/src -name '*.hs')
ELM_SOURCES=$(shell find client/src -name '*.elm')
ELM_MESSAGES_SOURCE=client/src/Messages.elm
STACK_BIN=$(shell cd server && stack path --local-install-root)/bin

.PHONY: build
build: server/assets/client.js \
	server/assets/client.html \
	server/assets/style.css \
	server/assets/black.txt \
	server/assets/white.txt

.PHONY: docker
docker:
	docker build -t jaspervdj/cafp .
	docker push jaspervdj/cafp

.PHONY: server
server: build
	(cd server && \
	    CAFP_HOSTNAME=$(CAFP_HOSTNAME) \
	    CAFP_PORT=$(CAFP_PORT) \
	    CAFP_BASE=$(CAFP_BASE) \
	    stack exec cafp-server)

.PHONY: stack_build
stack_build: $(HS_SOURCES)
	(cd server && stack build)

$(ELM_MESSAGES_SOURCE): stack_build
	(cd server && stack exec cafp-generate-elm-types) >$(ELM_MESSAGES_SOURCE)

server/assets/client.js: $(ELM_MESSAGES_SOURCE) $(ELM_SOURCES)
	mkdir -p server/assets
	cd client && elm make src/Client.elm --optimize --output=../$@

server/assets/client.html: client/index.html config.mk
	sed "s@\$$CAFP_BASE@$(CAFP_BASE)@" $< >$@

server/assets/style.css: client/style.css
	cp $< $@

server/assets/black.txt: black.txt
	cp $< $@

server/assets/white.txt: white.txt
	cp $< $@
