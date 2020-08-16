include config.mk

HS_SOURCES=$(shell find server/lib/ server/src -name '*.hs')
ELM_SOURCES=$(shell find client/src -name '*.elm')
ELM_MESSAGES_SOURCE=client/src/Messages.elm
STACK_BIN=$(shell cd server && stack path --local-install-root)/bin

.PHONY: build
build: server/assets/client.js \
	server/assets/style.css \
	server/assets/black.txt \
	server/assets/white.txt

.PHONY: docker
docker:
	docker build -t jaspervdj/uplcg .
	docker push jaspervdj/uplcg

.PHONY: server
server: build
	(cd server && \
	    UPLCG_HOSTNAME=$(UPLCG_HOSTNAME) \
	    UPLCG_PORT=$(UPLCG_PORT) \
	    UPLCG_BASE=$(UPLCG_BASE) \
	    stack exec uplcg-server)

.PHONY: stack_build
stack_build: $(HS_SOURCES)
	(cd server && stack build)

$(ELM_MESSAGES_SOURCE): stack_build
	(cd server && stack exec uplcg-generate-elm-types) >$(ELM_MESSAGES_SOURCE)

server/assets/client.js: $(ELM_MESSAGES_SOURCE) $(ELM_SOURCES)
	mkdir -p server/assets
	cd client && elm make src/Client.elm --optimize --output=../$@

server/assets/style.css: client/style.css
	cp $< $@

server/assets/black.txt: black.txt
	cp $< $@

server/assets/white.txt: white.txt
	cp $< $@
