HS_SOURCES=$(shell find server/lib/ server/src -name '*.hs')
ELM_SOURCES=$(shell find client/src -name '*.elm')
ELM_MESSAGES_SOURCE=client/src/Messages.elm

.PHONY: build
build: server/assets/client.js server/assets/client.html

server: build
	cd server && stack exec cafp-server

.PHONY: stack_build
stack_build: $(HS_SOURCES)
	(cd server && stack build)

$(ELM_MESSAGES_SOURCE): stack_build
	(cd server && stack exec cafp-generate-elm-types) >$(ELM_MESSAGES_SOURCE)

server/assets/client.js: $(ELM_MESSAGES_SOURCE) $(ELM_SOURCES)
	mkdir -p server/assets
	cd client && elm make src/Client.elm --output=../server/assets/client.js

server/assets/client.html: client/index.html
	mkdir -p server/assets
	cp client/index.html $@
