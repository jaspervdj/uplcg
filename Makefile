HS_SOURCES=$(shell find server/lib/ server/src -name '*.hs')
ELM_MESSAGES_MODULE=client/src/Messages.elm

.PHONY: build
build:
	cd server && stack build

server: build
	cd server && stack exec cafp-server

$(ELM_MESSAGES_MODULE): $(HS_SOURCES)
	(cd server && stack exec cafp-generate-elm-types) >$(ELM_MESSAGES_MODULE)
