CABAL ?= cabal
PROTO_COMPILE_HS = ~/.cabal/bin/compile-proto-file

grpc-haskell:
	($(CABAL) build proto3-suite && mkdir -p ~/.cabal/bin && \
		$(CABAL) exec which compile-proto-file | tail -1 | xargs -I{} cp {} $(PROTO_COMPILE_HS))
	($(PROTO_COMPILE_HS) \
		--includeDir /usr/local/include \
		--includeDir ../proto/helloworld \
		--proto helloworld.proto \
		--out ./src-gen/)
