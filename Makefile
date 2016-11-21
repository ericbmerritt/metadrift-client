PHONY: lint format

lint:
	stack exec hlint src
	stack exec hlint app

format:
	find ./src -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;
	find ./app -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;
	find ./test -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;
