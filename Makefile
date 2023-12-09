repository := ghcr.io/aringeri/gig-guide
local_img_prefix := aringeri/gig-guide

define docker-build
	docker build . -f docker/$1.Dockerfile \
		-t $(local_img_prefix)/$1 \
		-t $(repository)/$1
endef

.PHONY: cache
cache:
	$(call docker-build,gig-guide-cache)

cache-image := $(repository)/gig-guide-cache:latest

.PHONY: pull-cache
pull-cache:
	docker pull $(cache-image)

.PHONY: cache-from-docker-repository
cache-from-docker-repository:
	docker build . -f docker/gig-guide-cache.Dockerfile \
		-t $(local_img_prefix)/gig-guide-cache \
		-t $(cache-image) \
		--cache-from=$(cache-image)

.PHONY: push-cache
push-cache:
	docker push $(cache-image)

.PHONY: lib
lib: cache
	$(call docker-build,gig-guide-lib)

define make-exe
	docker build . -f docker/runtime.Dockerfile \
		--build-arg EXECUTABLE=$1 \
		-t $(local_img_prefix)/$1 \
		-t $(repository)/$1:$(or ${GIT_SHA},latest)
endef

.PHONY: scrape-venues
scrape-venues: lib
	$(call make-exe,scrape-venues)

.PHONY: scrape-events
scrape-events: lib
	$(call make-exe,scrape-events)

.PHONY: run-merge-overrides
run-merge-overrides:
	cabal run merge-overrides -- -r data/scraped/raw-venues.json \
		-g data/geocoded/first-pass/geocoded-venues.json \
		-i data/overrides/override-venues.json \
		-o data/geocoded/second-pass/geocoded-venues.json

.PHONY: elm-ui
elm-ui:
	cd ui/elm && \
	elm make --optimize --output=../../docs/main.js src/GigGuide.elm

.PHONY: elm-ui-dist
elm-ui-dist: elm-ui
	uglifyjs docs/main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
		| uglifyjs --mangle -o docs/main.js

.PHONY: run-local-ui-server
run-local-ui-server:
	python3 -m http.server 8080 -d docs/