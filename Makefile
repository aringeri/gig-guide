
.PHONY: cache
cache: gig-guide-cache.Dockerfile
	docker build . -f gig-guide-cache.Dockerfile -t aringeri/gig-guide-cache

.PHONY: lib
lib: cache gig-guide-lib.Dockerfile
	docker build . -f gig-guide-lib.Dockerfile -t aringeri/gig-guide-lib

.PHONY: spec
spec: lib gig-guide-spec.Dockerfile
	docker build . -f gig-guide-spec.Dockerfile -t aringeri/gig-guide-spec

.PHONY: integration-test
integration-test: spec geocode-stub beat-web-stub integration-test.Dockerfile
	docker build . -f integration-test.Dockerfile -t aringeri/gig-guide-integration-test

.PHONY: geocode-stub
geocode-stub: lib geocode-stub.Dockerfile
	docker build . -f geocode-stub.Dockerfile -t aringeri/geocode-stub

.PHONY: beat-web-stub
beat-web-stub: lib beat-web-stub.Dockerfile
	docker build . -f beat-web-stub.Dockerfile -t aringeri/beat-web-stub

define make-exe
	docker build . -f docker/runtime.Dockerfile --build-arg EXECUTABLE=$1 -t aringeri/$1
endef

.PHONY: scrape-venues
scrape-venues: lib docker/runtime.Dockerfile
	$(call make-exe,scrape-venues)

.PHONY: geocode-venues
geocode-venues: lib docker/runtime.Dockerfile
	$(call make-exe,geocode-venues)