repository := docker.pkg.github.com/aringeri/gig-guide

define docker-build
	docker build . -f $1.Dockerfile \
		-t aringeri/$1 \
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
cache-from-docker-repository: gig-guide-cache.Dockerfile
	docker build . -f gig-guide-cache.Dockerfile \
		-t aringeri/gig-guide-cache \
		-t $(cache-image) \
		--cache-from=$(cache-image)

.PHONY: push-cache
push-cache:
	docker push $(cache-image)

.PHONY: lib
lib: cache
	$(call docker-build,gig-guide-lib)

.PHONY: spec
spec: lib
	$(call docker-build,gig-guide-spec)

.PHONY: run-specs
run-specs: spec
	docker run aringeri/gig-guide-spec

.PHONY: integration-test
integration-test: spec geocode-stub beat-web-stub
	$(call docker-build,integration-test)

.PHONY: run-integration-tests
run-integration-tests: integration-test
	docker-compose -f integration-test-docker-compose.yml run integration-tests

.PHONY: geocode-stub
geocode-stub: lib
	$(call docker-build,geocode-stub)

.PHONY: beat-web-stub
beat-web-stub: lib
	$(call docker-build,beat-web-stub)

define make-exe
	docker build . -f docker/runtime.Dockerfile \
		--build-arg EXECUTABLE=$1 \
		-t aringeri/$1 \
		-t $(repository)/$1:$(or ${GIT_SHA},latest)
endef

.PHONY: scrape-venues
scrape-venues: lib
	$(call make-exe,scrape-venues)

.PHONY: geocode-venues
geocode-venues: lib
	$(call make-exe,geocode-venues)

.PHONY: push-scrape-venues
push-scrape-venues:
	docker push $(repository)/scrape-venues:$(or ${GIT_SHA},latest)