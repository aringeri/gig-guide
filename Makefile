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