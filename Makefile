STACK ?= stack
FORESTER ?= forester
WATCHEXEC ?= watchexec
STACK_FLAGS ?=

.PHONY: forest forest-incremental test hakyll build check clean serve watch

build: build-forest build-hakyll

# Forester does not remove files for deleted trees, so always rebuild its
# ignored output directory from scratch.
rebuild-forest:
	rm -rf forest/output
	cd forest && $(FORESTER) build

# Keep generated files present while Hakyll is watching. Clean builds still use
# the target above so output for deleted trees is removed before deployment.
build-forest:
	cd forest && $(FORESTER) build

test:
	$(STACK) test $(STACK_FLAGS)

build-hakyll:
	$(STACK) build $(STACK_FLAGS)
	$(STACK) build $(STACK_FLAGS) --exec "site rebuild"

check: test
	$(STACK) build $(STACK_FLAGS) --exec "site check --internal-links"

clean:
	$(STACK) build $(STACK_FLAGS) --exec "site clean"
	rm -rf forest/output

watch: build
	@command -v "$(WATCHEXEC)" >/dev/null || { echo "watchexec is required for make watch" >&2; exit 1; }
	@set -eu; \
		$(WATCHEXEC) --project-origin . --postpone --on-busy-update=queue \
			--watch forest/trees --watch forest/assets \
			--watch forest/forest.toml --watch forest/theme -- \
			$(MAKE) build-forest FORESTER="$(FORESTER)" & \
		forester_watch_pid=$$!; \
		trap 'kill "$$forester_watch_pid" 2>/dev/null || true; wait "$$forester_watch_pid" 2>/dev/null || true' EXIT INT TERM; \
		$(STACK) build $(STACK_FLAGS) --exec "site watch"
