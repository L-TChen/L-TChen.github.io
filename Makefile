STACK ?= stack
FORESTER ?= forester
WATCHEXEC ?= watchexec
XSLTPROC ?= xsltproc
STACK_FLAGS ?=

.PHONY: check-tools check-stack check-forester check-watchexec check-xsltproc \
	forest forest-incremental render-forest test hakyll build check clean serve watch

check-tools: check-stack check-forester check-watchexec check-xsltproc

check-stack:
	@command -v "$(STACK)" >/dev/null 2>&1 || { echo "$(STACK) is required but was not found in PATH" >&2; exit 1; }

check-forester:
	@command -v "$(FORESTER)" >/dev/null 2>&1 || { echo "$(FORESTER) is required but was not found in PATH" >&2; exit 1; }

check-watchexec:
	@command -v "$(WATCHEXEC)" >/dev/null 2>&1 || { echo "$(WATCHEXEC) is required but was not found in PATH" >&2; exit 1; }

check-xsltproc:
	@command -v "$(XSLTPROC)" >/dev/null 2>&1 || { echo "$(XSLTPROC) is required but was not found in PATH" >&2; exit 1; }

build: build-forest build-hakyll
	$(MAKE) render-forest

# Forester does not remove files for deleted trees, so always rebuild its
# ignored output directory from scratch.
rebuild-forest: check-forester
	rm -rf forest/output
	cd forest && $(FORESTER) build

# Keep generated files present while Hakyll is watching. Clean builds still use
# the target above so output for deleted trees is removed before deployment.
build-forest: check-forester
	cd forest && $(FORESTER) build

# Pre-render Forester XML after Hakyll has assembled the deployed XSLT tree.
# Read XML from Forester's output so this can be rerun even after the deployed
# XML copies have been removed.
render-forest: check-xsltproc
	@set -eu; \
		stylesheet="$$PWD/_site/posts/default.xsl"; \
		test -f "$$stylesheet"; \
		find forest/output/posts -type f -name 'index.xml' -print | while IFS= read -r xml; do \
			rel=$${xml#forest/output/posts/}; \
			html="_site/posts/$${rel%.xml}.html"; \
			mkdir -p "$$(dirname "$$html")"; \
			"$(XSLTPROC)" --output "$$html" "$$stylesheet" "$$xml"; \
			rm -f "_site/posts/$$rel"; \
		done

test: check-stack
	$(STACK) test $(STACK_FLAGS)

build-hakyll: check-stack
	$(STACK) build $(STACK_FLAGS)
	$(STACK) build $(STACK_FLAGS) --exec "site rebuild"

check: check-stack test
	$(STACK) build $(STACK_FLAGS) --exec "site check --internal-links"

clean: check-stack
	$(STACK) build $(STACK_FLAGS) --exec "site clean"
	rm -rf forest/output

watch: check-watchexec build
	@set -eu; \
		$(WATCHEXEC) --project-origin . --postpone --on-busy-update=queue \
			--watch forest/trees --watch forest/assets \
			--watch forest/forest.toml --watch forest/theme -- \
			$(MAKE) build-forest FORESTER="$(FORESTER)" & \
		forester_watch_pid=$$!; \
		$(WATCHEXEC) --project-origin . --postpone --on-busy-update=queue \
			--watch _site/posts --exts xml,xsl -- \
			$(MAKE) render-forest XSLTPROC="$(XSLTPROC)" & \
		render_watch_pid=$$!; \
		trap 'kill "$$forester_watch_pid" "$$render_watch_pid" 2>/dev/null || true; wait "$$forester_watch_pid" 2>/dev/null || true; wait "$$render_watch_pid" 2>/dev/null || true' EXIT INT TERM; \
		$(STACK) build $(STACK_FLAGS) --exec "site watch"
