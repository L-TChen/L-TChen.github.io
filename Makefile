STACK ?= stack
FORESTER ?= forester
WATCHEXEC ?= watchexec
XSLTPROC ?= xsltproc
STACK_FLAGS ?=

.PHONY: forest forest-incremental render-forest test hakyll build check clean serve watch

build: build-forest build-hakyll
	$(MAKE) render-forest

# Forester does not remove files for deleted trees, so always rebuild its
# ignored output directory from scratch.
rebuild-forest:
	rm -rf forest/output
	cd forest && $(FORESTER) build

# Keep generated files present while Hakyll is watching. Clean builds still use
# the target above so output for deleted trees is removed before deployment.
build-forest:
	cd forest && $(FORESTER) build

# Pre-render Forester XML after Hakyll has assembled the deployed XSLT tree.
# Read XML from Forester's output so this can be rerun even after the deployed
# XML copies have been removed.
render-forest:
	@command -v "$(XSLTPROC)" >/dev/null || { echo "xsltproc is required for make render-forest" >&2; exit 1; }
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
		$(WATCHEXEC) --project-origin . --postpone --on-busy-update=queue \
			--watch _site/posts --exts xml,xsl -- \
			$(MAKE) render-forest XSLTPROC="$(XSLTPROC)" & \
		render_watch_pid=$$!; \
		trap 'kill "$$forester_watch_pid" "$$render_watch_pid" 2>/dev/null || true; wait "$$forester_watch_pid" 2>/dev/null || true; wait "$$render_watch_pid" 2>/dev/null || true' EXIT INT TERM; \
		$(STACK) build $(STACK_FLAGS) --exec "site watch"
