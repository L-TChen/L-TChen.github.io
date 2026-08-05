STACK ?= stack
FORESTER ?= forester
WATCHEXEC ?= watchexec
STACK_FLAGS ?=

.PHONY: forest forest-incremental test hakyll build check clean serve watch

# Forester does not remove files for deleted trees, so always rebuild its
# ignored output directory from scratch.
forest:
	rm -rf forest/output
	cd forest && $(FORESTER) build

# Keep generated files present while Hakyll is watching. Clean builds still use
# the target above so output for deleted trees is removed before deployment.
forest-incremental:
	cd forest && $(FORESTER) build

test:
	$(STACK) test $(STACK_FLAGS)

hakyll:
	$(STACK) build $(STACK_FLAGS)
	$(STACK) exec -- site clean
	$(STACK) exec -- site build

build: forest
	$(MAKE) hakyll STACK="$(STACK)" STACK_FLAGS="$(STACK_FLAGS)"

check:
	$(MAKE) forest FORESTER="$(FORESTER)"
	$(MAKE) test STACK="$(STACK)" STACK_FLAGS="$(STACK_FLAGS)"
	$(MAKE) hakyll STACK="$(STACK)" STACK_FLAGS="$(STACK_FLAGS)"
	$(STACK) exec -- site check --internal-links
	test -f _site/posts/0003/index.xml
	test -f _site/posts/0003/index.html
	test -f _site/posts/default.xsl
	test -f _site/posts/style.css
	test -f _site/posts/forester.js
	test -f _site/posts/forest.json
	test -f _site/css/forester.css
	grep -Fq 'data-bs-theme="auto"' _site/posts/default.xsl
	grep -Fq 'href="/css/default.css"' _site/posts/default.xsl
	grep -Fq 'href="/css/forester.css"' _site/posts/default.xsl
	grep -Fq 'class="navbar navbar-expand navbar-dark bg-secondary sticky-top"' _site/posts/default.xsl
	grep -Fq 'class="col-12 col-xl-8 offset-xl-2 mw-100 me-0"' _site/posts/default.xsl
	grep -Fq 'class="d-none d-xl-block col-xl-2 sticky-xl-top sticky-below-navbar ms-0"' _site/posts/default.xsl
	grep -Fq 'class="block border rounded-3 bg-body-tertiary p-3 small"' _site/posts/default.xsl
	grep -Fq '<p class="fw-semibold text-body-secondary mb-3">Table of Contents</p>' _site/posts/default.xsl
	grep -Fq 'id="footer"' _site/posts/default.xsl
	test ! -e _site/posts/2026/03/29/LLM.html
	test ! -e content/posts/Personal/2026-03-29-LLM.md
	grep -Fq './posts/0003/' _site/index.html
	grep -Fq './posts/0003/' _site/posts.html
	grep -Fq 'class="list-group list-group-flush"' _site/index.html
	grep -Fq 'class="col-12 col-xl-8 offset-xl-2 order-2 order-xl-1"' _site/posts.html
	grep -Fq 'class="col-12 col-xl-2 order-1 order-xl-2 sticky-xl-top sticky-below-navbar"' _site/posts.html
	grep -Fq '<span aria-hidden="true">&middot;</span> Blog post</p>' _site/posts.html
	grep -Fq 'data-post-filter="all"' _site/posts.html
	grep -Fq 'data-post-tags>' _site/posts.html
	grep -Fq 'aria-pressed="true"' _site/posts.html
	grep -Fq 'data-bs-toggle="collapse"' templates/publications.html
	! grep -Fq -- '--publication-' assets/scss/default.scss assets/scss/forester.scss
	! grep -Fq -- '--post-' assets/scss/default.scss assets/scss/forester.scss
	! grep -Fq '48rem' assets/scss/default.scss assets/scss/forester.scss
	grep -Fq 'content="0;url=/posts/0003/index.xml"' _site/posts/0003/index.html

clean:
	$(STACK) exec -- site clean
	rm -rf forest/output

serve: build
	$(STACK) exec -- site watch

watch: build
	@command -v "$(WATCHEXEC)" >/dev/null || { echo "watchexec is required for make watch" >&2; exit 1; }
	@set -eu; \
		$(WATCHEXEC) --project-origin . --postpone --on-busy-update=queue \
			--watch forest/trees --watch forest/assets \
			--watch forest/forest.toml --watch forest/theme -- \
			$(MAKE) forest-incremental FORESTER="$(FORESTER)" & \
		forester_watch_pid=$$!; \
		trap 'kill "$$forester_watch_pid" 2>/dev/null || true; wait "$$forester_watch_pid" 2>/dev/null || true' EXIT INT TERM; \
		$(STACK) run -- site watch
