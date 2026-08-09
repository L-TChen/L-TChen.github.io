![Hakyll site CI](https://github.com/L-TChen/L-TChen.github.io/actions/workflows/hakyll.yml/badge.svg)

## Set up

Clone the repository with its pinned Bootstrap and Forester theme submodules:

```console
git clone --recurse-submodules https://github.com/L-TChen/L-TChen.github.io.git
```

The build needs Stack, OCaml 5.3, exactly Forester 5.0, and `xsltproc` from
libxslt. With opam:

```console
opam switch create 5.3.0
opam install forester.5.0
```

Run the complete build and verification from the repository root:

```console
opam exec -- make check
```

`make build` builds the Forester and Hakyll outputs, then pre-renders each
Forester `index.xml` as `_site/posts/<id>/index.html` with `xsltproc` and removes
the deployed XML copy. `make watch` keeps the same rendering stage active as
Forester, Hakyll, or the XSLT changes. `make test` runs the Haskell adapter tests,
and `make serve` builds and serves the combined site locally. The CI-specific
Hakyll flags can be supplied with `STACK_FLAGS="..."`.

### Forester rendering

Forester still generates XML under `forest/output`, but the published and local
Hakyll site is rendered to HTML ahead of time. The renderer uses
`_site/posts/default.xsl`, after Hakyll has assembled the site-owned stylesheet
with the Forester base-theme includes (`core.xsl`, `metadata.xsl`, `links.xsl`,
and `tree.xsl`). This keeps the browser from having to perform the XSLT
transformation itself.

If rendering fails because the base-theme includes are missing, initialize all
submodules and rebuild the generated output:

```console
git submodule update --init --recursive
make clean
make build
```

A leading `-` in `git submodule status forest/theme` means that the theme
submodule has not been initialized.

## Author posts with Forester

Add a `.tree` file under `forest/trees`. A dated entry is not included on the
Hakyll homepage or archive until it opts in explicitly:

```forester
\title{Example title}
\date{2026-08-03}
\taxon{Note}
\meta{published}{}
\tag{english}

\p{Post content goes here.}
```

For a published entry, `\title`, at least one complete `\date`, and `\taxon`
are required. The presence of `\meta{published}{}` opts the entry in; its value
is ignored. Forester 5.0 requires the empty second brace group as part of the
generic `\meta` syntax. Malformed or incomplete published entries fail the
Hakyll build. Use `\tag{...}` once per optional archive tag (this is Forester
5.0's source syntax for the manifest's tag content). Tags are normalized and
deduplicated for filtering.

The earliest date is the publication date. Later dates may record revisions
without moving an entry to the top of the archive. The taxon is the displayed
kind and may be, for example, `Blog post`, `Note`, or `Announcement`.

Do not edit or commit `forest/output` or `_site`. `make forest` removes stale
generated files, runs Forester from `forest/`, and recreates its output. The
committed `forest/theme` gitlink pins the official base theme's `5.0` release.
