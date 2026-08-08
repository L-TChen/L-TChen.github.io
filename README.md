![Hakyll site CI](https://github.com/L-TChen/L-TChen.github.io/actions/workflows/hakyll.yml/badge.svg)

## Set up

Clone the repository with its pinned Bootstrap and Forester theme submodules:

```console
git clone --recurse-submodules https://github.com/L-TChen/L-TChen.github.io.git
```

The build needs Stack, OCaml 5.3, and exactly Forester 5.0. With opam:

```console
opam switch create 5.3.0
opam install forester.5.0
```

Run the complete build and verification from the repository root:

```console
opam exec -- make check
```

`make build` performs a clean Forester build followed by a clean Hakyll build.
`make test` runs the Haskell adapter tests, and `make serve` builds and serves
the combined site locally. The CI-specific Hakyll flags can be supplied with
`STACK_FLAGS="..."`.

### Blank Forester post pages

Forester posts are generated as XML and rendered in the browser with XSLT. If
`/posts/<id>/index.xml` appears blank even though the XML contains content,
check that the Forester theme submodule is initialized. Without it,
`default.xsl` is generated but its base-theme includes (`core.xsl`,
`metadata.xsl`, `links.xsl`, and `tree.xsl`) are missing, so the browser cannot
transform the XML into HTML. This can explain why the same page works from a
clone on another computer.

Initialize all submodules and rebuild the generated output:

```console
git submodule update --init --recursive
make clean
make build
```

A leading `-` in `git submodule status forest/theme` means that the theme
submodule has not been initialized. Browser developer tools may also show 404
responses for the missing XSL files under `/posts/`.

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
