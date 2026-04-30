# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

`SpurAfrikaMicrosoft` is an R package that provides utilities for working with Microsoft 365 / SharePoint from R. The current primary function (`compress_sharepoint_image` in `R/compress_image.R`) recursively scans a SharePoint folder for JPEG files and uploads compressed `_small.jpg` versions using `magick` (ImageMagick wrapper).

## Common commands

```r
# Install dependencies (run once)
install.packages(c("Microsoft365R", "magick", "cli", "fs"))

# Install the package from source
devtools::install()

# Load for interactive development (no reinstall needed)
devtools::load_all()

# Run R CMD CHECK
devtools::check()

# Generate/update documentation from roxygen2 comments
devtools::document()

# Run tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-compress_image.R")
```

## Architecture

- `R/compress_image.R` — the main implementation file. Contains:
  - `compress_sharepoint_image()` — top-level entry point; connects to SharePoint via `Microsoft365R::get_sharepoint_site()`, then calls `process_folder()`
  - `process_folder()` — recursive walker over SharePoint drive items; skips files already having a `_small.jpg` counterpart
  - `compress_image()` — in-memory compress/resize helper using `magick`
  - `is_target_jpg()` / `small_name()` — filename predicate and naming helpers
  - `%||%` — null-coalescing operator (defined for R < 4.4 compat)

- `R/hello.R` — scaffolding placeholder; can be removed once real functions are documented

## Authentication

`Microsoft365R` uses OAuth via a browser prompt on first run; credentials are cached locally for subsequent runs. There are no API keys to manage in code.

## Key design notes

- `dry_run = TRUE` is the safe default: it reports what would happen without uploading anything.
