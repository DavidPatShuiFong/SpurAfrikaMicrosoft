# =============================================================================
# compress_sharepoint_images.R
#
# Recursively scans a SharePoint folder for .jpg files, then creates a
# compressed "_small.jpg" version of each image if one does not yet exist.
#
# Dependencies:
#
#     Microsoft365R     SharePoint / OneDrive access
#     magick            Image processing (wraps ImageMagick)
#     cli               Pretty console output
#     fs                Path helpers
#
# Authentication:
#   On first run you will be prompted to sign in via browser (OAuth).
#   Credentials are cached by Microsoft365R for subsequent runs.
# =============================================================================

# -----------------------------------------------------------------------------
# MAIN FUNCTION
# -----------------------------------------------------------------------------

#' Compress JPEG images in a SharePoint folder
#'
#' Recursively scans a SharePoint document library folder for JPEG files and
#' uploads a compressed `_small.jpg` version of each image that does not already
#' have one. Original files are never modified.
#'
#' @param sharepoint_site_url Character. Full URL of the SharePoint site,
#'   e.g. `"https://yourorg.sharepoint.com/sites/YourSite"`.
#' @param sharepoint_folder Character. Path to the starting folder relative to
#'   the root of the default document library (i.e. relative to "Documents"),
#'   e.g. `"Photos"` or `"Photos/Events/2025"`.
#' @param quality Integer (1--100). JPEG compression quality for the compressed
#'   version. Default `60L`.
#' @param max_width_px Integer or `NULL`. Images wider than this value are
#'   resized to this width (in pixels) before compression. Pass `NULL` to skip
#'   resizing. Default `1920L`.
#' @param small_suffix Character. Suffix inserted before `.jpg` to name the
#'   compressed file (e.g. `"photo_small.jpg"`). Default `"_small"`.
#' @param file_prefix Character or `NULL`. If provided, only JPEG files whose
#'   names begin with this string are eligible for compression (e.g. `"202604"`
#'   to process only April 2026 photos). Default `NULL` (no filtering).
#' @param file_prefix_regex Logical. If `TRUE`, `file_prefix` is treated as a
#'   regular expression passed to [grepl()] rather than a literal prefix
#'   (e.g. `"^2026(04|05)"` to match April or May 2026). Default `FALSE`.
#' @param dry_run Logical. If `TRUE` (the default), report planned actions
#'   without uploading anything.
#'
#' @return Invisibly returns a list of result records, one per JPEG
#'   encountered. Each record is a named list with at minimum an `action` field:
#'   `"compressed"`, `"dry_run"`, `"skipped"`, or `"error"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' compress_sharepoint_image(
#'   sharepoint_site_url = "https://yourorg.sharepoint.com/sites/YourSite",
#'   sharepoint_folder   = "Photos",
#'   dry_run             = TRUE
#' )
#' }
compress_sharepoint_image <- function(
    sharepoint_site_url = "https://yourorg.sharepoint.com/sites/YourSite",
    sharepoint_folder   = "Photos",  # relative to default document folder
    quality             = 60L,       # JPEG quality for compressed version (1-100)
    max_width_px        = 1920L,     # resize to this width if wider (NULL to skip)
    small_suffix        = "_small",  # appended before .jpg to photo_small.jpg
    file_prefix         = NULL,      # only process files starting with this string (NULL = all)
    file_prefix_regex   = FALSE,     # treat file_prefix as a regular expression
    dry_run             = TRUE       # TRUE = report actions without uploading. FALSE = upload
) {

  tictoc::tic() # start timer

  cli::cli_h1("SharePoint JPG Compressor")
  cli::cli_alert_info("Site   : {sharepoint_site_url}")
  cli::cli_alert_info("Folder : {sharepoint_folder}")
  cli::cli_alert_info("Quality: {quality}%  |  Max width: {max_width_px %||% 'none'}px")
  if (!is.null(file_prefix)) cli::cli_alert_info("Prefix filter: {file_prefix}")
  if (dry_run) cli::cli_alert_warning("DRY RUN MODE - no files will be uploaded")

  # Connect to SharePoint (opens browser for auth on first run)
  site  <- Microsoft365R::get_sharepoint_site(site_url = sharepoint_site_url)
  drive <- site$get_drive()                   # default document library
  root  <- drive$get_item(sharepoint_folder)  # starting folder

  # -----------------------------------------------------------------------------
  # Phase 1: scan folder tree — collect all target JPEGs sequentially
  # -----------------------------------------------------------------------------

  targets <- collect_targets(root, sharepoint_folder, small_suffix,
                             file_prefix = file_prefix, file_prefix_regex = file_prefix_regex)
  cli::cli_alert_info("Found {length(targets)} JPEG{?s} to evaluate")

  # -----------------------------------------------------------------------------
  # Phase 2: process files in parallel with a progress bar
  #
  # process_files is defined inside this function so purrr serialises it with its
  # captured environment (quality, max_width_px, dry_run, compress_image).
  # -----------------------------------------------------------------------------

  process_files <- function(t) {
    if (t$small_exists) {
      return(list(file = t$full_path, action = "skipped", reason = "small exists"))
    }

    tryCatch({
      item_file  <- t$folder$get_item(t$item_name)
      tmp_in     <- tempfile(fileext = ".jpg")
      item_file$download(dest = tmp_in, overwrite = TRUE)
      orig_bytes <- readBin(tmp_in, "raw", n = file.size(tmp_in))
      orig_kb    <- round(length(orig_bytes) / 1024, 1)
      unlink(tmp_in)

      small_bytes <- compress_image(orig_bytes, quality = quality, max_width = max_width_px)
      small_kb    <- round(length(small_bytes) / 1024, 1)
      saving_pct  <- round((1 - small_kb / orig_kb) * 100, 1)

      if (!dry_run) {
        tmp_out <- tempfile(fileext = ".jpg")
        writeBin(small_bytes, tmp_out)
        t$folder$upload(tmp_out, dest = t$expected_small)
        unlink(tmp_out)
      }

      list(
        file     = t$full_path,
        action   = if (dry_run) "dry_run" else "compressed",
        orig_kb  = orig_kb,
        small_kb = small_kb,
        saving   = saving_pct
      )
    }, error = function(e) {
      list(file = t$full_path, action = "error", reason = conditionMessage(e))
    })
  }

  progressr::handlers("cli")
  results <- progressr::with_progress({
    p <- progressr::progressor(steps = length(targets))
    purrr::map(targets, function(t) {
      result <- process_files(t)
      p()
      result
    }
    # if we used furrr we would need to set random number options
    # ,.options = furrr::furrr_options(seed = TRUE)
    )
  })

  # -----------------------------------------------------------------------------
  # Phase 3: print per-file results grouped by directory
  # -----------------------------------------------------------------------------

  cli::cli_h1("Results")

  files <- vapply(results, `[[`, character(1), "file")
  dirs  <- as.character(dirname(files))

  for (d in sort(unique(dirs))) {
    cli::cli_h2(d)
    for (r in results[dirs == d]) {
      switch(r$action,
        compressed = cli::cli_bullets(c(
          "v" = "{basename(r$file)}: {r$orig_kb} KB -> {r$small_kb} KB ({r$saving}% smaller, uploaded)"
        )),
        dry_run    = cli::cli_bullets(c(
          ">" = "{basename(r$file)}: {r$orig_kb} KB -> {r$small_kb} KB ({r$saving}% smaller)"
        )),
        skipped    = cli::cli_bullets(c(
          "i" = "{basename(r$file)}: skipped (small version exists)"
        )),
        error      = cli::cli_bullets(c(
          "x" = "{basename(r$file)}: ERROR - {r$reason}"
        ))
      )
    }
  }

  # -----------------------------------------------------------------------------
  # Phase 4: summary counts
  # -----------------------------------------------------------------------------

  cli::cli_h1("Summary")

  actions <- vapply(results, `[[`, character(1), "action")
  cli::cli_bullets(c(
    "v" = "Compressed : {sum(actions == 'compressed')}",
    "i" = "Dry-run    : {sum(actions == 'dry_run')}",
    ">" = "Skipped    : {sum(actions == 'skipped')}",
    "x" = "Errors     : {sum(actions == 'error')}"
  ))

  compressed_results <- results[actions %in% c("compressed", "dry_run")]
  if (length(compressed_results)) {
    savings <- vapply(compressed_results, `[[`, numeric(1), "saving")
    cli::cli_alert_info(
      "Average size reduction: {round(mean(savings), 1)}%  |  ",
      "Range: {round(min(savings), 1)}% to {round(max(savings), 1)}%"
    )
  }

  tictoc::toc()
  cli::cli_alert_success("Done.")
  invisible(results)
}

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

#' @noRd
is_target_jpg <- function(name, small_suffix) {
  # Return TRUE if `name` looks like a .jpg (case-insensitive, not _small.jpg)
  grepl("\\.jpg$", name, ignore.case = TRUE) &&
    !grepl(paste0(small_suffix, "\\.jpg$"), name, ignore.case = TRUE)
}

#' @noRd
is_target_png <- function(name, small_suffix) {
  # Return TRUE if `name` looks like a .png (case-insensitive, not _small.png)
  grepl("\\.png$", name, ignore.case = TRUE) &&
    !grepl(paste0(small_suffix, "\\.png$"), name, ignore.case = TRUE)
}

#' @noRd
small_name <- function(filename, small_suffix) {
  # Build the expected small-version name from an original filename
  sub("\\.jpg$", paste0(small_suffix, ".jpg"), filename, ignore.case = TRUE)
}

#' @noRd
compress_image <- function(raw_bytes, quality = 60L, max_width = 1920L) {
  # Compress a raw JPEG byte vector and return compressed bytes
  img  <- magick::image_read(raw_bytes)
  info <- magick::image_info(img)

  if (!is.null(max_width) && info$width > max_width) {
    img <- magick::image_resize(img, paste0(max_width, "x"))
  }

  compressed <- magick::image_write(img, format = "jpeg", quality = quality)
  compressed  # raw vector
}

# -----------------------------------------------------------------------------
# RECURSIVE FOLDER SCANNER
# -----------------------------------------------------------------------------

#' @noRd
collect_targets <- function(folder, path, small_suffix,
                            file_prefix = NULL, file_prefix_regex = FALSE, .pb = NULL) {
  # Recursively walk `folder` and return a flat list of target-file descriptors.
  # Each descriptor is a named list: folder, item_name, full_path,
  # expected_small, small_exists.
  # file_prefix: when non-NULL, only include JPEGs matching this string/regex.
  # .pb: cli progress bar id; created at the top level and passed through recursion.

  top_level <- is.null(.pb)
  if (top_level) {
    .pb <- cli::cli_progress_bar(
      name        = "Scanning",
      total       = NA,
      clear       = FALSE,
      .auto_close = FALSE
    )
  }

  # Show which folder is being listed (the slow API call)
  cli::cli_progress_update(id = .pb, status = path, inc = 0L)
  items <- folder$list_items()

  small_files <- items$name[grepl(
    paste0(small_suffix, "\\.jpg$"), items$name, ignore.case = TRUE
  )]

  targets <- list()

  purrr::pmap(items, function(...) {
    item  <- list(...)
    nm    <- item$name
    isdir <- isTRUE(item$isdir)

    if (isdir) {
      sub     <- folder$get_item(nm)
      targets <<- c(targets, collect_targets(sub, fs::path(path, nm), small_suffix,
                                             file_prefix, file_prefix_regex, .pb))
    } else if (is_target_jpg(nm, small_suffix) &&
               (is.null(file_prefix) ||
                if (file_prefix_regex) grepl(file_prefix, nm) else startsWith(nm, file_prefix))) {
      cli::cli_progress_update(id = .pb, inc = 1L)
      exp_small <- small_name(nm, small_suffix)
      targets   <<- c(targets, list(list(
        folder         = folder,
        item_name      = nm,
        full_path      = fs::path(path, nm),
        expected_small = exp_small,
        small_exists   = exp_small %in% small_files
      )))
    }
  })

  if (top_level) cli::cli_progress_done(id = .pb)
  targets
}

# Null-coalescing operator (available in R 4.4+ natively; defined here for compat)
`%||%` <- function(a, b) if (!is.null(a)) a else b
