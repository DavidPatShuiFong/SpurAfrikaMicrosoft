# =============================================================================
# compress_sharepoint_images.R
#
# Recursively scans a SharePoint folder for .jpg and .png files, then creates a
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

#' Compress JPEG and PNG images in a SharePoint folder
#'
#' Recursively scans a SharePoint document library folder for JPEG and PNG files
#' and uploads a compressed `_small.jpg` version of each image that does not
#' already have a `_small.jpg` (or `_small.png`). Original files are never modified.
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
#'   resizing. Default `NULL`.
#' @param small_suffix Character. Suffix inserted before the extension to name
#'   the compressed file (e.g. `"photo_small.jpg"`). Default `"_small"`.
#' @param file_prefix Character or `NULL`. If provided, only image files whose
#'   names begin with this string are eligible for compression (e.g. `"202604"`
#'   to process only April 2026 photos). Default `NULL` (no filtering).
#' @param file_prefix_regex Logical. If `TRUE`, `file_prefix` is treated as a
#'   regular expression passed to [grepl()] rather than a literal prefix
#'   (e.g. `"^2026(04|05)"` to match April or May 2026). Default `FALSE`.
#' @param n_workers Integer. Number of parallel processes. Default `1`
#' @param dry_run Logical. If `TRUE` (the default), report planned actions
#'   without uploading anything.
#'
#' @return Invisibly returns a list of result records, one per image
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
    max_width_px        = NULL,      # resize to this width if wider (NULL to skip)
    small_suffix        = "_small",  # appended before extension e.g. photo_small.jpg
    file_prefix         = NULL,      # only process files starting with this string (NULL = all)
    file_prefix_regex   = FALSE,     # treat file_prefix as a regular expression
    n_workers           = 1,         # number of parallel processes
    dry_run             = TRUE       # TRUE = report actions without uploading. FALSE = upload
) {

  tictoc::tic() # start timer

  if (n_workers > 1)
    future::plan(strategy = future::multisession, workers = n_workers)
  else
    future::plan(strategy = future::sequential)

  cli::cli_h1("SharePoint JPG and PNG Compressor")
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
  # Phase 1: scan folder tree — collect all target images sequentially
  # -----------------------------------------------------------------------------

  progressr::handlers(progressr::handler_cli(
    format="{cli::pb_spin} Top-level folder {cli::pb_current}/{cli::pb_total} {cli::pb_status} | ETA: {cli::pb_eta}",
    intrusiveness = 10
  ))

  targets <- progressr::with_progress(
    collect_targets(root, sharepoint_folder, small_suffix,
                    file_prefix = file_prefix, file_prefix_regex = file_prefix_regex)
  )
  cli::cli_alert_info("Found {length(targets)} image{?s} to evaluate")

  # -----------------------------------------------------------------------------
  # Phase 2: process files in parallel with a progress bar
  #
  # process_files is defined inside this function so furrr serialises it with its
  # captured environment (quality, max_width_px, dry_run, compress_image).
  # -----------------------------------------------------------------------------

  process_files <- function(t) {
    if (t$small_exists) {
      return(list(file = t$full_path, action = "skipped", reason = "small exists"))
    }

    tryCatch({
      item_file  <- t$folder$get_item(t$item_name)
      tmp_in     <- tempfile(
        fileext = if (grepl("\\.jpg$", t$item_name, ignore.case = TRUE)) ".jpg" else ".png"
        )
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

  progressr::handlers(progressr::handler_cli(
    format="{cli::pb_spin} Image file {cli::pb_current}/{cli::pb_total} {cli::pb_status} | ETA: {cli::pb_eta}",
    intrusiveness = 10
  ))

  results <- progressr::with_progress({
    p <- progressr::progressor(steps = length(targets))
    furrr::future_map(targets, function(t) {
      result <- process_files(t)
      p()
      result
    },.options = furrr::furrr_options(seed = TRUE)
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
      "Average size reduction: {round(mean(savings), 1)}%  |  Range: {round(min(savings), 1)}% to {round(max(savings), 1)}%"
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
small_name_jpg <- function(filename, small_suffix) {
  # Build the expected small-version name from an original filename
  sub("\\.jpg$", paste0(small_suffix, ".jpg"), filename, ignore.case = TRUE)
}

#' @noRd
small_name_png_to_jpg <- function(filename, small_suffix) {
  # Build the expected small-version name from an original filename
  sub("\\.png$", paste0(small_suffix, ".jpg"), filename, ignore.case = TRUE)
}

#' @noRd
compress_image <- function(raw_bytes, quality = 60L, max_width = 1920L) {
  # Compress a raw JPEG/PNG byte vector and return compressed bytes JPG
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
                            file_prefix = NULL, file_prefix_regex = FALSE, toplevel = TRUE) {
  # Recursively walk `folder` and return a flat list of target-file descriptors.
  # Each descriptor is a named list: folder, item_name, full_path,
  # expected_small, small_exists.
  # file_prefix: when non-NULL, only include images matching this string/regex.
  #
  # Returns lists:
  #   folder
  #   item_name (file name)
  #   full_path
  #   expected_small (file name to use for small version; always .jpg)
  #   small_exists (TRUE if a small version already exists, in either .jpg or .png)
  #
  # progressr::with_progress() must be active in the caller; this function only
  # creates a progressor whose signals relay up to that outer context — including
  # from recursive calls running inside furrr workers.

  items <- folder$list_items()

  small_files <- items$name[grepl(
    paste0(small_suffix, "\\.(jpg|png)$"), items$name, ignore.case = TRUE
  )]

  # One progressor at the top level search, signals relay to the caller's with_progress().
  if (toplevel)
    p <- progressr::progressor(steps = nrow(items), message = paste("Scanning:", path))

  item_results <- furrr::future_pmap(items, function(...) {
    item  <- list(...)
    nm    <- item$name
    isdir <- isTRUE(item$isdir)
    if (toplevel)
      p()

    if (isdir) {
      subfolder <- folder$get_item(nm)
      collect_targets(subfolder, fs::path(path, nm), small_suffix,
                      file_prefix, file_prefix_regex, toplevel = FALSE)
    } else if (
      (is_target_jpg(nm, small_suffix) || is_target_png(nm, small_suffix)) &&
      (is.null(file_prefix) ||
       if (file_prefix_regex) grepl(file_prefix, nm) else startsWith(nm, file_prefix))
    ) {
      is_png    <- grepl("\\.png$", nm, ignore.case = TRUE)
      # 'small' version is always .jpg, even when the source is .png
      exp_small <- if (is_png)
        small_name_png_to_jpg(nm, small_suffix)
      else
        small_name_jpg(nm, small_suffix)
      # For PNG source files a pre-existing small version could be either
      # _small.jpg (normal output) or _small.png (legacy)
      small_exists <- exp_small %in% small_files ||
        (is_png && sub("\\.png$", paste0(small_suffix, ".png"), nm,
                       ignore.case = TRUE) %in% small_files)
      list(list(
        folder         = folder,
        item_name      = nm,
        full_path      = fs::path(path, nm),
        expected_small = exp_small,
        small_exists   = small_exists
      ))
    } else {
      list()
    }
  })

  purrr::list_flatten(item_results)
}

# Null-coalescing operator (available in R 4.4+ natively; defined here for compat)
`%||%` <- function(a, b) if (!is.null(a)) a else b
