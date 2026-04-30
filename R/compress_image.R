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
  # Return TRUE if `name` looks like a .png (case-insensitive)
  grepl("\\.png$", name, ignore.case = TRUE)
}

#' @noRd
small_name <- function(filename, small_suffix) {
  # Build the expected small-version name from an original filename
  sub("\\.jpg$", paste0(small_suffix, ".jpg"), filename, ignore.case = TRUE)
}

#' @noRd
compress_image <- function(
    raw_bytes, quality = 60L,
    max_width = 1920L){
  # Compress a raw JPEG byte vector and return compressed bytes

  img <- magick::image_read(raw_bytes)
  info <- magick::image_info(img)

  if (!is.null(max_width) && info$width > max_width) {
    img <- magick::image_resize(img, paste0(max_width, "x"))
    cli::cli_alert_info("  Resized from {info$width}px to {max_width}px wide")
  }

  compressed <- magick::image_write(
    img,
    format  = "jpeg",
    quality = quality
  )
  compressed  # raw vector
}

# -----------------------------------------------------------------------------
# RECURSIVE FOLDER WALKER
# -----------------------------------------------------------------------------

#' @noRd
process_folder <- function(
    folder, path, results = list(),
    quality, max_width_px, small_suffix, dry_run){
  # Walk `folder` (a SharePoint drive item) and process all target JPGs
  # Returns a list of result records (for a summary at the end)

  cli::cli_h2("Scanning: {path}")

  items      <- folder$list_items()          # data frame of child items
  item_names <- items$name
  is_folder  <- items$`@microsoft.graph.isFolder` %||% rep(FALSE, nrow(items))

  # Collect names of existing _small files for quick lookup
  small_files_present <- item_names[grepl(
    paste0(small_suffix, "\\.jpg$"), item_names, ignore.case = TRUE
  )]

  for (i in seq_len(nrow(items))) {
    item_name <- item_names[[i]]

    # Recurse into sub-folders
    if (isTRUE(is_folder[[i]])) {
      sub_folder <- folder$get_item(item_name)
      results    <- process_folder(
        sub_folder,
        path         = fs::path(path, item_name),
        results      = results,
        quality      = quality,
        max_width_px = max_width_px,
        small_suffix = small_suffix,
        dry_run      = dry_run
      )
      next
    }

    # Skip non-target files
    if (!is_target_jpg(item_name, small_suffix)) next

    expected_small <- small_name(item_name, small_suffix)
    full_path      <- fs::path(path, item_name)

    # Skip if compressed version already exists
    if (expected_small %in% small_files_present) {
      cli::cli_alert_success("SKIP  {item_name}  (small version exists)")
      results <- c(results, list(list(
        file = full_path, action = "skipped", reason = "small exists"
      )))
      next
    }

    # Download, compress, upload
    cli::cli_alert_info("FOUND {item_name}")

    tryCatch({
      # Download original as raw bytes
      item      <- folder$get_item(item_name)
      tmp_in    <- tempfile(fileext = ".jpg")
      item$download(dest = tmp_in, overwrite = TRUE)
      orig_bytes <- readBin(tmp_in, "raw", n = file.size(tmp_in))
      orig_kb    <- round(length(orig_bytes) / 1024, 1)

      # Compress in memory
      small_bytes <- compress_image(orig_bytes, quality = quality, max_width = max_width_px)
      small_kb    <- round(length(small_bytes) / 1024, 1)
      saving_pct  <- round((1 - small_kb / orig_kb) * 100, 1)

      cli::cli_alert_success(
        "  {orig_kb} KB to {small_kb} KB  ({saving_pct}% smaller)"
      )

      if (!dry_run) {
        # Write to temp file then upload
        tmp_out <- tempfile(fileext = ".jpg")
        writeBin(small_bytes, tmp_out)
        folder$upload(tmp_out, dest = expected_small)
        cli::cli_alert_success("  Uploaded: {expected_small}")
        unlink(tmp_out)
      } else {
        cli::cli_alert_warning("  DRY RUN - upload skipped")
      }

      unlink(tmp_in)
      results <- c(results, list(list(
        file     = full_path,
        action   = if (dry_run) "dry_run" else "compressed",
        orig_kb  = orig_kb,
        small_kb = small_kb,
        saving   = saving_pct
      )))

    }, error = function(e) {
      cli::cli_alert_danger("  ERROR processing {item_name}: {conditionMessage(e)}")
      results <<- c(results, list(list(
        file   = full_path,
        action = "error",
        reason = conditionMessage(e)
      )))
    })
  }

  results
}

# Null-coalescing operator (available in R 4.4+ natively; defined here for compat)
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
#'   the site's default document library root,
#'   e.g. `"Shared Documents/Photos"`.
#' @param quality Integer (1--100). JPEG compression quality for the compressed
#'   version. Default `60L`.
#' @param max_width_px Integer or `NULL`. Images wider than this value are
#'   resized to this width (in pixels) before compression. Pass `NULL` to skip
#'   resizing. Default `1920L`.
#' @param small_suffix Character. Suffix inserted before `.jpg` to name the
#'   compressed file (e.g. `"photo_small.jpg"`). Default `"_small"`.
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
#'   sharepoint_folder   = "Shared Documents/Photos",
#'   dry_run             = TRUE
#' )
#' }
compress_sharepoint_image <- function(
    sharepoint_site_url = "https://yourorg.sharepoint.com/sites/YourSite",
    sharepoint_folder = "Shared Documents/Photos",  # relative to site root
    quality = 60L, # JPEG quality for compressed version (1-100)
    max_width_px = 1920L, # resize to this width if wider (NULL to skip)
    small_suffix = "_small", # appended before .jpg to photo_small.jpg
    dry_run = TRUE # TRUE = report actions without uploading. FALSE = upload
    ) {

  cli::cli_h1("SharePoint JPG Compressor")
  cli::cli_alert_info("Site   : {sharepoint_site_url}")
  cli::cli_alert_info("Folder : {sharepoint_folder}")
  cli::cli_alert_info("Quality: {quality}%  |  Max width: {max_width_px %||% 'none'}px")
  if (dry_run) cli::cli_alert_warning("DRY RUN MODE - no files will be uploaded")

  # Connect to SharePoint (opens browser for auth on first run)
  site  <- Microsoft365R::get_sharepoint_site(sharepoint_site_url)
  drive <- site$get_drive()                          # default document library
  root  <- drive$get_item(sharepoint_folder)         # starting folder

  # Walk and process
  results <- process_folder(
    root,
    path         = sharepoint_folder,
    quality      = quality,
    max_width_px = max_width_px,
    small_suffix = small_suffix,
    dry_run      = dry_run
  )

  # -----------------------------------------------------------------------------
  # SUMMARY
  # -----------------------------------------------------------------------------

  cli::cli_h1("Summary")

  actions <- vapply(results, `[[`, character(1), "action")
  cli::cli_bullets(c(
    "v" = "Compressed : {sum(actions == 'compressed')}",
    "i" = "Dry-run    : {sum(actions == 'dry_run')}",
    ">" = "Skipped    : {sum(actions == 'skipped')}",
    "x" = "Errors     : {sum(actions == 'error')}"
  ))

  compressed <- results[actions %in% c("compressed", "dry_run")]
  if (length(compressed)) {
    savings <- vapply(compressed, `[[`, numeric(1), "saving")
    cli::cli_alert_info(
      "Average size reduction: {round(mean(savings), 1)}%  |  ",
      "Range: {round(min(savings), 1)}% to {round(max(savings), 1)}%"
    )
  }

  cli::cli_alert_success("Done.")
}
