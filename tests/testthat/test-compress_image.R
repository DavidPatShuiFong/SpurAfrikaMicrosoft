make_raw_jpeg <- function(width = 200, height = 200) {
  magick::image_write(
    magick::image_blank(width, height, "white"),
    format = "jpeg"
  )
}

# is_target_jpg -----------------------------------------------------------

test_that("is_target_jpg returns TRUE for plain jpg files", {
  expect_true(is_target_jpg("photo.jpg", "_small"))
  expect_true(is_target_jpg("photo.JPG", "_small"))
  expect_true(is_target_jpg("my photo.jpg", "_small"))
})

test_that("is_target_jpg returns FALSE for already-compressed files", {
  expect_false(is_target_jpg("photo_small.jpg", "_small"))
  expect_false(is_target_jpg("photo_small.JPG", "_small"))
})

test_that("is_target_jpg returns FALSE for non-JPEG files", {
  expect_false(is_target_jpg("photo.png", "_small"))
  expect_false(is_target_jpg("photo.txt", "_small"))
  expect_false(is_target_jpg("photo", "_small"))
})

test_that("is_target_jpg respects a custom small_suffix", {
  expect_true(is_target_jpg("photo_small.jpg", "_thumb"))
  expect_false(is_target_jpg("photo_thumb.jpg", "_thumb"))
})

# is_target_png -----------------------------------------------------------

test_that("is_target_png returns TRUE for plain png files", {
  expect_true(is_target_png("photo.png", "_small"))
  expect_true(is_target_png("photo.PNG", "_small"))
  expect_true(is_target_png("my photo.png", "_small"))
})

test_that("is_target_png returns FALSE for non-PNG files", {
  expect_false(is_target_png("photo.jpg", "_small"))
  expect_false(is_target_png("photo.txt", "_small"))
  expect_false(is_target_png("photo", "_small"))
})

test_that("is_target_png does not filter already-compressed files", {
  # small_suffix is accepted but unused — _small.png is still a target
  expect_true(is_target_png("photo_small.png", "_small"))
})

# small_name --------------------------------------------------------------

test_that("small_name appends suffix before .jpg", {
  expect_equal(small_name("photo.jpg", "_small"), "photo_small.jpg")
})

test_that("small_name is case-insensitive on the extension", {
  expect_equal(small_name("photo.JPG", "_small"), "photo_small.jpg")
})

test_that("small_name works with a custom suffix", {
  expect_equal(small_name("photo.jpg", "_thumb"), "photo_thumb.jpg")
})

test_that("small_name handles filenames with dots in the stem", {
  expect_equal(small_name("my.photo.jpg", "_small"), "my.photo_small.jpg")
})

# compress_image ----------------------------------------------------------

test_that("compress_image returns a raw vector", {
  result <- compress_image(make_raw_jpeg(), quality = 60L, max_width = NULL)
  expect_type(result, "raw")
})

test_that("compress_image produces a smaller file at low quality", {
  raw_in <- make_raw_jpeg(800, 800)
  raw_out <- compress_image(raw_in, quality = 10L, max_width = NULL)
  expect_lt(length(raw_out), length(raw_in))
})

test_that("compress_image resizes when image exceeds max_width", {
  raw_in  <- make_raw_jpeg(400, 300)
  raw_out <- compress_image(raw_in, quality = 80L, max_width = 200L)
  info <- magick::image_info(magick::image_read(raw_out))
  expect_lte(info$width, 200L)
})

test_that("compress_image does not resize when image is within max_width", {
  raw_in  <- make_raw_jpeg(100, 100)
  raw_out <- compress_image(raw_in, quality = 80L, max_width = 200L)
  info <- magick::image_info(magick::image_read(raw_out))
  expect_equal(info$width, 100L)
})

test_that("compress_image skips resize when max_width is NULL", {
  raw_in  <- make_raw_jpeg(400, 300)
  raw_out <- compress_image(raw_in, quality = 80L, max_width = NULL)
  info <- magick::image_info(magick::image_read(raw_out))
  expect_equal(info$width, 400L)
})
