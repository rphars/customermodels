test_that("every dataset loads with the expected shape", {
  expected <- list(
    telco_churn         = c(5000, 21),
    cm_lecture3         = c(200, 10),
    movie_ticket_data   = c(1000, 4),
    donation_data       = c(4268, 9),
    cm_lecture5_flyer   = c(1800, 6),
    cm_lecture5_contest = c(3660, 12),
    churn_data          = c(432, 59)
  )
  expect_setequal(cm_datasets(), names(expected))
  for (nm in names(expected)) {
    expect_equal(dim(cm_data(nm)), expected[[nm]], label = nm)
  }
})

test_that("semicolon files are read with the right separator and decimals", {
  contests <- cm_data("cm_lecture5_contest")
  expect_identical(names(contests)[1], "id")           # no BOM junk in the name
  expect_type(contests$award_amount_USD, "double")     # "2,8" read as 2.8
  expect_equal(contests$award_amount_USD[1], 2.8)
  flyer <- cm_data("cm_lecture5_flyer.csv")            # ".csv" suffix is fine too
  expect_identical(names(flyer)[1], "Customer_ID")
})

test_that("unknown names give a helpful error", {
  expect_error(cm_data("nope"), "Available")
  expect_error(cm_script("nope", open = FALSE), "Available")
})

test_that("cm_script copies a script without overwriting", {
  dir <- tempfile("cmscript")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- cm_script("lecture2", dir = dir, open = FALSE)
  expect_true(file.exists(path))
  writeLines("# my edits", path)
  expect_message(cm_script("lecture2", dir = dir, open = FALSE), "already exists")
  expect_identical(readLines(path), "# my edits")
})
