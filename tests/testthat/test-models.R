# Re-estimate every lecture model and compare with the stored reference values.
# A failure here means a package update changed (or broke) a course result.

# Plain named numbers: drops extra attributes and makes duplicate names unique
# (e.g. Heckman models use the same variable names in both equations), the
# same way the names are stored in the reference JSON.
as_plain <- function(x) {
  x <- unlist(x)
  n <- names(x)
  x <- as.numeric(x)
  if (!is.null(n)) names(x) <- make.unique(n)
  x
}

for (nm in names(cm_reference_models)) {
  local({
    name <- nm
    spec <- cm_reference_models[[name]]

    test_that(paste(name, "matches the reference values"), {
      for (p in spec$pkgs) skip_if_not_installed(p)
      ref_file <- test_path("reference", paste0(name, ".json"))
      skip_if_not(file.exists(ref_file),
                  paste("no reference yet for", name, "- run data-raw/make_reference.R"))

      ref <- jsonlite::read_json(ref_file, simplifyVector = TRUE)
      res <- suppressWarnings(spec$fit())

      expect_setequal(names(res), names(ref))
      for (k in names(ref)) {
        expect_equal(as_plain(res[[k]]), as_plain(ref[[k]]),
                     tolerance = spec$tol, label = paste0(name, "$", k))
      }
    })
  })
}
