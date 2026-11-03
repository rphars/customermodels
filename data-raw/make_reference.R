# Create (or refresh) the reference values the tests compare against.
#
# Run this from the package folder, on a machine where all course packages work:
#   Rscript data-raw/make_reference.R            # all models
#   Rscript data-raw/make_reference.R lecture4_heckman lecture3_mlogit   # just these
#
# Only re-run it when you have checked that a changed result is correct
# (e.g. a package fixed a bug), otherwise the tests lose their point.

library(customermodels)
source("tests/testthat/helper-models.R")

out_dir <- "tests/testthat/reference"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

which <- commandArgs(trailingOnly = TRUE)
if (length(which) == 0) which <- names(cm_reference_models)

for (nm in which) {
  spec <- cm_reference_models[[nm]]
  if (is.null(spec)) stop("Unknown model: ", nm)
  missing <- spec$pkgs[!vapply(spec$pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("SKIP  ", nm, " (not installed: ", paste(missing, collapse = ", "), ")")
    next
  }
  res <- suppressWarnings(spec$fit())
  jsonlite::write_json(lapply(res, function(x) as.list(x)),
                       file.path(out_dir, paste0(nm, ".json")),
                       digits = NA, auto_unbox = TRUE, pretty = TRUE)
  message("OK    ", nm)
}

# Record where the reference values came from.
pkgs <- unique(c("customermodels", unlist(lapply(cm_reference_models, `[[`, "pkgs"))))
pkgs <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
jsonlite::write_json(
  list(created = format(Sys.time(), "%Y-%m-%d %H:%M"),
       R = R.version.string,
       platform = R.version$platform,
       packages = as.list(vapply(pkgs, function(p) as.character(utils::packageVersion(p)), ""))),
  file.path(out_dir, "_info.json"), auto_unbox = TRUE, pretty = TRUE)
