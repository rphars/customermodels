# Read settings per dataset. Names match the original csv files, so the
# lecture slides and scripts keep working with the same names.
.cm_files <- list(
  telco_churn         = list(),                        # lecture 2
  cm_lecture3         = list(),                        # lecture 3
  movie_ticket_data   = list(),                        # lecture 4
  donation_data       = list(),                        # lecture 4
  cm_lecture5_flyer   = list(sep = ";"),               # lecture 5
  cm_lecture5_contest = list(sep = ";", dec = ","),    # lecture 5
  churn_data          = list()                         # lecture 6
)

#' Load a course dataset
#'
#' Reads one of the course datasets with the right separator and decimal
#' settings, so students don't need setwd() or to fiddle with sep/dec.
#'
#' @param name Dataset name, with or without ".csv", e.g. "telco_churn".
#' @return A data frame, identical to what read.csv() gave in the old scripts.
#' @examples
#' telco <- cm_data("telco_churn")
#' flyer <- cm_data("cm_lecture5_flyer.csv")
#' @importFrom utils read.csv
#' @export
cm_data <- function(name) {
  name <- sub("\\.csv$", "", name)
  spec <- .cm_files[[name]]
  if (is.null(spec)) {
    stop("Unknown dataset '", name, "'. Available: ",
         paste(names(.cm_files), collapse = ", "), call. = FALSE)
  }
  path <- system.file("extdata", paste0(name, ".csv"),
                      package = "customermodels", mustWork = TRUE)
  read.csv(path,
           sep = if (is.null(spec$sep)) "," else spec$sep,
           dec = if (is.null(spec$dec)) "." else spec$dec,
           fileEncoding = "UTF-8-BOM")
}

#' List the course datasets
#' @return Character vector of dataset names to pass to cm_data().
#' @export
cm_datasets <- function() names(.cm_files)

#' List the lecture scripts
#' @return Character vector of script names to pass to cm_script().
#' @export
cm_scripts <- function() {
  sub("\\.R$", "", list.files(system.file("scripts", package = "customermodels"),
                              pattern = "\\.R$"))
}

#' Copy a lecture script to your working directory
#'
#' Copies the script so students can edit their own copy, and opens it in the
#' editor when running interactively. An existing file is never overwritten.
#'
#' @param name Script name, e.g. "lecture2" (see cm_scripts()).
#' @param dir Folder to copy into. Defaults to the working directory.
#' @param open Open the copied script in the editor?
#' @return The path of the copy, invisibly.
#' @examples
#' cm_scripts()
#' \dontrun{cm_script("lecture2")}
#' @export
cm_script <- function(name, dir = ".", open = interactive()) {
  name <- sub("\\.R$", "", name)
  src <- system.file("scripts", paste0(name, ".R"), package = "customermodels")
  if (src == "") {
    stop("Unknown script '", name, "'. Available: ",
         paste(cm_scripts(), collapse = ", "), call. = FALSE)
  }
  dest <- file.path(dir, paste0(name, ".R"))
  if (file.exists(dest)) {
    message(dest, " already exists, leaving it untouched.")
  } else {
    file.copy(src, dest)
    message("Copied ", name, ".R to ", normalizePath(dir))
  }
  if (open) open_in_editor(dest)
  invisible(dest)
}

# Open a file in an editor where one is available: RStudio/Positron, or the
# script editor of the standard R program on Windows (Rgui) and macOS (R.app).
# Elsewhere (terminal, VS Code, ...) just say where the file is.
open_in_editor <- function(path) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(path)
  } else if (.Platform$GUI %in% c("Rgui", "AQUA")) {
    utils::file.edit(path)
  } else {
    message("Open it in your editor: ", normalizePath(path))
  }
}
