# Run every lecture script from top to bottom, the way a student would.
# Interactive-only parts (View(), the shiny apps) are skipped automatically,
# because tests never run interactively.

script_packages <- list(
  lecture2          = "mfx",
  lecture3          = "mlogit",
  lecture4          = c("AER", "sampleSelection"),
  lecture5          = c("AER", "MASS", "VGAM"),
  lecture5_countreg = c("countreg", "AER", "MASS"),
  lecture6          = c("survival", "survminer", "Hmisc")
)

for (nm in cm_scripts()) {
  local({
    name <- nm
    test_that(paste("script", name, "runs without errors"), {
      skip_on_cran()
      for (p in script_packages[[name]]) skip_if_not_installed(p)
      path <- system.file("scripts", paste0(name, ".R"), package = "customermodels")

      grDevices::pdf(NULL)                 # send plots nowhere
      on.exit(grDevices::dev.off(), add = TRUE)
      out <- utils::capture.output(
        suppressWarnings(suppressMessages(
          source(path, local = new.env(), echo = FALSE)
        ))
      )
      succeed()
    })
  })
}
