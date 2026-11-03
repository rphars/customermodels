# customermodels

Everything for the R part of the **Customer Models** course in one package:
the R packages used in the lectures, the course datasets, and the lecture
scripts. A weekly automated check makes sure all lecture models still run and
still give the same results.

## For students

Install once (this also installs every package used in the course):

```r
install.packages("pak")
pak::pak("rphars/customermodels")
```

Then, in every session:

```r
library(customermodels)         # loads all course packages

cm_script("lecture2")           # copies the lecture script to your folder and opens it
data <- cm_data("telco_churn")  # loads a dataset, no setwd() needed
```

`cm_datasets()` and `cm_scripts()` list what's available.

The lecture 5 alternative using **countreg** needs one extra install, because
countreg is not on CRAN:

```r
install.packages("countreg", repos = "https://R-Forge.R-project.org")
```

To install the version for a specific course year, add its tag, for example
`pak::pak("rphars/customermodels@v2026")`.

## For the teacher

### What gets checked

Every Monday (and on every push), GitHub Actions installs the latest R and the
latest versions of all course packages, and then:

1. **Re-estimates every lecture model** and compares coefficients,
   log-likelihoods and other key numbers with stored reference values
   (`tests/testthat/reference/*.json`). This catches renamed functions, changed
   defaults, archived packages, and estimates that quietly shift.
2. **Runs every lecture script** top to bottom, the way a student would.
   `View()` and the shiny apps only run interactively, so they are skipped.
3. **Checks that no package masks the wrong function.** VGAM exports its own
   `tobit()` and `lrtest()`, so it is attached first; a test guards that
   `tobit()` is AER's and `lrtest()` is lmtest's.

The same checks also run on R-devel as an early warning for the next R
release. That job may fail without failing the whole run.

When something breaks, GitHub emails you and the run log shows which model or
script failed, and which number changed.

### Where things live

| Path | What |
|---|---|
| `DESCRIPTION` | the course packages (`Depends` = loaded with `library(customermodels)`) |
| `inst/extdata/` | course datasets |
| `inst/scripts/` | lecture scripts |
| `tests/testthat/helper-models.R` | every lecture model, in one list |
| `tests/testthat/reference/` | stored results the tests compare against |
| `data-raw/make_reference.R` | creates or refreshes the reference values |
| `.github/workflows/course-check.yml` | the weekly check |

### Common tasks

**Create or refresh the reference values** (from the package folder, on your
own machine with all packages installed):

```bash
Rscript data-raw/make_reference.R                     # all models
Rscript data-raw/make_reference.R lecture4_heckman    # just one
```

Only refresh a reference after checking that the new result is correct (for
example because a package fixed a bug). `reference/_info.json` records the R
and package versions the values came from.

**Add a model:** add an entry to `cm_reference_models` in
`tests/testthat/helper-models.R`, run `make_reference.R` for it, and commit the
new JSON file.

**Add a package:** add it under `Depends` in `DESCRIPTION` (or `Imports` if it
shouldn't be loaded automatically). If it exports a function with the same
name as another course package, check `tests/testthat/test-masking.R`.

**Add a dataset:** put the csv in `inst/extdata/` and add a line to `.cm_files`
in `R/cm_data.R` (with `sep`/`dec` if it isn't a plain comma-separated file).

**New course year:** tag a release, e.g. `git tag v2027 && git push --tags`,
so students on an older edition keep a version that doesn't change.

### Good to know

GitHub pauses scheduled workflows in a repository with no activity for
60 days. If the weekly run stops, any commit or a click on
**Actions → course-check → Enable workflow** starts it again.
