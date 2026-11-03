# customermodels

Everything for the R part of the **Customer Models** course in one package:
the R packages used in the lectures, the course datasets, and the lecture
scripts.

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
