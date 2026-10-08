required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "plotly", "DT", "tidytext")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing)) {
  stop("Could not install required package(s): ", paste(still_missing, collapse = ", "))
}

cat("All required R packages are available.\n")
