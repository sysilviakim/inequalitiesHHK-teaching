locate_script_path <- function() {
  frames <- sys.frames()

  if (length(frames) && !is.null(frames[[1]]$ofile)) {
    return(frames[[1]]$ofile)
  }

  file_arg <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )

  if (length(file_arg)) {
    return(sub("^--file=", "", file_arg[[1]]))
  }

  stop("Could not locate the current script path.")
}

script_dir <- dirname(normalizePath(locate_script_path()))
source(file.path(script_dir, "00_bootstrap.R"))

raw_path <- here::here(
  "data",
  "raw",
  "whyinequalitiespersist_replication.dta"
)
validation_path <- here::here(
  "data",
  "raw",
  "whyinequalitiespersist_validation_replication.dta"
)
derived_path <- here::here("data", "derived", "analysis_data.rds")
validation_rds <- here::here("data", "derived", "validation_data.rds")
summary_path <- here::here(
  "data",
  "results",
  "analysis_sample_summary.csv"
)

dir.create(dirname(derived_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)

analysis_data <- haven::read_dta(raw_path) |>
  dplyr::mutate(
    cty = factor(.data$cty),
    party = factor(.data$party),
    marpor_right4 = as.numeric(.data$marpor_right4)
  )

saveRDS(analysis_data, derived_path)

validation_data <- haven::read_dta(validation_path)
saveRDS(validation_data, validation_rds)

sample_summary <- dplyr::tibble(
  observations = nrow(analysis_data),
  countries = dplyr::n_distinct(analysis_data$cty),
  parties = dplyr::n_distinct(analysis_data$party),
  min_year = min(analysis_data$year, na.rm = TRUE),
  max_year = max(analysis_data$year, na.rm = TRUE)
)

utils::write.csv(sample_summary, summary_path, row.names = FALSE)

message("Prepared analysis data: ", derived_path)
