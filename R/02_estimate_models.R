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

derived_path <- here::here("data", "derived", "analysis_data.rds")
models_path <- here::here("data", "derived", "main_models.rds")
table_path <- here::here("data", "results", "table_1_main_models.csv")

if (!file.exists(derived_path)) {
  source(here::here("R", "01_prepare_data.R"))
}

analysis_data <- readRDS(derived_path)

models <- setNames(vector("list", nrow(analysis_spec)), analysis_spec$display)
table_rows <- vector("list", nrow(analysis_spec))

for (i in seq_len(nrow(analysis_spec))) {
  spec_row <- analysis_spec[i, ]
  model <- fit_main_model(analysis_data, spec_row$variable)
  models[[spec_row$display]] <- model
  table_rows[[i]] <- extract_fixed_effects(
    model,
    spec_row$variable,
    spec_row$display
  )
}

saveRDS(models, models_path)
table_output <- dplyr::bind_rows(table_rows)
utils::write.csv(table_output, table_path, row.names = FALSE)

message("Estimated ", length(models), " mixed-effects models.")
