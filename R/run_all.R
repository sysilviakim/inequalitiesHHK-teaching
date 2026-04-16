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
source(here::here("R", "01_prepare_data.R"))
source(here::here("R", "02_estimate_models.R"))
source(here::here("R", "03_visualize_results.R"))
source(here::here("R", "04_reproducibility_check.R"))
