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

bootstrap_file <- normalizePath(
  locate_script_path(),
  winslash = "/",
  mustWork = FALSE
)
project_root <- normalizePath(
  file.path(dirname(bootstrap_file), ".."),
  winslash = "/",
  mustWork = FALSE
)
local_lib <- file.path(project_root, "data", "_r_libs")

if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

ensure_packages <- function(packages) {
  is_available <- function(package) {
    requireNamespace(package, quietly = TRUE)
  }

  missing <- packages[!vapply(packages, is_available, logical(1))]
  if (!length(missing)) {
    return(invisible(NULL))
  }

  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(local_lib, .libPaths()))

  still_missing <- missing[!vapply(missing, is_available, logical(1))]
  if (length(still_missing)) {
    install.packages(
      still_missing,
      repos = "https://cran.rstudio.com",
      lib = local_lib
    )
  }

  final_missing <- packages[!vapply(packages, is_available, logical(1))]
  if (length(final_missing)) {
    stop("Missing required packages: ", paste(final_missing, collapse = ", "))
  }

  invisible(NULL)
}

ensure_packages("here")
suppressPackageStartupMessages(library(here))
suppressMessages(here::i_am("R/00_bootstrap.R"))
project_root <- here::here()
local_lib <- file.path(project_root, "data", "_r_libs")

if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

required_packages <- c(
  "here",
  "haven",
  "dplyr",
  "ggplot2",
  "lme4",
  "patchwork",
  "reformulas",
  "scales"
)
ensure_packages(required_packages)

suppressPackageStartupMessages({
  library(here)
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(lme4)
  library(patchwork)
  library(scales)
})

options(stringsAsFactors = FALSE)

theme_teaching <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(2, 2, 2, 2)
    )
}

analysis_spec <- dplyr::tribble(
  ~variable,                 ~display,
  "wid_low50_l1",            "Bottom 50 (L1)",
  "wid_low50_d",             "Bottom 50 (D)",
  "wid_top1_l1",             "Top 1 (L1)",
  "wid_top1_d",              "Top 1 (D)",
  "wid_ratio9010_l1",        "90-10 (L1)",
  "wid_ratio9010_d",         "90-10 (D)",
  "wid_ratio9050_l1",        "90-50 (L1)",
  "wid_ratio9050_d",         "90-50 (D)",
  "swiid_mean_gini_mkt_l1",  "Gini (L1)",
  "swiid_mean_gini_mkt_d",   "Gini (D)"
)

build_model_formula <- function(variable) {
  stats::as.formula(
    paste0(
      "voe_econ_share_abs ~ ", variable, " * marpor_right4 + ",
      "newparty + marpor_niche + marpor_pervote + cpds_realgdpgr + ",
      "voe_enpv_eff + qog_cpds_vt + (1 | cty) + (1 | party)"
    )
  )
}

fit_main_model <- function(data, variable) {
  lmer(
    formula = build_model_formula(variable),
    data = data,
    REML = TRUE,
    na.action = na.exclude,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
  )
}

extract_fixed_effects <- function(model, variable, label) {
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  interaction_terms <- c(
    paste0(variable, ":marpor_right4"),
    paste0("marpor_right4:", variable)
  )

  coef_table |>
    dplyr::filter(
      .data$term %in% c(variable, "marpor_right4", interaction_terms)
    ) |>
    dplyr::mutate(
      model = label,
      std_error = .data$`Std. Error`,
      estimate = .data$Estimate,
      statistic = .data$`t value`
    ) |>
    dplyr::select(model, term, estimate, std_error, statistic)
}

prediction_grid <- function(model, data, variable, n_points = 21) {
  rng <- range(data[[variable]], na.rm = TRUE)
  grid_values <- seq(rng[1], rng[2], length.out = n_points)

  party_summaries <- data |>
    dplyr::group_by(.data$marpor_right4) |>
    dplyr::summarise(
      newparty = mean(.data$newparty, na.rm = TRUE),
      marpor_niche = mean(.data$marpor_niche, na.rm = TRUE),
      marpor_pervote = mean(.data$marpor_pervote, na.rm = TRUE),
      cpds_realgdpgr = mean(.data$cpds_realgdpgr, na.rm = TRUE),
      voe_enpv_eff = mean(.data$voe_enpv_eff, na.rm = TRUE),
      qog_cpds_vt = mean(.data$qog_cpds_vt, na.rm = TRUE),
      .groups = "drop"
    )

  newdata <- expand.grid(
    inequality_value = grid_values,
    marpor_right4 = c(0, 1),
    KEEP.OUT.ATTRS = FALSE
  ) |>
    dplyr::left_join(party_summaries, by = "marpor_right4")

  names(newdata)[names(newdata) == "inequality_value"] <- variable

  fixed_formula <- reformulas::nobars(formula(model))
  mm <- model.matrix(delete.response(terms(fixed_formula)), newdata)
  beta <- lme4::fixef(model)
  vc <- as.matrix(vcov(model))
  fit <- as.numeric(mm %*% beta)
  se <- sqrt(diag(mm %*% vc %*% t(mm)))
  crit <- qnorm(0.975)

  newdata |>
    dplyr::mutate(
      fit = fit,
      conf_low = fit - crit * se,
      conf_high = fit + crit * se,
      party_label = ifelse(.data$marpor_right4 == 0, "Left", "Right")
    )
}

prediction_at_quantile <- function(
  model,
  data,
  variable,
  party_value,
  probability
) {
  q_value <- stats::quantile(
    data[[variable]],
    probs = probability,
    na.rm = TRUE,
    names = FALSE
  ) |>
    as.numeric()

  base_row <- data |>
    dplyr::filter(.data$marpor_right4 == party_value) |>
    dplyr::summarise(
      newparty = mean(.data$newparty, na.rm = TRUE),
      marpor_niche = mean(.data$marpor_niche, na.rm = TRUE),
      marpor_pervote = mean(.data$marpor_pervote, na.rm = TRUE),
      cpds_realgdpgr = mean(.data$cpds_realgdpgr, na.rm = TRUE),
      voe_enpv_eff = mean(.data$voe_enpv_eff, na.rm = TRUE),
      qog_cpds_vt = mean(.data$qog_cpds_vt, na.rm = TRUE)
    )

  newdata <- base_row
  newdata[[variable]] <- q_value
  newdata$marpor_right4 <- party_value

  fixed_formula <- reformulas::nobars(formula(model))
  mm <- model.matrix(delete.response(terms(fixed_formula)), newdata)
  beta <- lme4::fixef(model)
  fit <- as.numeric(mm %*% beta)

  dplyr::tibble(
    variable = variable,
    marpor_right4 = party_value,
    party_label = ifelse(party_value == 0, "Left", "Right"),
    probability = probability,
    quantile_value = q_value,
    predicted_share = fit
  )
}
