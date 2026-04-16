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
plot_path <- here::here("data", "results", "figure_2_reproduction.png")

if (!file.exists(derived_path)) {
  source(here::here("R", "01_prepare_data.R"))
}

if (!file.exists(models_path)) {
  source(here::here("R", "02_estimate_models.R"))
}

analysis_data <- readRDS(derived_path)
models <- readRDS(models_path)

panel_order <- c(
  "Bottom 50 (L1)", "Bottom 50 (D)",
  "Top 1 (L1)", "Top 1 (D)",
  "90-10 (L1)", "90-10 (D)",
  "90-50 (L1)", "90-50 (D)",
  "Gini (L1)", "Gini (D)"
)

panel_plots <- vector("list", length(panel_order))

for (i in seq_along(panel_order)) {
  label <- panel_order[[i]]
  spec_row <- analysis_spec |> dplyr::filter(.data$display == label)
  variable <- spec_row$variable[[1]]
  pred <- prediction_grid(models[[label]], analysis_data, variable)
  hist_data <- analysis_data |> dplyr::filter(!is.na(.data[[variable]]))

  hist_obj <- hist(hist_data[[variable]], plot = FALSE, breaks = 20)
  hist_df <- dplyr::tibble(
    xmin = head(hist_obj$breaks, -1),
    xmax = tail(hist_obj$breaks, -1),
    count = hist_obj$counts
  )

  max_hist <- max(hist_df$count, na.rm = TRUE)
  pred_limits <- range(c(pred$conf_low, pred$conf_high), na.rm = TRUE)
  y_min <- min(0, pred_limits[1], na.rm = TRUE)
  y_max <- max(pred_limits[2], na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span == 0) {
    y_span <- 1
  }

  hist_df <- hist_df |>
    dplyr::mutate(
      scaled_count = y_min + (.data$count / max_hist) * y_span * 0.35
    )

  p <- ggplot() +
    geom_rect(
      data = hist_df,
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = y_min,
        ymax = .data$scaled_count
      ),
      fill = "grey60",
      alpha = 0.25,
      color = NA
    ) +
    geom_ribbon(
      data = pred,
      aes(
        x = .data[[variable]],
        ymin = .data$conf_low,
        ymax = .data$conf_high,
        fill = .data$party_label
      ),
      alpha = 0.18,
      color = NA
    ) +
    geom_line(
      data = pred,
      aes(x = .data[[variable]], y = .data$fit, color = .data$party_label),
      linewidth = 0.7
    ) +
    scale_color_manual(values = c("Left" = "#1b5e20", "Right" = "#263238")) +
    scale_fill_manual(values = c("Left" = "#1b5e20", "Right" = "#263238")) +
    scale_y_continuous(
      labels = label_number(accuracy = 0.1),
      sec.axis = sec_axis(
        transform = ~ (. - y_min) / (y_span * 0.35) * max_hist,
        name = "Frequency"
      )
    ) +
    labs(
      x = label,
      y = "Predicted % statements",
      title = label,
      color = NULL,
      fill = NULL
    ) +
    theme_teaching() +
    theme(
      legend.position = if (i == 1) "bottom" else "none",
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      axis.text = element_text(size = 6),
      plot.title = element_text(size = 7, face = "bold"),
      plot.margin = margin(2, 2, 2, 2)
    )

  panel_plots[[i]] <- p
}

combined_plot <- wrap_plots(panel_plots, ncol = 2) +
  plot_annotation(
    title = paste(
      "Figure 2 Reproduction:",
      "Economic Equality Statements",
      "Conditional on Partisanship"
    ),
    theme = theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(
        size = 9,
        face = "bold",
        color = "black"
      ),
      plot.margin = margin(2, 2, 2, 2)
    )
  )

ggsave(
  filename = plot_path,
  plot = combined_plot,
  width = 7,
  height = 11,
  dpi = 300,
  bg = "transparent"
)

message("Saved Figure 2 reproduction: ", plot_path)
