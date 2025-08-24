#' simdec_visualization -
#'
#' builds SimDec visualization using data decomposition
#'
#' @param output              - an array of Y values.
#' @param inputs              - a two dimensional array of input variables (Xs).
#' @param SI                  - sensitivity indices.
#' @param output_2            - a second array of another output variable, NULL by default.
#' @param decomposition_limit - (optional) threshold of cumulative importance (sum(SI))
#'                              for selection of input variables for decomposition.
#' @param order_of_variables  - (optional) for custom decomposition specify the order
#'                              of variables for decomposition, use zero to exclude.
#'                              For example, if there are 4 input variables, and third
#'                              and second are desired for decomposition, then specify
#'                              order_of_variable as c(0,2,1,0).
#' @param number_of_states    - (optional) the number of states for each input variable,
#'                              e.g., c(0,3,2,0).
#' @param boundary_type       - (optional) 1 for 'precentile-based' (same amount of observations
#'                              in each state), 2 (default) for 'interval-based' (equaly-spaced
#'                              ranges of states).
#' @param state_boundaries    - (optional) maximums (numeric boundaries) of every state, leave
#'                              rest as NAs, e.g.,
#'                              matrix(NA, 3, -1, NA,
#'                                     NA, 5,  0, NA,
#'                                     NA, 7, NA, NA,
#'                                     byrow = TRUE)
#' @param main_colors         - (optional) hex values of the main colors, should be as many as the
#'                              states for the first in decomposition (or the most
#'                              influential input variable). For example,
#'                              main_colors <- c('#00B0F0', '#1FDF4D', '#1FDF4D')
#' @param input_names         - (optional) names of the input variables in the order of
#'                              their appearance in the original data set.
#' @param axis_title          - (optional) a title for the x axis.
#' @param plot_type           - "Stacked_Histogram by default, "Boxplot" as an alternative.
#' @param Scatter_Fraction    - the proportion of data/ points displayed on the scatter plot.
#'                              the default is 1 - the entire data is displayed in the plot.
#'                              A value of e.g. 0.5 will show every second point.
#' @param  XLim               - Minimum and maximum values for x axis c(min, max).
#' @param  YLim               - Minimum and maximum values for y axis c(min, max).
#'                              If option "Boxplot" is chosen, then YLim is ignored.
#' @param  XLim2              - Minimum and maximum values for x axis of the second histogram.
#'                              This scales the scatter plot (YLim) accordingly.
#' @param  YLim2              - Maximim and minimum valyues for the y-axis of the second histogram.
#'
#' @return scenarios          - an array of the same size as Y with scenario indices
#'                              for every simulation run
#' @return scen_legend        - a scenario table that shows which states of which
#'                              variables compose different scenarios
#' @return boundaries_out     - numeric boundaries of states of input variables
#' @return simdec_plot        - a stacked histogram (visualization)
#' @return legend_table       - a table of legends
#' @export
#'
#' @examples
#' library(Simdec)
#' data(example_data)
#' output                    <- example_data[,1]
#' inputs                    <- example_data[,2:5]
#'
#' # For automatic decomposition and visualization
#'
#' auto                      <- simdec_visualization(output, inputs, SI
#' vis_auto                  <- auto$simdec_plot
#' legend_auto               <- auto$legend_table
#'
#' # For manual decomposition and visualization
#'
#' order_of_variables_m      <- c(0, 2, 1, 0)
#' number_of_states_m        <- c(0, 3, 2, 0)
#' state_boundaries_m        <- matrix(c(NA, min(inputs[,2]), min(inputs[,3]), NA,
#'                                       NA, 100, 657.5, NA,
#'                                       NA, 650, max(inputs[,3]), NA,
#'                                       NA, max(inputs[,2]), NA, NA),
#'                                       nrow = max(number_of_states)+1,
#'                                       ncol = length(order_of_variables),
#'                                       byrow = TRUE)
#' main_colors_m             <- c('#8c5eff', '#ffe252', '#0dd189')
#'
#' manual                    <- simdec_visualization(output, inputs, SI,
#'                                                   order_of_variables = order_of_variables_m,
#'                                                   number_of_sattes   = number_of_states_m,
#'                                                   state_boundaries   = state_boundaries_m,
#'                                                   main_colors        = main_colors_m)
#' vis_manual                <- manual$simdec_plot
#' legend_manual             <- manual$legend_table
#'
#' # Boxplot example
#' rm(list = ls())
#'
#' data(example_data_2)
#' output_1       <- example_data_2[, 1]
#' output_2       <- example_data_2[, 2]
#' inputs         <- example_data_2[, 3:10]
#'
#' sen            <- sensitivity_indices(output_1, inputs)
#' SI             <- sen$SI
#'
#' simdec_boxplot <- simdec_visualization(output_1, inputs, SI, plot_type = "Boxplot")
#' simdec_boxplot$box_plot
#' simdec_boxplot$legend_table
#'
#' # Two output plot
#'
#' outputs_plot   <- simdec_visualization(output_1, inputs, SI, output_2 = output_2)
#' outputs_plot$scatter_hist
#' outputs_plot$legend_table
simdec_visualization <- function(output, inputs, SI,
                                 output_2 = NULL,
                                 decomposition_limit = 0.8 * (sum(SI)),
                                 order_of_variables = NULL,
                                 number_of_states = NULL,
                                 state_boundaries = NULL,
                                 boundary_type = 2,
                                 input_names = NULL,
                                 main_colors = NULL,
                                 axistitle = NULL,
                                 plot_type = "Stacked_Histogram",
                                 Scatter_Fraction = 1,
                                 XLim  = NULL,
                                 YLim  = NULL,
                                 XLim2 = NULL,
                                 YLim2 = NULL) {
  # ---- validate & init ----
  plot_type <- match.arg(plot_type, c("Stacked_Histogram", "Boxplot"))
  plot_1 <- plot_2 <- scatter_hist <- box <- NULL

  # 1) variable selection
  input_names <- colnames(inputs)
  if (is.null(order_of_variables)) {
    SI_sorted <- sort(SI, decreasing = TRUE)
    var_order <- order(SI, decreasing = TRUE)
    N_var_dec <- which(cumsum(SI_sorted) > decomposition_limit)[1]
    if (!is.na(N_var_dec) && N_var_dec < length(var_order)) var_order[(N_var_dec + 1):length(var_order)] <- 0
  } else {
    var_order <- rep(0, length(order_of_variables))
    for (i in seq_along(order_of_variables)) if (order_of_variables[i] > 0) var_order[order_of_variables[i]] <- i
    N_var_dec <- sum(var_order > 0)
  }
  var_names_dec <- input_names[var_order[var_order != 0]]

  # 2) states
  N_var <- ncol(inputs)
  if (is.null(number_of_states) && is.null(state_boundaries)) {
    states <- rep(0, N_var)
    states[1:N_var] <- if (N_var_dec < 3) 3 else 2
    for (f in 1:N_var) {
      n_unique <- length(unique(inputs[, f]))
      if (n_unique < 5) states[f] <- n_unique
    }
  } else states <- number_of_states
  for (f in 1:N_var) if (!(f %in% var_order[var_order != 0])) states[f] <- 0

  # 3) thresholds
  N_runs <- length(output)
  if (is.null(state_boundaries)) {
    thresholds <- matrix(NA, max(states), N_var)
    if (boundary_type == 1) {
      for (f in 1:N_var) if (states[f] > 0) {
        x <- inputs[, f]
        x_sorted <- sort(x); x_sorted <- c(x_sorted, x_sorted[length(x_sorted)] + 1)
        state_size <- round(N_runs / states[f])
        for (s in 1:(states[f] - 1)) thresholds[s, f] <- x_sorted[(state_size * s) + 1]
        thresholds[states[f], f] <- max(x) + 1
      }
    } else {
      for (f in 1:N_var) if (states[f] > 0) {
        f_min <- min(inputs[, f]); f_max <- max(inputs[, f])
        n_states <- states[f]; step <- (f_max - f_min) / n_states
        for (s in 1:n_states) thresholds[s, f] <- f_min + step * s
        thresholds[n_states, f] <- max(inputs[, f]) + 1
      }
    }
  } else thresholds <- state_boundaries[-1, ]

  # 5) scenario matrix
  N_scen <- prod(states[states > 0])
  Scen_matrix <- matrix(1, N_scen, N_var_dec + 1); Scen_matrix[, 1] <- 1:N_scen
  j <- N_scen
  for (v in 1:N_var_dec) {
    ind_inf <- var_order[v]; j <- j / states[ind_inf]; s <- 1
    for (row in 1:N_scen) {
      Scen_matrix[row, v + 1] <- s
      if (row %% j == 0) { s <- s + 1; if (s > states[ind_inf]) s <- 1 }
    }
  }

  # 6) scenario matching
  states_matching <- matrix(0, N_runs, N_var_dec); scenario_matching <- rep(0, N_runs)
  for (i in 1:N_runs) {
    for (v in 1:N_var_dec) {
      ind_inf <- var_order[v]
      for (s in 1:states[ind_inf]) if (inputs[i, ind_inf] < thresholds[s, ind_inf]) { states_matching[i, v] <- s; break }
    }
    matching_indices <- which(apply(as.matrix(Scen_matrix[, 2:(N_var_dec + 1)]), 1, function(x) all(x == states_matching[i, ])))
    if (length(matching_indices) > 0) scenario_matching[i] <- matching_indices[1]
  }
  scenarios <- scenario_matching

  # thresholds export
  if (is.null(state_boundaries)) {
    boundaries_out <- matrix(NA, max(states) + 1, N_var)
    for (f in 1:N_var_dec) {
      boundaries_out[1, var_order[f]] <- min(inputs[, var_order[f]])
      th <- thresholds[states[var_order[f]], var_order[f]]
      thresholds[states[var_order[f]], var_order[f]] <- th - 1
    }
    boundaries_out[2:(max(states) + 1), ] <- thresholds
  } else boundaries_out <- thresholds

  # scenario legend
  scen_legend <- cbind(Scen_matrix, matrix(NA, N_scen, 4))
  for (sc in 1:N_scen) {
    y <- output[scenarios == sc]
    if (!is.null(y) && length(y) > 0) {
      scen_legend[sc, (ncol(scen_legend) - 3)] <- min(y)
      scen_legend[sc, (ncol(scen_legend) - 2)] <- mean(y)
      scen_legend[sc, (ncol(scen_legend) - 1)] <- max(y)
      scen_legend[sc, ncol(scen_legend)]     <- length(y) / N_runs
    }
  }
  scenario_legend <- scen_legend

  # colors
  if (is.null(main_colors)) main_colors <- c("#DC267F","#E8EA2F","#26DCD1","#C552E4","#3F45D0")
  N_main_colors <- max(scenario_legend[, 2])
  N_shades <- scenario_legend[nrow(scenario_legend), 1] / N_main_colors
  color <- rep(NA_character_, scenario_legend[nrow(scenario_legend), 1])
  sc <- 1; range <- if (N_shades >= 3) 0.4 else 0.5
  if (N_shades == 1) beta <- 0.5 else { step <- range * 2 / (N_shades - 1); beta <- seq(-range, range, by = step) }
  if (!requireNamespace("colorspace", quietly = TRUE)) stop("Package 'colorspace' is required.")
  for (m in 1:N_main_colors) for (sh in 1:N_shades) { color[sc] <- colorspace::lighten(main_colors[m], amount = (beta[ifelse(N_shades == 1, 1, sh)] + sh * 0.1)); sc <- sc + 1 }
  color_inv <- rev(color)

  # libs
  pkgs <- c("ggplot2","gridExtra","grid","dplyr","tidyr","kableExtra","cowplot")
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("Package '%s' is required.", p))
  library(ggplot2); library(dplyr); library(tidyr); library(kableExtra); library(cowplot)

  # themes
  tight <- theme(plot.margin = margin(0,0,0,0), panel.spacing = unit(0, "pt"))
  hist_no_axes <- theme(axis.text=element_blank(), axis.ticks=element_blank(),
                        axis.title=element_blank())
  no_legend <- theme(legend.position = "none")
  no_guides <- guides(fill = "none", color = "none", linetype = "none", shape = "none")

  bins <- 100

  # ---- plotting ----
  if (plot_type == "Stacked_Histogram") {

    if (is.null(output_2)) {
      # single output: honor XLim & YLim (and CLIP values before hist)
      xlim_main <- if (is.null(XLim)) range(output) else sort(as.numeric(XLim))
      if (diff(xlim_main) <= 0) xlim_main <- range(output)

      edges1 <- seq(xlim_main[1], xlim_main[2], length.out = bins + 1L)
      mids1  <- head(edges1, -1) + diff(edges1)[1]/2

      fmat1 <- matrix(0, nrow = max(scenarios), ncol = bins)
      for (i in 1:max(scenarios)) {
        xi <- output[scenarios == i]
        xi <- xi[xi >= xlim_main[1] & xi <= xlim_main[2]]        # <<< CLIP
        if (length(xi)) {
          fmat1[i, ] <- hist(xi, breaks = edges1, include.lowest = TRUE, right = TRUE, plot = FALSE)$counts
        } else {
          fmat1[i, ] <- 0
        }
      }

      df_counts1  <- data.frame(x = mids1, t(fmat1))
      scen_names1 <- colnames(df_counts1)[-1]
      df_long1 <- df_counts1 |>
        tidyr::pivot_longer(cols = -x, names_to = "scenario", values_to = "count") |>
        dplyr::mutate(scenario = factor(scenario, levels = rev(scen_names1)))
      fill_vals1 <- setNames(color_inv, rev(scen_names1))

      yticks1 <- pretty(c(0, max(colSums(fmat1))), n = 10)
      if (!is.null(YLim)) {
        yL <- as.numeric(YLim); if (length(yL) == 1) yL <- c(0, yL)
        ylim_counts <- length(output) * yL/100
      } else ylim_counts <- NULL

      b <- ggplot(df_long1, aes(x = x, y = count, fill = scenario)) +
        geom_col(position = "stack", colour = "black", width = diff(edges1)[1]) +
        scale_fill_manual(values = fill_vals1) +
        scale_x_continuous(limits = xlim_main, expand = c(0,0)) +
        scale_y_continuous(breaks = yticks1,
                           labels = sprintf("%.0f%%", 100*yticks1/length(output)),
                           limits = ylim_counts, expand = c(0,0)) +
        labs(x = "Y", y = "Probability") +
        theme_minimal() + tight + no_legend + no_guides
      plot_1 <- b

    } else {
      # two-output aligned panel: honor XLim, XLim2, YLim, YLim2
      xlim_main  <- if (is.null(XLim))  range(output)  else sort(as.numeric(XLim))
      if (diff(xlim_main) <= 0) xlim_main <- range(output)
      xlim_right <- if (is.null(XLim2)) range(output_2) else sort(as.numeric(XLim2))
      if (diff(xlim_right) <= 0) xlim_right <- range(output_2)

      # TOP HIST (Output1)
      edges1 <- seq(xlim_main[1], xlim_main[2], length.out = bins + 1L)
      mids1  <- head(edges1, -1) + diff(edges1)[1]/2
      fmat1 <- matrix(0, nrow = max(scenarios), ncol = bins)
      for (i in 1:max(scenarios)) {
        xi <- output[scenarios == i]
        xi <- xi[xi >= xlim_main[1] & xi <= xlim_main[2]]          # <<< CLIP
        if (length(xi)) {
          fmat1[i, ] <- hist(xi, breaks = edges1, include.lowest = TRUE, right = TRUE, plot = FALSE)$counts
        } else {
          fmat1[i, ] <- 0
        }
      }
      df_counts1  <- data.frame(x = mids1, t(fmat1))
      scen_names1 <- colnames(df_counts1)[-1]
      df_long1 <- df_counts1 |>
        tidyr::pivot_longer(cols = -x, names_to = "scenario", values_to = "count") |>
        dplyr::mutate(scenario = factor(scenario, levels = rev(scen_names1)))
      fill_vals1 <- setNames(color_inv, rev(scen_names1))
      yticks1 <- pretty(c(0, max(colSums(fmat1))), n = 10)
      if (!is.null(YLim)) { yL <- as.numeric(YLim); if (length(yL)==1) yL <- c(0,yL); ylim_counts <- length(output)*yL/100 } else ylim_counts <- NULL

      top_hist <- ggplot(df_long1, aes(x = x, y = count, fill = scenario)) +
        geom_col(position = "stack", colour = "black", width = diff(edges1)[1]) +
        scale_fill_manual(values = fill_vals1) +
        scale_x_continuous(limits = xlim_main, expand = c(0,0)) +
        scale_y_continuous(breaks = yticks1,
                           labels = sprintf("%.0f%%", 100*yticks1/length(output)),
                           limits = ylim_counts, expand = c(0,0)) +
        theme_minimal() + tight + hist_no_axes + no_legend + no_guides

      # RIGHT HIST (Output2)
      edges2 <- seq(xlim_right[1], xlim_right[2], length.out = bins + 1L)
      mids2  <- head(edges2, -1) + diff(edges2)[1]/2
      fmat2 <- matrix(0, nrow = max(scenarios), ncol = bins)
      for (i in 1:max(scenarios)) {
        yi <- output_2[scenarios == i]
        yi <- yi[yi >= xlim_right[1] & yi <= xlim_right[2]]        # <<< CLIP
        if (length(yi)) {
          fmat2[i, ] <- hist(yi, breaks = edges2, include.lowest = TRUE, right = TRUE, plot = FALSE)$counts
        } else {
          fmat2[i, ] <- 0
        }
      }
      df_counts2  <- data.frame(x = mids2, t(fmat2))
      scen_names2 <- colnames(df_counts2)[-1]
      df_long2 <- df_counts2 |>
        tidyr::pivot_longer(cols = -x, names_to = "scenario", values_to = "count") |>
        dplyr::mutate(scenario = factor(scenario, levels = rev(scen_names2)))
      fill_vals2 <- setNames(color_inv, rev(scen_names2))
      yticks2 <- pretty(c(0, max(colSums(fmat2))), n = 10)
      if (!is.null(YLim2)) { yL2 <- as.numeric(YLim2); if (length(yL2)==1) yL2 <- c(0,yL2); ylim2_counts <- length(output_2)*yL2/100 } else ylim2_counts <- NULL

      right_hist <- ggplot(df_long2, aes(x = x, y = count, fill = scenario)) +
        geom_col(position = "stack", colour = "black", width = diff(edges2)[1]) +
        scale_fill_manual(values = fill_vals2) +
        scale_x_continuous(limits = xlim_right, expand = c(0,0)) +
        scale_y_continuous(breaks = yticks2,
                           labels = sprintf("%.0f%%", 100*yticks2/length(output_2)),
                           limits = ylim2_counts, expand = c(0,0)) +
        theme_minimal() + tight + hist_no_axes + no_legend + no_guides
      right_hist <- suppressMessages(right_hist + coord_flip())

      # SCATTER
      step <- max(1, round(1 / Scatter_Fraction)); idx <- seq(1, length(output), by = step)
      df_scatter <- data.frame(X = output[idx], Y = output_2[idx],
                               Scenario = factor(scenarios[idx], levels = 1:max(scenarios)))
      s <- ggplot(df_scatter, aes(X, Y, color = Scenario)) +
        geom_point(size = 2) +
        scale_color_manual(values = color) +
        scale_x_continuous(limits = xlim_main,  expand = c(0,0)) +
        scale_y_continuous(limits = xlim_right, expand = c(0,0)) +
        labs(x = "Output1", y = "Output2") +
        theme_minimal() + tight + no_legend + no_guides

      # ALIGN
      v_al <- cowplot::align_plots(top_hist, s, align = "v", axis = "lr")
      top_hist_a <- v_al[[1]]; s_a <- v_al[[2]]
      h_al <- cowplot::align_plots(s_a, right_hist, align = "h", axis = "tb")
      s_a2 <- h_al[[1]]; right_hist_a <- h_al[[2]]

      scatter_hist <- cowplot::plot_grid(
        cowplot::plot_grid(top_hist_a, NULL, ncol = 2, rel_widths = c(2,1)),
        cowplot::plot_grid(s_a2, right_hist_a, ncol = 2, rel_widths = c(2,1)),
        nrow = 2, rel_heights = c(1,2)
      )
      plot_1 <- top_hist; plot_2 <- right_hist
    }
  }

  # ---- Boxplot ----
  if (plot_type == "Boxplot") {
    N_scen <- max(scen_legend[, 1])
    output_fixed <- output; scenarios_fixed <- scenarios
    missing <- setdiff(1:N_scen, unique(scenarios))
    if (length(missing) > 0) {
      artificial_value <- max(output) + 1
      output_fixed <- c(output_fixed, rep(artificial_value, length(missing)))
      scenarios_fixed <- c(scenarios_fixed, missing)
    }
    df_box <- data.frame(output = output_fixed, scenario = factor(scenarios_fixed, levels = 1:N_scen))
    box <- ggplot(df_box, aes(x = scenario, y = output, fill = scenario)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 1.8, color = "black") +
      coord_flip() +
      scale_fill_manual(values = color) +
      labs(x = "Scenario", y = "Output") +
      theme_minimal() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    if (!is.null(XLim)) box <- box + scale_y_continuous(limits = sort(as.numeric(XLim)))
  }

  # ---- legend table ----
  legend_data <- as.data.frame(scenario_legend)
  legend_data[, 1] <- as.factor(legend_data[, 1])
  colnames(legend_data) <- c("Color", var_names_dec, "min(Y)", "mean(Y)", "max(Y)", "probability")
  stats_y <- c("min(Y)", "mean(Y)", "max(Y)")
  for (varname in var_names_dec) {
    uv <- unique(legend_data[[varname]])
    if (length(uv) == 2) {
      legend_data[[varname]][legend_data[[varname]] == 1] <- "low"
      legend_data[[varname]][legend_data[[varname]] == 2] <- "high"
    } else if (length(uv) == 3) {
      legend_data[[varname]][legend_data[[varname]] == 1] <- "low"
      legend_data[[varname]][legend_data[[varname]] == 2] <- "medium"
      legend_data[[varname]][legend_data[[varname]] == 3] <- "high"
    }
  }
  if (length(var_names_dec) >= 1) {
    merge_var <- var_names_dec[[1]]
    legend_data[[merge_var]] <- ifelse(duplicated(legend_data[[merge_var]]), "", legend_data[[merge_var]])
  }
  legend_data <- legend_data %>% dplyr::mutate(dplyr::across(dplyr::all_of(stats_y), ~ round(., 0)))
  legend_data$probability <- paste0(round(legend_data$probability * 100), "%")
  legend_table <- legend_data %>%
    kable(escape = FALSE, format = "html", align = 'c') %>%
    kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center", font_size = 12) %>%
    column_spec(1, color = "black", background = color, width = "6em") %>%
    collapse_rows(columns = 2, valign = "middle") %>%
    column_spec(2, bold = TRUE)

  # ---- return ----
  list(
    scenarios      = scenarios,
    scen_legend    = scen_legend,
    boundaries_out = boundaries_out,
    simdec_plot    = plot_1,
    simdec_plot_2  = plot_2,
    scatter_hist   = scatter_hist,
    legend_table   = legend_table,
    box_plot       = box
  )
}








