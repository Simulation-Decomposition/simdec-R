#' simdec_visualization -
#'
#' builds SimDec visualization using data decomposition
#'
#' @param output              - an array of Y values.
#' @param inputs              - a two dimensional array of input variables (Xs).
#' @param SI                  - sensitivity indices.
#'
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
simdec_visualization <- function(output, input, SI,
                                 decomposition_limit = 0.8*(sum(SI)),
                                 order_of_variables=NULL,
                                 number_of_states=NULL,
                                 state_boundaries=NULL,
                                 boundary_type=2,
                                 input_names = NULL,
                                 main_colors = NULL,
                                 axistitle = NULL) {


  # 1. Variables for decomposition

  input_names <- colnames(inputs)

  if (is.null(order_of_variables)) {
    SI_sorted <- sort(SI, decreasing = TRUE)
    var_order <- order(SI, decreasing = TRUE)
    N_var_dec <- which(cumsum(SI_sorted) > decomposition_limit)[1]
    var_order[(N_var_dec + 1):length(var_order)] <- 0
  } else {
    var_order <- rep(0, length(order_of_variables))
    for (i in 1:length(order_of_variables)) {
      if (order_of_variables[i] > 0) {
        var_order[order_of_variables[i]] <- i
      }
    }
    N_var_dec <- sum(var_order > 0)
  }

  var_names_dec <- input_names[var_order[var_order != 0]]

  # 2. States formation

  N_var <- ncol(inputs)
  if (is.null(number_of_states) && is.null(state_boundaries)) {
    states <- rep(0, N_var)
    if (N_var_dec < 3) {
      states[1:N_var] <- 3
    } else {
      states[1:N_var] <- 2
    }

    for (f in 1:N_var) {
      n_unique <- length(unique(inputs[, f]))
      if (n_unique < 5) {
        states[f] <- n_unique
      }
    }
  } else {
    # states <- ifelse(is.null(number_of_states), 0, number_of_states)
    states <- number_of_states
  }



  for (f in 1:N_var) {
    if (f %in% var_order[var_order != 0]) {
      next
    } else {
      states[f] <- 0
    }
  }

  # 3. Numeric thresholds

  N_runs <- length(output)

  if (is.null(state_boundaries)) {
    thresholds <- matrix(NA, max(states), N_var)
    if (boundary_type == 1) {
      for (f in 1:N_var) {
        if (states[f] > 0) {
          x <- inputs[, f]
          x_sorted <- sort(x)
          x_sorted <- c(x_sorted, x_sorted[length(x_sorted)] + 1)
          min_threshold <- x_sorted[1]
          state_size <- round(N_runs / states[f])
          for (s in 1:(states[f] - 1)) {
            thresholds[s, f] <- x_sorted[(state_size * s) + 1]
          }
          thresholds[states[f], f] <- max(x) + 1
        }
      }
    } else {
      for (f in 1:N_var) {
        if (states[f] > 0) {
          f_min <- min(inputs[, f])
          f_max <- max(inputs[, f])
          n_states <- states[f]
          step <- (f_max - f_min) / n_states
          for (s in 1:n_states) {
            thresholds[s, f] <- f_min + step * s
          }
          thresholds[n_states, f] <- max(inputs[, f]) + 1
        }
      }
    }
  } else {
    thresholds <- state_boundaries[-1, ]
  }

  # 5. Scenario matrix

  N_scen <- prod(states[states > 0])
  Scen_matrix <- matrix(1, N_scen, N_var_dec + 1)
  Scen_matrix[, 1] <- 1:N_scen
  j <- N_scen

  for (v in 1:N_var_dec) {
    ind_inf <- var_order[v]
    j <- j / states[ind_inf]
    s <- 1
    for (row in 1:N_scen) {
      Scen_matrix[row, v + 1] <- s
      if (row %% j == 0) {
        s <- s + 1
        if (s > states[ind_inf]) {
          s <- 1
        }
      }
    }
  }


  states_matching <- matrix(0, N_runs, N_var_dec)
  scenario_matching <- rep(0, N_runs)

  for (i in 1:N_runs) {
    for (v in 1:N_var_dec) {
      ind_inf <- var_order[v]
      for (s in 1:states[ind_inf]) {
        if (inputs[i, ind_inf] < thresholds[s, ind_inf]) {
          states_matching[i, v] <- s
          break
        }
      }
    }

    matching_indices <- which(apply(as.matrix(Scen_matrix[, 2:(N_var_dec + 1)]), 1, function(x) all(x == states_matching[i, ])))
    if (length(matching_indices) > 0) {
      scenario_matching[i] <- matching_indices[1]
    }
  }

  scenarios <- scenario_matching

  # Adding min values to thresholds for export

  if (is.null(state_boundaries)) {
    boundaries_out <- matrix(NA, max(states) + 1, N_var)
    for (f in 1:N_var_dec) {
      boundaries_out[1, var_order[f]] <- min(inputs[, var_order[f]])
      th <- thresholds[states[var_order[f]], var_order[f]]
      thresholds[states[var_order[f]], var_order[f]] <- th - 1
    }
    boundaries_out[2:(max(states) + 1), ] <- thresholds
  } else {
    boundaries_out <- thresholds
  }

  # 7. Filling scenario legend

  scen_legend <- cbind(Scen_matrix, matrix(NA, N_scen, 4))
  for (sc in 1:N_scen) {
    y <- output[scenarios == sc]
    if (!is.null(y) && length(y) > 0) {
      scen_legend[sc, (ncol(scen_legend) - 3)] <- min(y)
      scen_legend[sc, (ncol(scen_legend) - 2)] <- mean(y)
      scen_legend[sc, (ncol(scen_legend) - 1)] <- max(y)
      scen_legend[sc, ncol(scen_legend)] <- length(y) / N_runs
    }
  }

  scenario_legend <- scen_legend

  # Separating result by scenario
  result_dec <- vector("list", max(scenarios))

  for (i in 1:max(scenarios)) {
    s <- length(output[scenarios == i])
    result_dec[[i]] <- matrix(output[scenarios == i], nrow = s, ncol = 1)
  }

  # Defining edges of bins
  l <- min(output)
  h <- max(output)
  bins <- 100
  edges <- seq(l, h, length.out = bins + 1)

  # Define frequency of each scenario NPV for each bin
  f <- matrix(0, nrow = max(scenarios), ncol = 100)

  for (i in 1:max(scenarios)) {
    f[i, ] <- hist(result_dec[[i]], breaks = edges, plot = FALSE)$counts
  }
  # Main colors
  if (is.null(main_colors)) {
    main_colors <- c("#00B0F0", "#E7D819", "#1FDF4D", "#ed4284", "#9054de")
  }



  N_main_colors <- max(scenario_legend[,2])
  N_shades <- scenario_legend[nrow(scenario_legend),1] / N_main_colors
  color <- matrix(NA, nrow = scenario_legend[nrow(scenario_legend),1], ncol = 3)


  sc <- 1
  if (N_shades >= 3) {
    range <- 0.4
  } else {
    range <- 0.5
  }
  step <- range * 2 / (N_shades - 1)
  if (N_shades == 1) {
    step <- 0.5
    beta <- -range + step
  } else {
    step <- range * 2 / (N_shades - 1)
    beta <- seq(-range, range, by = step)
  }

  library(colorspace)

  for (m in 1:N_main_colors) {
    current_main_color <- main_colors[m]

    for (sh in 1:N_shades) {
      color[sc, ] <- lighten(current_main_color, amount = (beta[sh]+sh*0.1))   # Made changes to differentiate between medium and light shades
      sc <- sc + 1
    }
  }

  color_inv <- color[nrow(color):1, ]  # Created the color_inv matrix for the plot

  library(ggplot2)
  library(gridExtra)
  library(dplyr)

  # Create a data frame with the required data
  df <- data.frame(x = 1:bins, t(f))


  # Convert the data frame to long format using tidyr::pivot_longer
  df_long <- tidyr::pivot_longer(df, cols = -x, names_to = "variable", values_to = "value")

  # Plotting the histogram
  h <- hist(output, breaks = bins, plot = FALSE)
  xticks <- pretty(h$breaks, n = 10)

  # Adjusting x-ticks
  xtick_step <- xticks[2] - xticks[1]

  if (max(h$breaks) > max(xticks)) {
    xticks <- c(xticks, xticks[length(xticks)] + xtick_step)
  }

  if (min(h$breaks) < min(xticks)) {
    xticks <- c(xticks[1] - xtick_step, xticks)
  }

  # Getting y-ticks
  yticks <- pretty(seq(0, max(h$counts)), n = 10)

  min_art <- (xticks[1] - min(output)) * bins / (max(output) - min(output))
  max_art <- (xticks[length(xticks)] - max(output)) * bins / (max(output) - min(output)) + bins
  distance_art <- max_art - min_art
  xticks_art <- (xticks - xticks[1]) * distance_art / (xticks[length(xticks)] - xticks[1]) + min_art

  total <- length(output)
  yticks_art <- yticks / total
  a <- ifelse(yticks_art %% 1 == 0 & yticks_art != 0,
              paste0(formatC(yticks_art * 100, digits = 0, format = "f")),
              paste0(formatC(yticks_art * 100, digits = 1, format = "f")))
  new_yticks <- paste0(a, "%")

  df_long$variable <- factor(df_long$variable, ordered = TRUE, levels = rev(colnames(df[,-1]))) # changed the order here


  # Create the plot
  b <- ggplot(df_long, aes(x = x, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack", colour="black") +
    scale_x_continuous(breaks = xticks_art, labels = xticks) +
    scale_y_continuous(breaks = yticks, labels = new_yticks) +
    xlab("Y") +
    ylab("Probability") + scale_fill_manual(values = color_inv) +          # Used the color_inv matrix here for matching matlab
    coord_cartesian(xlim = c(min(xticks_art), max(xticks_art))) +
    theme(legend.position="none") +
    labs(title = "SimDec Plot") +
    theme(plot.title = element_text(hjust = 0.5))

  # Load the required libraries
  library(kableExtra)

  # Create a data frame for the legend
  legend_data <- as.data.frame(scenario_legend)
  legend_data[,1] <- as.factor(legend_data[,1])
  colnames(legend_data) <- c("Color", var_names_dec, "min(Y)", "mean(Y)", "max(Y)", "probability")
  stats_y <- c("min(Y)", "mean(Y)", "max(Y)")


  ### State Labels ###

  # For loop to iterate over variable names
  for (varname in var_names_dec) {
    unique_values <- unique(legend_data[[varname]])

    if (length(unique_values) == 2) {
      if (1 %in% unique_values) {
        legend_data[[varname]][legend_data[[varname]] == 1] <- "low"
      }
      if (2 %in% unique_values) {
        legend_data[[varname]][legend_data[[varname]] == 2] <- "high"
      }
    } else if (length(unique_values) == 3) {
      if (1 %in% unique_values) {
        legend_data[[varname]][legend_data[[varname]] == 1] <- "low"
      }
      if (2 %in% unique_values) {
        legend_data[[varname]][legend_data[[varname]] == 2] <- "medium"
      }
      if (3 %in% unique_values) {
        legend_data[[varname]][legend_data[[varname]] == 3] <- "high"
      }
    }
  }

  # Define the column name for merging and aligning cells NEW
  merge_var <- var_names_dec[[1]]

  # Merge the cells in the specified column NEW
  legend_data[[merge_var]] <- ifelse(duplicated(legend_data[[merge_var]]), "", legend_data[[merge_var]])

  legend_data <- legend_data %>% mutate(across(all_of(stats_y), ~ round(., digits = 0)))

  # Displaying the percentage sign, and rounding the rows in the probability column
  legend_data$probability <- paste0(round(legend_data$probability * 100), "%")


  #Use kableExtra to generate the table with styled cells
  legend_table <- legend_data %>%
    kable(escape = FALSE, format = "html", align = 'c') %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "center",font_size = 12) %>%
    column_spec(1, color = "black", background = color[,1], width = "6em") %>%
    collapse_rows(columns = 2, valign = "middle") %>% column_spec(2, bold = TRUE) #NEW

  # Display the table
  legend_table

  return(list(scenarios = scenarios,
              scen_legend = scen_legend,
              boundaries_out = boundaries_out,
              simdec_plot = b,
              legend_table = legend_table))
}








