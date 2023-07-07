#' Decomposition -
#'
#' This functions creates the scenarios and maps them onto output values.
#'
#' @param output             - a vector of the target variable (Y)
#' @param inputs             - a matrix of input variables
#' @param SI                 - sensitivity indices
#' @param dec_limit          - threshold of cumulative significance for selection
#'                             of input variables for decomposition
#' @param manual_vars        - (optional) for custom decomposition specify the order
#'                             of variables for decomposition, use zero to exclude.
#'                             For example, if 4 input variables, third and second
#'                             are desired for decomposition, then
#'                             manual_vars <- c(0 2 1 0).
#' @param manual_states      - (optional) the number of states for each input
#'                             variable, i.e. c(0 3 2 0)
#' @param manual_thresholds  - manual_thresholds - [optional] maximums (numeric thresholds)
#'                             of every state, leave the rest as NA, e.g.
#'                             matrix(NA, 3, -1, NA,
#'                                    NA, 5,  0, NA,
#'                                    NA, 7, NA, NA, byrow = TRUE)
#' @param threshold_type     - 1 for 'precentile-based' (same amount of observations in each state),
#'                             2 for 'median-based' (equaly-spaced ranges of states)
#' @param var_names          - names of input variables
#'
#' @return scenarios         - an array of the same size as Y with scenario indices
#'                             for every simulation run.
#' @return scen_legend       - a scenario table that shows which states of which
#'                             variables compose different sceanrios
#' @return thresholds_out    - thresholds of states of input variables
#' @return var_names_dec     - a cell array with sorted input variables' names
#' @export
#'
#' @examples
#' df <- data.frame(Y  <- c(167, 82, 75, 134, 186, 51, 17, 167, 86, 198),
#'                  X1 <- c(10, 20, 30, 15, 8, 40, 60, 10, 17, 4),
#'                  X2 <- c(4,9,8,7,6,17,6,7,8,3))
#' output <- df[,1]
#' inputs <- df[,2:3]
#' S      <- significance(output, inputs)
#' SI     <- s[[2]]
#' DE <- decomposition(output, inputs, SI,
#'                     dec_limit = 0.8,
#'                     manual_vars = NULL,
#'                     manual_states = NULL,
#'                     manual_thresholds = NULL,
#'                     threshold_type = 2,
#'                     var_names = colnames(inputs))
build_simdec_chart <- function(output, scenario, scenario_legend, main_colors, axistitle, var_names_dec) {
  # Separating result by scenario
  result_dec <- vector("list", max(scenario))

  for (i in 1:max(scenario)) {
    s <- length(output[scenario == i])
    result_dec[[i]] <- matrix(output[scenario == i], nrow = s, ncol = 1)
  }

  # Defining edges of bins
  l <- min(output)
  h <- max(output)
  bins <- 100
  edges <- seq(l, h, length.out = bins + 1)

  # Define frequency of each scenario NPV for each bin
  f <- matrix(0, nrow = max(scenario), ncol = 100)

  for (i in 1:max(scenario)) {
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
  beta <- seq(-range, range, by = step)

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
    collapse_rows(columns = 2, valign = "middle")
  %>% column_spec(2, bold = TRUE) #NEW


  # Display the table
  legend_table

  return(list(simdec_plot = b, legend_table = legend_table))
}

