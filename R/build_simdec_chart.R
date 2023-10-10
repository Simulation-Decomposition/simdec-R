#' build_simdec_chart -
#'
#' function visually represents results.
#'
#' @param output           - an array of Y values
#' @param scenario         - an array of associated scenario indices
#' @param scenario_legend  - a scenario table that shows which states of which
#'                           variables compose different scenarios
#' @param main_colors      - (optional) hex values of the main colors, should be as many as the
#'                           states for the first in decomposition (or the most
#'                           influential input variable). For example,
#'                           main_colors <- c('#00B0F0', '#1FDF4D', '#1FDF4D')
#' @param axistitle        - a title for the x axis.
#' @param var_names_dec    - cell array with names for variables for decomposition
#'
#' @return simdec_plot     - a stacked histogram
#' @return legend_table    - a table of legends
#' @export
#'
#' @examples
#' BSC <- build_simdec_chart(output, scenario, scenario_legend, main_colors, axistitle, var_names_dec)
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



  return(list(simdec_plot = b, legend_table = legend_table, legend_data = legend_data, color = color))
}
