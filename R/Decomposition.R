#' Decomposition -
#'
#' This functions creates the scenarios and maps them onto output values.
#'
#' @param output             - a vector of the target variable (Y)
#' @param inputs             - a matrix of input variables
#' @param SI                 - sensitivity indices
#' @param decomposition_limit- threshold of cumulative significance for selection
#'                             of input variables for decomposition
#' @param order_of_variables - (optional) for custom decomposition specify the order
#'                             of variables for decomposition, use zero to exclude.
#'                             For example, if 4 input variables, third and second
#'                             are desired for decomposition, then
#'                             order_of_variables <- c(0 2 1 0).
#' @param number_of_states      - (optional) the number of states for each input
#'                             variable, i.e. c(0 3 2 0)
#' @param state_boundaries   - state_boundaries - [optional] maximums (numeric thresholds)
#'                             of every state, leave the rest as NA, e.g.
#'                             matrix(NA, 3, -1, NA,
#'                                    NA, 5,  0, NA,
#'                                    NA, 7, NA, NA, byrow = TRUE)
#' @param boundary_type      - 1 for 'precentile-based' (same amount of observations in each state),
#'                             2 for 'interval-based' (equaly-spaced ranges of states)
#' @param input_names        - names of the input variables in the order of their appearance
#'                             in the original dataset. Default value {X1, X2, X3...}.
#'
#' @return scenarios         - an array of the same size as Y with scenario indices
#'                             for every simulation run.
#' @return scen_legend       - a scenario table that shows which states of which
#'                             variables compose different sceanrios
#' @return boundaries_out    - numeric boundaries of states of input variables
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
#'                     decomposition_limit = 0.8,
#'                     order_of_variables = NULL,
#'                     number_of_states = NULL,
#'                     state_boundaries = NULL,
#'                     boundary_type = 2,
#'                     input_names = colnames(inputs))
decomposition <- function(output, inputs, SI, decomposition_limit, order_of_variables=NULL, number_of_states=NULL, state_boundaries=NULL, boundary_type=2, input_names) {

  # 1. Variables for decomposition

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

  return(list(scenarios = scenarios, scen_legend = scen_legend, boundaries_out = boundaries_out, var_names_dec = var_names_dec))
}

