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
decomposition <- function(output, inputs, SI, dec_limit, manual_vars=NULL, manual_states=NULL, manual_thresholds=NULL, threshold_type=2, var_names) {

  # 1. Variables for decomposition

  if (is.null(manual_vars)) {
    SI_sorted <- sort(SI, decreasing = TRUE)
    var_order <- order(SI, decreasing = TRUE)
    N_var_dec <- which(cumsum(SI_sorted) > dec_limit)[1]
    var_order[(N_var_dec + 1):length(var_order)] <- 0
  } else {
    var_order <- rep(0, length(manual_vars))
    for (i in 1:length(manual_vars)) {
      if (manual_vars[i] > 0) {
        var_order[manual_vars[i]] <- i
      }
    }
    N_var_dec <- sum(var_order > 0)
  }

  var_names_dec <- var_names[var_order[var_order != 0]]

  # 2. States formation

  N_var <- ncol(inputs)
  if (is.null(manual_states) && is.null(manual_thresholds)) {
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
    # states <- ifelse(is.null(manual_states), 0, manual_states)
    states <- manual_states
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

  if (is.null(manual_thresholds)) {
    thresholds <- matrix(NA, max(states), N_var)
    if (threshold_type == 1) {
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
    thresholds <- manual_thresholds[-1, ]
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

    matching_indices <- which(apply(Scen_matrix[, 2:(N_var_dec + 1)], 1, function(x) all(x == states_matching[i, ])))
    if (length(matching_indices) > 0) {
      scenario_matching[i] <- matching_indices[1]
    }
  }

  scenarios <- scenario_matching





  # Adding min values to thresholds for export

  if (is.null(manual_thresholds)) {
    thresholds_out <- matrix(NA, max(states) + 1, N_var)
    for (f in 1:N_var_dec) {
      thresholds_out[1, var_order[f]] <- min(inputs[, var_order[f]])
      th <- thresholds[states[var_order[f]], var_order[f]]
      thresholds[states[var_order[f]], var_order[f]] <- th - 1
    }
    thresholds_out[2:(max(states) + 1), ] <- thresholds
  } else {
    thresholds_out <- thresholds
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

  return(list(scenarios = scenarios, scen_legend = scen_legend, thresholds_out = thresholds_out, var_names_dec = var_names_dec))
}
