#' cat_transform -
#'
#'  Transforms categorical variable into numeric, by assigning indices in
#'  accordance with average Y values in each category.
#'
#' @param Y - a vector of Y (target) values
#' @param X - a vector of X values
#'
#' @return sortedX  - categories of X sorted by increasing average Y
#'
#' @return indexMap - an array of the same size as X with assigned numeric
#'                    indices corresponding to original categorical values
#'
#' @export
#'
#' @examples -
#'
#' Y               <- c(10,20,40,30,60,10)
#' X               <- c(1,2,2,2,1,1)
#' CT              <- cat_transform(Y, X)
#' sortedX         <- CT$sortedX
#' indexMap        <- CT$indexMap
#' print(sortedX)
#' print(indexMap)

cat_transform <- function(Y, X) {
  # Find all unique values of X
  sort_unique <- sort(unique(X), decreasing = FALSE)
  uniqueX <- sort_unique[!duplicated(sort_unique)]

  # Compute average of Ys for each unique value of X
  averages <- numeric(length(uniqueX))
  for (i in 1:length(uniqueX)) {
    idx <- X == uniqueX[i]
    averages[i] <- mean(Y[idx])
  }

  # Sort the unique values of X in accordance by average Y
  sortedIndices <- order(averages)
  sortedX <- uniqueX[sortedIndices]

  # Assign indices to sorted unique values of X
  indexMap <- match(X, sortedX)

  return(list(sortedX = sortedX, indexMap = indexMap))
}

#' append_with_cat -
#'
#' Appends numeric inputs with categorical variables which are transformed into numeric ones.
#'
#' @param output           - a vector of Y (target) values
#' @param numeric_inputs   - a matrix of X values - i.e., all numeric inputs
#' @param string_data      - all non-numeric columns in the data set
#'
#' @return all_inputs      - a matrix containing all numeric and all transformed input variables
#' @return sorted_X        - the order of categories in increasing average Y for each non-numeric input variables
#'
#' @export
#'
#' @examples
#'
#' data            <- data.frame (Y  = c(10,20,40,30,60,10),
#'                                X1 = c(1,2,2,2,1,1),
#'                                X2 = c(0.6,0.8,0.9,0.4,0.2,0.3)
#'                                X3 = c("Y", "N", "N", "N", "Y", "Y")
#' AWC             <- cat_transform(data[,1], data[, 2:3], data[,4])
#' all_inputs      <- AWSC$all_inputs
#' head(all_inputs)

append_with_cat <- function(output, numeric_inputs, string_data) {
  # Check if string_data is a list or vector
  if (is.list(string_data) || is.vector(string_data)) {
    string_data[nzchar(string_data)]
    string_data <- matrix(string_data, nrow = length(string_data))
  }

  # Get rid of empty columns
  string_data <- na.omit(string_data)

  # Get number of categorical variables
  n_cats <- ncol(string_data)

  # Initialize matrix for transformed categorical variables & list for sorted categories
  X_cat_num <- matrix(NA_real_, nrow = length(output), ncol = n_cats)
  sorted_X <- vector("list", n_cats)

  # Check Y averages to wisely index the categories
  for (c in 1:n_cats) {
    result <- cat_transform(output, string_data[, c])
    sorted_X[[c]] <- result$sorted_X
    X_cat_num[, c] <- result$indexMap
  }

  # Append inputs
  all_inputs <- cbind(numeric_inputs, X_cat_num)

  return(list(all_inputs = all_inputs, sorted_X = sorted_X))
}


#' number_of_bins -
#'
#' number_of_bins defines the optimal number of bins for first-order and
#' second-order significance indices calculation.
#'
#' @param N_runs    - the total number of observations
#' @param N_factors - number of inputs variables
#'
#' @return N_bins_foe - number of bins for first-order effect calculation
#' @return N_bins_soe - number of bins for second-order effect calculation
#'
#' @export
#'
#' @examples
#'
#' data            <- data.frame (Y  = c(10,20,40,30,60,10),
#'                                X1 = c(1,2,2,2,1,1),
#'                                X2 = c(0.6,0.8,0.9,0.4,0.2,0.3),
#'                                X3 = c("Y", "N", "N", "N", "Y", "Y"))
#' N_runs          <- nrow(data)
#' N_factors       <- ncol(data) - 1 # Assuming one column of the data is for the target variable, i.e., Y
#' NB              <- number_of_bins(N_runs, N_factors)
#' nbins_foes      <- NB$nbins_foe
#' nbins_soe       <- NB$nbins_soe
number_of_bins <- function(N_runs, N_factors) {
  N_bins_foe <- ceiling(36 - 2.7 * N_factors + (0.0017 - 0.00008 * N_factors) * N_runs)
  if (N_bins_foe <= 30) {
    N_bins_foe <- 10
  }

  N_bins_soe <- max(4, round(sqrt(N_bins_foe)))

  return(list(N_bins_foe = N_bins_foe, N_bins_soe = N_bins_soe))
}


#' weighted_variance -
#'
#' a function that calculates weighted variance.
#'
#' @param x       - a numeric vector
#' @param weights - a vector of weights
#'
#' @return weighted variance
#' @export
#'
#' @examples
#'
#' X       <- c(10, 20, 30, 15, 8, 40, 60, 10, 17, 4)
#' weights <- c(0.1, 0.3, 0.05, 0.05, 0.02, 0.08, 0.1, 0.1, 0.15, 0.05)
#' WV      <- weighted_variance(X, weights)
weighted_variance <- function(x, weights) {
  # Calculate the weighted mean
  weighted_mean <- weighted.mean(x, weights)

  # Calculate the squared deviations from the weighted mean
  squared_deviations <- (x - weighted_mean) ^ 2
  weighted_squared_deviations <- squared_deviations * weights

  # Calculate the sum of the weighted squared deviations
  sum_weighted_squared_deviations <- sum(weighted_squared_deviations)

  # Calculate the weighted variance
  variance <- sum_weighted_squared_deviations / (sum(weights) - 1)

  return(variance)
}

#' magic_binning -
#'
#' a function that bins values following a set of rules. n_bins_default is obtained using the number_of_bins function.
#'
#' The following binning rules apply:
#' 1. The size of each bin should be no less than length(X)/n_bins.
#' 2. The same values of X should be allocated to a single bin.
#' 3. NA values should get the 0 bin index.
#'
#' @param X               - a numeric vector of values
#' @param n_bins_default  - default number of bins
#'
#' @return bin_idx         - an array with bin indices of the same length as X
#' @return n_bins_out      - resulting number of bins (might be less than default if many NaNs or same values)
#' @export
#'
#' @examples
#' X <- c(NA, NA, 6, 6, 6, 6, 6, 6, 100, 200, 2, 1)
#' n_bins_default = 6
#' MB <- magic_binning(X, n_bins_default)
magic_binning <- function(X, n_bins_default) {
  # Sort X
  X_sorted <- sort(X)
  idx <- order(X)

  # Compute minimum bin size
  min_bin_size <- floor(length(X_sorted) / n_bins_default)

  # Initialize bin index array
  bin_idx <- rep(0, length(X_sorted))

  # If number of unique values is less than or equal to the number of bins,
  # then use them to form the bins
  if (length(unique(X_sorted)) <= n_bins_default) {
    bin_idx <- as.integer(as.factor(X_sorted))
  } else {
    # Initialize remaining size & edge index
    not_nan_size <- length(X) - sum(is.na(X))
    remaining_size <- length(X) - sum(is.na(X))
    current_edge_idx <- min_bin_size

    # Iterate over bins to produce a corresponding bin_idx
    for (b in 1:n_bins_default) {
      current_bin_size <- min_bin_size

      # While the edge is between same values, move one element further
      while (current_edge_idx < not_nan_size && X_sorted[current_edge_idx + 1] == X_sorted[current_edge_idx]) {
        current_edge_idx <- current_edge_idx + 1
        current_bin_size <- current_bin_size + 1
      }

      # Assign bin indices to elements in the bin
      bin_idx[(current_edge_idx - current_bin_size + 1):current_edge_idx] <- b

      # Update remaining size and bin edge index for next iteration
      remaining_size <- remaining_size - current_bin_size

      # If not enough elements left for two bins, assign all to the last bin and break the for loop.
      if (remaining_size < min_bin_size * 2) {
        bin_idx[(current_edge_idx + 1):length(bin_idx)] <- b + 1
        break
      }
      current_edge_idx <- current_edge_idx + min_bin_size
    }
  }

  # Map bin indices back to original order
  bin_idx[idx] <- bin_idx

  # Assign 0 bin index to NaN values
  bin_idx[is.na(X)] <- 0

  # The resulting number of bins
  n_bins_out <- max(bin_idx)

  return(list(bin_idx = bin_idx, n_bins_out = n_bins_out))
}

#' bin_data_1D -
#'
#' bin_data_1D computes averages and counts of Y in bins of X. It uses the magic_binning function.
#'
#' @param X              - a vector of X values
#' @param Y              - a vector of Y values
#' @param n_bins_default - default number of bins
#'
#' @return bin_avg       - averages of Y values in bins of X
#' @return bin_count     - number of Y values in bins of X
#' @export
#'
#' @examples
#'
#' X              <- c(10, 20, 30, 15, 8, 40, 60, 10, 17, 4)
#' Y              <- c(167, 82, 75, 134, 186, 51, 17, 167, 86, 198)
#' n_bins_default <- 3
#' BD1D <- bin_data_1D(X, Y, )
bin_data_1D <- function(X, Y, n_bins_default) {
  # Getting the bins indices
  result_magic_binning <- magic_binning(X, n_bins_default)
  bin_idx <- result_magic_binning$bin_idx
  n_bins_X <- result_magic_binning$n_bins_out

  # Initializing matrices for recording averages and counts of Y
  bin_avg <- rep(NA, n_bins_X)
  bin_count <- rep(NA, n_bins_X)

  # Computing averages and counts of Y
  for (b in 1:n_bins_X) {
    bin_avg[b] <- mean(Y[bin_idx == b])
    bin_count[b] <- sum(bin_idx == b)
  }

  return(list(bin_avg = bin_avg, bin_count = bin_count))
}

#' bin_data_2D -
#'
#' bin_data_2D computes averages and counts of Y in bins of XiXj, Xi, and Xj. The function uses the magic_binning function.
#'
#' @param Xi                    - a vector of Xi values
#' @param Xj                    - a vector of Xj values
#' @param Y                     - a vector of Y values
#' @param n_bins_default        - default number of bins
#'
#' @return bin_avg_ij_string    - an array of average Y values in 2D bins of Xi and Xj
#' @return bin_count_ij_string  - an array of corresponding count of Y values
#' @return bin_avg_i            - an array of average Y values in 1D bins of Xi
#' @return bin_count_i          - an array of corresponding count of Y values
#' @return bin_avg_j            - an array of average Y values in 1D bins of Xj
#' @return bin_count_j          - an array of corresponding count of Y values
#'
#' @export
#'
#' @examples
#' df <- data.frame(Y  <- c(167, 82, 75, 134, 186, 51, 17, 167, 86, 198),
#'                  X1 <- c(10, 20, 30, 15, 8, 40, 60, 10, 17, 4),
#'                  X2 <- c(4,9,8,7,6,17,6,7,8,3))
#' n_bins_default <- 4
#' BD2D           <- bin_data_2D(df[,2], df[,3], df[,1], n_bins_default)
bin_data_2D <- function(Xi, Xj, Y, n_bins_default) {
  bins_Xi <- magic_binning(Xi, n_bins_default)
  bin_idx_i <- bins_Xi[[1]]
  n_bins_i <- bins_Xi[[2]]

  bins_Xj <- magic_binning(Xj, n_bins_default)
  bin_idx_j <- bins_Xj[[1]]
  n_bins_j <- bins_Xj[[2]]

  bin_avg_ij <- matrix(NA, nrow = n_bins_i, ncol = n_bins_j)
  bin_count_ij <- matrix(NA, nrow = n_bins_i, ncol = n_bins_j)
  bin_avg_i <- rep(NA, n_bins_i)
  bin_count_i <- rep(NA, n_bins_i)
  bin_avg_j <- rep(NA, n_bins_j)
  bin_count_j <- rep(NA, n_bins_j)

  for (n in 1:n_bins_i) {
    bin_avg_i[n] <- mean(Y[bin_idx_i == n])
    bin_count_i[n] <- length(Y[bin_idx_i == n])
  }

  for (m in 1:n_bins_j) {
    bin_avg_j[m] <- mean(Y[bin_idx_j == m])
    bin_count_j[m] <- length(Y[bin_idx_j == m])
  }

  for (n in 1:n_bins_i) {
    for (m in 1:n_bins_j) {
      mask_i <- which(bin_idx_i == n)
      mask_j <- which(bin_idx_j == m)
      bin_indices <- intersect(mask_i, mask_j)
      bin_avg_ij[n, m] <- mean(Y[bin_indices])
      bin_count_ij[n, m] <- length(Y[bin_indices])
    }
  }

  bin_avg_ij_string <- as.vector(t(bin_avg_ij))
  ind_exists <- which(!is.na(bin_avg_ij_string))
  bin_avg_ij_string <- bin_avg_ij_string[ind_exists]

  bin_count_ij_string <- as.vector(t(bin_count_ij))
  bin_count_ij_string <- bin_count_ij_string[ind_exists]

  return(list(bin_avg_ij_string, bin_count_ij_string, bin_avg_i, bin_count_i, bin_avg_j, bin_count_j))
}

#' sensitivity_indices -
#'
#' sensitivity_indices calculates how much variability of the output is explained by inputs.
#' It uses the the number_of_bins, bin_data_1D, bin_data_2D  and magic_binning functions.
#'
#' @param output A vector containing the target variable (Y).
#' @param inputs All input variables.
#'
#' @return FOE - first-order effects (also called 'main' or 'individual' effects).
#' @return SOE - second-order effects (also called 'interaction' effects).
#' @return SI  - significance index, combined effect of each input.
#'
#' @export
#'
#' @examples
#' df <- data.frame(Y  <- c(167, 82, 75, 134, 186, 51, 17, 167, 86, 198),
#'                  X1 <- c(10, 20, 30, 15, 8, 40, 60, 10, 17, 4),
#'                  X2 <- c(4,9,8,7,6,17,6,7,8,3))
#' output <- df[,1]
#' inputs <- df[,2:3]
#' S      <- sensitivity_indices(output, inputs)
sensitivity_indices <- function(output, inputs) {
  # sensitivity_indices calculates how much variability of the output is explained by inputs
  #
  # Uses functions number_of_bins, bin_data, magic_binning.

  # Get data size
  N_runs <- length(output)
  N_factors <- ncol(inputs)
  factor_names <- names(inputs)

  # Define number of bins
  bins <- number_of_bins(N_runs, N_factors)
  N_bins_foe <- bins[[1]]
  N_bins_soe <- bins[[2]]

  # Initialize effects
  FOE <- rep(NA, N_factors)
  SOE <- matrix(0, nrow = N_factors, ncol = N_factors)
  SI <- rep(NA, N_factors)

  # Overall variance of the output
  VarY <- var(output)

  # For each input variable
  for (i in 1:N_factors) {
    # Calculate its first-order effect
    bin_data <- bin_data_1D(inputs[, i], output, N_bins_foe)
    bin_avg <- bin_data[[1]]
    bin_count <- bin_data[[2]]
    FOE[i] <- weighted_variance(bin_avg, bin_count) / VarY

    # Calculate second-order effect
    for (j in 1:N_factors) {
      if (i != j && j > i) {
        bin_data_2D_res <- bin_data_2D(inputs[, i], inputs[, j], output, N_bins_soe)
        bin_avg_ij <- bin_data_2D_res[[1]]
        bin_count_ij <- bin_data_2D_res[[2]]
        bin_avg_i <- bin_data_2D_res[[3]]
        bin_count_i <- bin_data_2D_res[[4]]
        bin_avg_j <- bin_data_2D_res[[5]]
        bin_count_j <- bin_data_2D_res[[6]]
        var_ij <- weighted_variance(bin_avg_ij, bin_count_ij)
        var_i <- weighted_variance(bin_avg_i, bin_count_i)
        var_j <- weighted_variance(bin_avg_j, bin_count_j)
        SOE[i, j] <- (var_ij - var_i - var_j) / VarY
      }
    }
  }

  # Calculate combined effect (FOE plus halves of all SOE)
  SOE_full <- SOE + t(SOE)
  for (k in 1:N_factors) {
    SI[k] <- FOE[k] + sum(SOE_full[, k]) / 2
  }

  return(list(factor_names = factor_names, SI = SI, FOE = FOE, SOE= SOE))
}
