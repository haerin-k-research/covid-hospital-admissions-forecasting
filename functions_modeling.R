# Predictor order used throughout
predictor_names <- c(
  "Wearing.Masks",
  "Symtoms",
  "Symtoms.in.community",
  "Doctor.Visits",     # Outpatient Visits
  "Test.Positivity",
  "Cases",
  "Deaths",
  "Distributed",       # Vaccinations
  "index",             # Policy Index
  "climate",
  "Hospital.Admissions",
  "ED.visits"
)

# ---------------------------------------------------------
# Apply predictor-specific exclusion rules to the raw data.
# Setting excluded values to NA keeps them out of the spline
# knot placement as well as the model fit.
# ---------------------------------------------------------
apply_predictor_exclusions <- function(dataset, exclusion_rules, predictor_names) {

  out <- dataset %>% dplyr::mutate(date = as.Date(date))

  if (is.null(exclusion_rules)) return(out)

  for (block in names(exclusion_rules)) {

    block_idx <- as.integer(block)

    if (is.na(block_idx) || block_idx < 1 || block_idx > length(predictor_names)) {
      stop("Invalid predictor block in exclusion rules: ", block)
    }

    predictor_name <- predictor_names[block_idx]
    rule <- exclusion_rules[[block]]

    row_match <- out$state %in% rule$states

    if (!is.null(rule$date_range)) {
      row_match <- row_match &
        out$date >= as.Date(rule$date_range[1]) &
        out$date <= as.Date(rule$date_range[2])
    }

    out[[predictor_name]][row_match] <- NA_real_
  }

  out
}


# ---------------------------------------------------------
# Level, slope, and acceleration features for each predictor
# ---------------------------------------------------------
make_lag_features <- function(dataset, lag, ylag, exclusion_rules = NULL) {

  X_data <- apply_predictor_exclusions(
    dataset = dataset,
    exclusion_rules = exclusion_rules,
    predictor_names = predictor_names
  ) %>%
    dplyr::arrange(state, date) %>%
    dplyr::group_by(state)

  feature_names <- character(0)

  for (k in seq_along(predictor_names)) {

    predictor_name <- predictor_names[k]

    lag_n <- if (k == 11) ylag else lag

    level_name <- paste0("level.", k)
    slope_name <- paste0("slope.", k)
    accel_name <- paste0("acceleration.", k)

    X_data <- X_data %>%
      dplyr::mutate(
        !!level_name := dplyr::lag(.data[[predictor_name]], n = lag_n, order_by = date),
        !!slope_name := .data[[level_name]] - dplyr::lag(.data[[level_name]], order_by = date),
        !!accel_name := .data[[slope_name]] - dplyr::lag(.data[[slope_name]], order_by = date)
      )

    feature_names <- c(feature_names, level_name, slope_name, accel_name)
  }

  X_data %>%
    dplyr::ungroup() %>%
    dplyr::select(state, date, dplyr::all_of(feature_names))
}


# ---------------------------------------------------------
# B-spline basis, preserving NA
# ---------------------------------------------------------
fit_bs_preserve_na <- function(x, feature_index, df = 6, degree = 3) {

  if (any(is.infinite(x), na.rm = TRUE)) {
    stop("Infinite value found in training feature ", feature_index)
  }

  observed <- !is.na(x)

  if (sum(observed) == 0) {
    stop("No observed training values for feature ", feature_index)
  }

  bs_object <- splines::bs(x[observed], df = df, degree = degree)

  basis_matrix <- matrix(NA_real_, nrow = length(x), ncol = ncol(bs_object))
  basis_matrix[observed, ] <- unclass(bs_object)
  colnames(basis_matrix) <- paste0("bs.", feature_index, ".", seq_len(ncol(basis_matrix)))

  list(matrix = basis_matrix, object = bs_object)
}


predict_bs_preserve_na <- function(x, bs_object, feature_index) {

  if (any(is.infinite(x), na.rm = TRUE)) {
    stop("Infinite value found in test feature ", feature_index)
  }

  observed <- !is.na(x)

  basis_matrix <- matrix(NA_real_, nrow = length(x), ncol = ncol(bs_object))

  if (any(observed)) {

    boundary_knots <- attr(bs_object, "Boundary.knots")

    if (is.null(boundary_knots) || length(boundary_knots) != 2 ||
        any(!is.finite(boundary_knots))) {
      stop("Invalid Boundary.knots for feature ", feature_index)
    }

    x_clamped <- pmin(pmax(x[observed], boundary_knots[1]), boundary_knots[2])

    transformed <- stats::predict(bs_object, newx = x_clamped)

    if (any(!is.finite(transformed))) {
      stop("Non-finite spline basis generated for test feature ", feature_index)
    }

    basis_matrix[observed, ] <- unclass(transformed)
  }

  colnames(basis_matrix) <- paste0("bs.", feature_index, ".", seq_len(ncol(basis_matrix)))

  basis_matrix
}


make_Xdata_train <- function(dataset, lag, ylag, exclusion_rules = NULL) {

  X_data_lag <- make_lag_features(
    dataset = dataset, lag = lag, ylag = ylag,
    exclusion_rules = exclusion_rules
  )

  # state, date, and 36 derived features
  stopifnot(ncol(X_data_lag) == 38)

  bs_objects  <- vector("list", 36)
  bs_matrices <- vector("list", 36)

  for (j in seq_len(36)) {
    fitted_bs <- fit_bs_preserve_na(
      x = X_data_lag[[j + 2]], feature_index = j, df = 6, degree = 3
    )
    bs_objects[[j]]  <- fitted_bs$object
    bs_matrices[[j]] <- fitted_bs$matrix
  }

  X_data_final <- dplyr::bind_cols(
    X_data_lag,
    tibble::as_tibble(do.call(cbind, bs_matrices))
  )

  # state, date, 36 derived features, 216 spline features
  stopifnot(ncol(X_data_final) == 254)

  list(data = X_data_final, bs_objects = bs_objects)
}


make_Xdata_test <- function(dataset, lag, ylag, bs_objects_from_train,
                            exclusion_rules = NULL) {

  if (length(bs_objects_from_train) != 36) {
    stop("bs_objects_from_train must contain exactly 36 spline objects.")
  }

  X_data_lag <- make_lag_features(
    dataset = dataset, lag = lag, ylag = ylag,
    exclusion_rules = exclusion_rules
  )

  stopifnot(ncol(X_data_lag) == 38)

  bs_matrices <- vector("list", 36)

  for (j in seq_len(36)) {
    bs_matrices[[j]] <- predict_bs_preserve_na(
      x = X_data_lag[[j + 2]],
      bs_object = bs_objects_from_train[[j]],
      feature_index = j
    )
  }

  X_data_final <- dplyr::bind_cols(
    X_data_lag,
    tibble::as_tibble(do.call(cbind, bs_matrices))
  )

  stopifnot(ncol(X_data_final) == 254)

  X_data_final
}

# ---------------------------------------------------------
# Each block holds 3 derived features and 18 spline features.
# ---------------------------------------------------------
make_Xlist_new_labeled <- function(Xdata) {

  Xdata_trimmed <- Xdata %>%
    dplyr::mutate(state = as.character(state), date = as.Date(date)) %>%
    dplyr::arrange(state, date) %>%
    dplyr::group_by(state) %>%
    dplyr::slice(6:dplyr::n()) %>%
    dplyr::ungroup()

  X_ <- Xdata_trimmed %>% dplyr::select(-date, -state)

  X_list <- lapply(seq_len(12), function(k) {
    X_[, c((3 * k - 2):(3 * k), (18 * k + 19):(18 * k + 36))]
  })

  list(
    X_list = X_list,
    state  = Xdata_trimmed$state,
    date   = Xdata_trimmed$date
  )
}


make_y_labeled <- function(dataset) {

  dataset %>%
    dplyr::mutate(state = as.character(state), date = as.Date(date)) %>%
    dplyr::arrange(state, date) %>%
    dplyr::group_by(state) %>%
    dplyr::slice(6:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(state = state, date = date, obs = Hospital.Admissions)
}


get_exclusion_mask <- function(combo_idx, state_vec, date_vec, exclusion_rules) {

  exclude <- rep(FALSE, length(state_vec))

  for (block in as.character(combo_idx)) {

    if (!block %in% names(exclusion_rules)) next

    rule <- exclusion_rules[[block]]
    state_match <- state_vec %in% rule$states

    if (is.null(rule$date_range)) {
      exclude <- exclude | state_match
    } else {
      date_match <- date_vec >= as.Date(rule$date_range[1]) &
                    date_vec <= as.Date(rule$date_range[2])
      exclude <- exclude | (state_match & date_match)
    }
  }

  exclude
}

# ---------------------------------------------------------
# Fit every predictor combination of size k within one fold
# ---------------------------------------------------------
modeling_m_stateaware <- function(k, X_train_labeled, X_test_labeled,
                                  y_train_full, y_test_full, exclusion_rules) {

  lambdas <- 10^seq(3, -2, by = -.1)
  comb_indices <- combn(12, k)

  y_predicted <- list()
  error_rmse  <- vector()

  for (i in 1:ncol(comb_indices)) {

    combo_idx <- comb_indices[, i]

    X_train_combo <- do.call(cbind, lapply(X_train_labeled$X_list[combo_idx], as.matrix))
    X_test_combo  <- do.call(cbind, lapply(X_test_labeled$X_list[combo_idx],  as.matrix))

    storage.mode(X_train_combo) <- "double"
    storage.mode(X_test_combo)  <- "double"

    mask_train <- get_exclusion_mask(combo_idx, X_train_labeled$state,
                                     X_train_labeled$date, exclusion_rules)
    mask_test  <- get_exclusion_mask(combo_idx, X_test_labeled$state,
                                     X_test_labeled$date, exclusion_rules)

    X_train_f <- X_train_combo[!mask_train, , drop = FALSE]
    y_train_f <- y_train_full[!mask_train]
    X_test_f  <- X_test_combo[!mask_test, , drop = FALSE]
    y_test_f  <- y_test_full[!mask_test]

    if (!all(is.finite(X_train_f)) || !all(is.finite(y_train_f))) {
      stop(sprintf("Combination %s (train): non-finite values remain after exclusion",
                   paste(combo_idx, collapse = ",")))
    }
    if (!all(is.finite(X_test_f)) || !all(is.finite(y_test_f))) {
      stop(sprintf("Combination %s (test): non-finite values remain after exclusion",
                   paste(combo_idx, collapse = ",")))
    }

    cv_fit <- glmnet::cv.glmnet(
      as.matrix(X_train_f), y_train_f,
      alpha = 1, family = poisson(link = "log"), lambda = lambdas
    )

    y_pred <- as.numeric(
      predict(cv_fit, type = "response", s = cv_fit$lambda.min,
              newx = as.matrix(X_test_f))
    )

    y_predicted[[i]] <- tibble::tibble(
      state = X_test_labeled$state[!mask_test],
      date  = X_test_labeled$date[!mask_test],
      obs   = y_test_f,
      pred  = y_pred
    )

    error_rmse[i] <- sqrt(mean((y_pred - y_test_f)^2))

  }

  list(y_predicted = y_predicted, error_rmse = error_rmse)
}

# ---------------------------------------------------------
# Rolling-window cross-validation over five folds.
# ---------------------------------------------------------
modeling_20_10_median_stateaware <- function(count, wlag, dataset, n_states,
                                             exclusion_rules) {

  set.seed(237)

  d <- 5
  n_combos <- ncol(combn(12, count))

  y_predicted_by_fold <- vector("list", d)
  error_vec_rmse <- matrix(nrow = 0, ncol = n_combos)

  for (i in seq_len(d)) {

    if (i == 1) {
      train_start <- 1
      train_end   <- n_states * 20
      test_start  <- n_states * 15 + 1
      test_end    <- n_states * 30
    } else {
      train_start <- (i - 2) * (n_states * 10) + (n_states * 5 + 1)
      train_end   <- train_start + (n_states * 25) - 1
      test_start  <- (i - 1) * (n_states * 10) + (n_states * 15 + 1)
      test_end    <- test_start + (n_states * 15) - 1
    }

    train_data <- dataset[train_start:train_end, , drop = FALSE]
    test_data  <- dataset[test_start:test_end, , drop = FALSE]

    X_train_result <- make_Xdata_train(
      dataset = train_data, lag = wlag, ylag = wlag,
      exclusion_rules = exclusion_rules
    )

    X_test_data <- make_Xdata_test(
      dataset = test_data, lag = wlag, ylag = wlag,
      bs_objects_from_train = X_train_result$bs_objects,
      exclusion_rules = exclusion_rules
    )

    X_train_labeled <- make_Xlist_new_labeled(X_train_result$data)
    X_test_labeled  <- make_Xlist_new_labeled(X_test_data)

    y_train_labeled <- make_y_labeled(train_data)
    y_test_labeled  <- make_y_labeled(test_data)

    check_alignment <- function(X_labeled, y_labeled, label) {

      x_key <- paste(as.character(X_labeled$state), as.character(as.Date(X_labeled$date)))
      y_key <- paste(as.character(y_labeled$state), as.character(as.Date(y_labeled$date)))

      if (!identical(x_key, y_key)) {
        first_bad <- which(x_key != y_key)[1]
        stop("Fold ", i, " ", label, " X-y alignment failure at row ", first_bad,
             ". X key: ", x_key[first_bad], "; y key: ", y_key[first_bad])
      }
    }

    check_alignment(X_train_labeled, y_train_labeled, "train")
    check_alignment(X_test_labeled,  y_test_labeled,  "test")

    stopifnot(
      length(X_train_labeled$state) == nrow(y_train_labeled),
      length(X_test_labeled$state)  == nrow(y_test_labeled)
    )

    comb <- modeling_m_stateaware(
      k = count,
      X_train_labeled = X_train_labeled,
      X_test_labeled = X_test_labeled,
      y_train_full = y_train_labeled$obs,
      y_test_full = y_test_labeled$obs,
      exclusion_rules = exclusion_rules
    )

    y_predicted_by_fold[[i]] <- comb$y_predicted
    error_vec_rmse <- rbind(error_vec_rmse, comb$error_rmse)

    message("k=", count, " | fold=", i, " done")
  }

  y_predicted_new <- lapply(seq_len(n_combos), function(combo_i) {
    dplyr::bind_rows(lapply(y_predicted_by_fold, function(fold) fold[[combo_i]]))
  })

  list(
    y_predicted_new   = y_predicted_new,
    error_median_rmse = apply(error_vec_rmse, 2, median)
  )
}

run_stateaware_by_k_checked <- function(k, wlag, dataset, n_states, exclusion_rules,
                                        save_dir = "results/model_fits") {

  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

  save_path <- file.path(save_dir, sprintf("stateaware_k%d_wlag%d.rds", k, wlag))

  if (file.exists(save_path)) {
    message("k=", k, " already exists, skipping")
    return(readRDS(save_path))
  }

  message("k=", k, " started (", Sys.time(), ")")

  result <- modeling_20_10_median_stateaware(k, wlag, dataset, n_states, exclusion_rules)

  n_combos   <- length(result$y_predicted_new)
  row_counts <- vapply(result$y_predicted_new, nrow, integer(1))

  has_bad_values <- vapply(
    result$y_predicted_new,
    function(df) any(!is.finite(df$pred)),
    logical(1)
  )

  if (any(has_bad_values)) {
    stop("k=", k, ": non-finite predictions in ", sum(has_bad_values), " combinations")
  }

  bad_rmse <- !is.finite(result$error_median_rmse) |
              result$error_median_rmse < 0 |
              result$error_median_rmse > 100

  if (any(bad_rmse)) {
    stop("k=", k, ": implausible RMSE in ", sum(bad_rmse), " combinations")
  }

  expected_row_counts <- vapply(combn(12, k, simplify = FALSE), function(combo) {

    excluded_states <- character(0)
    if (5  %in% combo) excluded_states <- union(excluded_states, "IA")
    if (12 %in% combo) excluded_states <- union(excluded_states, c("MN", "MO", "OK"))

    as.integer((n_states - length(excluded_states)) * 50)
  }, integer(1))

  if (!identical(row_counts, expected_row_counts)) {
    stop("k=", k, ": prediction row count mismatch in combinations ",
         paste(which(row_counts != expected_row_counts), collapse = ", "))
  }

  message(
    "k=", k, " done -- combos: ", n_combos,
    " | row range: ", min(row_counts), "-", max(row_counts),
    " | RMSE range: ", round(min(result$error_median_rmse), 3), "-",
    round(max(result$error_median_rmse), 3),
    " (", Sys.time(), ")"
  )

  saveRDS(result, save_path)

  result
}
