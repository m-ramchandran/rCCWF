#' @import rlang
NULL

#' Fit a Random Forest Model
#'
#' @param data A data frame or tibble containing the training data
#' @param outcome_col Character string specifying the name of the outcome variable
#'        (default: "y")
#' @param ntrees Integer specifying the number of trees in the forest
#'        (default: 100)
#'
#' @returns Returns a fitted random forest model object of class 'model_fit' from the parsnip package,
#'          using the ranger engine with permutation importance enabled.
#'
#' @importFrom parsnip rand_forest set_engine fit
rf_fit <- function(data, outcome_col, ntrees = 100) {
  parsnip::rand_forest(mode = "regression", trees = ntrees) |>
    parsnip::set_engine("ranger", importance = "permutation") |>
    parsnip::fit(y ~ ., data = data)
}

#' Make Predictions with a Fitted Random Forest Model
#'
#' @param model A fitted model object from rf_fit()
#' @param newdata A data frame or tibble containing new data for prediction
#'
#' @returns Returns a numeric vector containing the predicted values for the new data
#'
#' @importFrom stats predict
rf_predict <- function(model, newdata) {
  stats::predict(model, new_data = newdata)$.pred
}

#' Fit cross-cluster weighted forests method
#'
#' @param train_data Either a single dataframe or a list of pre-clustered dataframes
#' @param outcome_col Name of outcome column
#' @param model_fit Function to fit models (defaults to rf_fit)
#' @param model_predict Function to make predictions (defaults to rf_predict)
#' @param merged_trees Number of trees for merged model
#' @param cluster_trees Number of trees for individual cluster models
#' @param cluster_ind Boolean representing if we should perform k-means clustering
#' @param n_clusters Number of clusters for k-means. Only used if cluster_ind = TRUE
#' @param n_cores Number of cores for parallel processing (default = 2)
#'
#' @returns Returns a list containing:
#'   \item{merged_model}{A single random forest model fitted on all data combined}
#'   \item{cluster_models}{List of models, one fitted to each cluster}
#'   \item{stack_ridge}{Fitted ridge regression model for the stacking component}
#'   \item{stack_lasso}{Fitted lasso regression model for the stacking component}
#'   \item{stack_ridge_lambda}{Optimal lambda value for ridge regression}
#'   \item{stack_lasso_lambda}{Optimal lambda value for lasso regression}
#'   \item{stack_ridge_coef}{Coefficients from ridge regression stacking model}
#'   \item{stack_lasso_coef}{Coefficients from lasso regression stacking model}
#'   \item{model_fit}{The base ensemble member fitting function used}
#'   \item{model_predict}{The base ensemble member prediction function used}
#'
#' @importFrom future plan multisession sequential
#' @importFrom dplyr bind_rows select pull
#' @importFrom furrr future_map furrr_options
#' @importFrom glmnet cv.glmnet
#' @importFrom stats coef setNames predict
#'
#' @export
crosscluster_fit <- function(train_data,
                             outcome_col = "y",
                             model_fit = rf_fit,
                             model_predict = rf_predict,
                             merged_trees = 500,
                             cluster_trees = 100,
                             cluster_ind = TRUE,
                             n_clusters = 10,
                             n_cores = 2) {
  # Set up parallel processing
  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Process training data
  if (!is.list(train_data)) {
    clusters <- create_clusters(
      train_data = train_data,
      n_clusters = n_clusters,
      outcome_col = outcome_col
    )
    clusters_list <- clusters$clusters
  } else {
    if (cluster_ind == TRUE) {
      clusters <- create_clusters(
        train_data = train_data |> dplyr::bind_rows(),
        n_clusters = n_clusters,
        outcome_col = outcome_col
      )
      clusters_list <- clusters$clusters
    } else {
      clusters_list <- train_data
    }
  }

  # Combine all training data for merged model
  merged_data <- dplyr::bind_rows(clusters_list)

  # Fit merged model with specified number of trees
  merged_model <- rf_fit(merged_data, outcome_col, merged_trees)

  # Create workflow for each cluster
  model_workflows <- furrr::future_map(
    clusters_list,
    function(cluster_data) {
      model_fit(cluster_data, outcome_col, cluster_trees)
    },
    .options = furrr::furrr_options(seed = TRUE)
  )

  # Prepare training predictions for stacking
  train_preds <- furrr::future_map(
    model_workflows,
    function(model) {
      model_predict(model, dplyr::select(merged_data, -dplyr::all_of(outcome_col)))
    },
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    dplyr::bind_cols() |>
    stats::setNames(paste0("model_", seq_along(model_workflows)))

  # Rest of the function remains the same, but with explicit namespacing
  stack_x <- as.matrix(train_preds)
  stack_y <- dplyr::pull(merged_data, outcome_col)

  stack_ridge_cv <- glmnet::cv.glmnet(
    x = stack_x,
    y = stack_y,
    alpha = 0,
    nfolds = 5,
    parallel = TRUE,
    standardize = TRUE
  )

  stack_lasso_cv <- glmnet::cv.glmnet(
    x = stack_x,
    y = stack_y,
    alpha = 1,
    nfolds = 5,
    parallel = TRUE,
    standardize = TRUE
  )

  ridge_coef <- stats::coef(stack_ridge_cv, s = "lambda.min")
  lasso_coef <- stats::coef(stack_lasso_cv, s = "lambda.min")

  list(
    merged_model = merged_model,
    cluster_models = model_workflows,
    stack_ridge = stack_ridge_cv,
    stack_lasso = stack_lasso_cv,
    stack_ridge_lambda = stack_ridge_cv$lambda.min,
    stack_lasso_lambda = stack_lasso_cv$lambda.min,
    stack_ridge_coef = ridge_coef,
    stack_lasso_coef = lasso_coef,
    model_fit = model_fit,
    model_predict = model_predict
  )
}

#' Make predictions using fitted CCWF models
#'
#' @param ccwf_fit Output from crosscluster_fit function
#' @param new_data New data for predictions
#'
#' @importFrom stats predict
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#'
#' @returns Tibble with predictions from all methods
#' @export
crosscluster_predict <- function(ccwf_fit, new_data) {
  merged_preds <- stats::predict(ccwf_fit$merged_model, new_data)$.pred

  individual_preds <- purrr::map(
    ccwf_fit$cluster_models,
    function(model) {
      ccwf_fit$model_predict(model, new_data)
    }
  ) |>
    dplyr::bind_cols() |>
    stats::setNames(paste0("model_", seq_along(ccwf_fit$cluster_models)))

  unweighted_preds <- rowMeans(individual_preds)

  stack_ridge_preds <- stats::predict(
    ccwf_fit$stack_ridge,
    newx = as.matrix(individual_preds),
    s = "lambda.min"
  )[, 1]

  stack_lasso_preds <- stats::predict(
    ccwf_fit$stack_lasso,
    newx = as.matrix(individual_preds),
    s = "lambda.min"
  )[, 1]

  tibble::tibble(
    merged = merged_preds,
    unweighted = unweighted_preds,
    stack_ridge = stack_ridge_preds,
    stack_lasso = stack_lasso_preds
  )
}
