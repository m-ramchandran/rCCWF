#' @import rlang
NULL

utils::globalVariables(c(".metric", ".estimate", "truth", "estimate", "method", "rmse", "cluster"))

#' Cluster Ensemble Workflow
#'
#' @description Implements a cluster ensemble workflow for prediction
#'
#' @param train_data Training data
#' @param test_data Test data
#' @param n_clusters Number of clusters (default: 5)
#' @param outcome_col Name of outcome column (default: "y")
#' @param merged_trees Number of merged trees (default: 500)
#' @param cluster_trees Number of cluster trees (default: 100)
#' @param cluster_ind Clustering indicator (default: TRUE)
#' @param n_cores Number of cores for parallel processing (default: 2)
#' @param seed Random seed (default: NULL)
#'
#' @returns Returns a list containing:
#'   \item{fitted_models}{The fitted cross-cluster model objects}
#'   \item{predictions}{A list of predictions for each test dataset}
#'   \item{individual_performance}{A list of performance metrics for each test dataset}
#'   \item{average_performance}{A data frame of averaged performance metrics across all test sets}
#'
#' @importFrom yardstick rmse
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom stats sd
#' @importFrom purrr map map_df reduce
#'
#' @export
cluster_ensemble_workflow <- function(train_data,
                                      test_data,
                                      n_clusters = 5,
                                      outcome_col = "y",
                                      merged_trees = 500,
                                      cluster_trees = 100,
                                      cluster_ind = TRUE,
                                      n_cores = 2,
                                      seed = NULL) {
  # Process test data
  if (length(dim(test_data[[1]])) == 0) {
    test_data <- list(test_data)
  }

  # Fit models
  fitted_models <- crosscluster_fit(
    train_data = train_data,
    outcome_col = outcome_col,
    model_fit = rf_fit,
    model_predict = rf_predict,
    merged_trees = merged_trees,
    cluster_trees = cluster_trees,
    cluster_ind = cluster_ind,
    n_clusters = n_clusters,
    n_cores = n_cores
  )

  # Make predictions and calculate performance for each test set
  performance_list <- purrr::map(test_data, function(test_set) {
    # Get predictions
    predictions <- crosscluster_predict(
      ccwf_fit = fitted_models,
      new_data = test_set
    )

    # Calculate performance metrics
    metrics <- purrr::map_df(predictions, function(preds) {
      yardstick::metric_set(
        yardstick::rmse
      )(
        data.frame(
          truth = test_set[[outcome_col]],
          estimate = preds
        ),
        truth = "truth",  # Changed to string
        estimate = "estimate"  # Changed to string
      )
    }, .id = "method")

    # Reshape metrics for easier reading
    metrics |>
      tidyr::pivot_wider(
        names_from = .data$.metric,
        values_from = .data$.estimate
      )
  })

  # Calculate average performance if multiple test sets
  avg_performance <- if (length(test_data) > 1) {
    purrr::reduce(performance_list, dplyr::bind_rows) |>
      dplyr::group_by(.data$method) |>
      dplyr::summarize(
        avg_rmse = mean(.data$rmse),
        sd_rmse = stats::sd(.data$rmse),
        .groups = "drop"
      )
  } else {
    performance_list[[1]]
  }

  # Return complete results
  list(
    fitted_models = fitted_models,
    predictions = purrr::map(test_data, ~ crosscluster_predict(fitted_models, .)),
    individual_performance = performance_list,
    average_performance = avg_performance
  )
}

#' Plot performance comparison across methods
#'
#' @param workflow_results Output from cluster_ensemble_workflow
#' @return ggplot object
#'
#' @returns Returns a ggplot2 object displaying a bar plot comparing performance metrics
#'   (RMSE) across different methods, with error bars representing standard deviation
#'   when multiple test sets are evaluated
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar labs theme_minimal theme element_text
#' @importFrom dplyr mutate
#'
#' @export
plot_performance_comparison <- function(workflow_results) {
  metric <- "rmse"
  # Extract performance data
  perf_data <- workflow_results$average_performance

  # Create plot
  metric_col <- if (length(workflow_results$predictions) > 1) {
    paste0("avg_", metric)
  } else {
    metric
  }

  sd_col <- paste0("sd_", metric)
  if (!(sd_col %in% colnames(perf_data))) {
    perf_data <- perf_data |>
      dplyr::mutate("{sd_col}" := 0)
  }

  plot <- ggplot2::ggplot(
    perf_data,
    ggplot2::aes(x = .data$method, y = .data[[metric_col]])
  ) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data[[metric_col]] - .data[[sd_col]],
        ymax = .data[[metric_col]] + .data[[sd_col]]
      ),
      width = 0.2
    ) +
    ggplot2::labs(
      title = paste("Performance Comparison -", toupper(metric)),
      x = "Method",
      y = toupper(metric)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(plot)
}
