#' Complete clustering ensemble workflow
#'
#' @param train_data Either a single dataframe or a list of pre-clustered dataframes
#' @param test_data Either a single dataframe or a list of test dataframes
#' @param n_clusters Number of clusters to create if train_data is a single dataframe
#' @param outcome_col Name of outcome column
#' @param merged_trees Number of trees for merged model
#' @param cluster_trees Number of trees for individual cluster models
#' @param cluster_ind Boolean representing if we should perform k-means clustering on the merged train_data or keep the original structure
#' @param n_cores Number of cores for parallel processing (default = 2)
#' @param seed Random seed for reproducibility
#'
#' @return List containing fitted models, predictions, and performance metrics
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
  test_data <- if (!is.list(test_data)) {
    list(test_data)
  } else {
    test_data
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
        truth = truth,
        estimate = estimate
      )
    }, .id = "method")

    # Reshape metrics for easier reading
    metrics |>
      tidyr::pivot_wider(
        names_from = .metric,
        values_from = .estimate
      )
  })

  # Calculate average performance if multiple test sets
  avg_performance <- if (length(test_data) > 1) {
    purrr::reduce(performance_list, dplyr::bind_rows) |>
      dplyr::group_by(method) |>
      dplyr::summarize(
        avg_rmse = mean(rmse),
        sd_rmse = stats::sd(rmse),
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
    average_performance = avg_performance,
    input_info = list(
      n_train_clusters = length(clusters_list),
      n_test_sets = length(test_data),
      outcome_col = outcome_col,
      merged_trees = merged_trees,
      cluster_trees = cluster_trees,
      cluster_ind = cluster_ind,
      seed = seed
    )
  )
}


#' Plot performance comparison across methods
#'
#' @param workflow_results Output from cluster_ensemble_workflow
#' @return ggplot object
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

  plot <- ggplot2::ggplot(
    perf_data,
    ggplot2::aes(x = method, y = .data[[metric_col]])
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
