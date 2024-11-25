#' Create clusters from training data
#' @param train_data Training dataset (tibble/data.frame)
#' @param n_clusters Number of clusters to create
#' @param outcome_col Name of outcome column
#' @return List containing cluster assignments and clustered data
#' @export
create_clusters <- function(train_data, n_clusters, outcome_col = "y") {
  # Setup recipe for normalization
  cluster_recipe <- recipes::recipe(~., data = dplyr::select(train_data, -dplyr::all_of(outcome_col))) |>
    recipes::step_normalize(recipes::all_predictors())

  # Prepare data for clustering
  cluster_prep <- recipes::prep(cluster_recipe)
  cluster_data <- recipes::bake(cluster_prep, new_data = train_data)

  # Perform kmeans clustering
  kmeans_fit <- stats::kmeans(
    x = cluster_data,
    centers = n_clusters,
    nstart = 25,
    iter.max = 50
  )

  # Add cluster assignments to original data
  clustered_data <- dplyr::bind_cols(
    train_data,
    cluster = factor(kmeans_fit$cluster)
  ) |>
    dplyr::group_split(cluster) |>
    purrr::keep(~ nrow(.) > 2) |> # Remove any clusters with 2 or fewer observations
    purrr::map(~ dplyr::select(., -cluster))

  list(
    clusters = clustered_data,
    kmeans_fit = kmeans_fit,
    recipe = cluster_prep,
    centers = kmeans_fit$centers
  )
}
