#' @import rlang
NULL

#' Create clusters from training data
#'
#' @param train_data Training dataset (tibble or data.frame)
#' @param n_clusters Number of clusters to create
#' @param outcome_col Name of outcome column
#'
#' @returns Returns a list containing:
#'   \item{clusters}{A list of tibbles, one per cluster, of the partitioned original data.
#'                  Clusters with 2 or fewer observations
#'                  are removed.}
#'   \item{kmeans_fit}{The fitted kmeans object containing cluster assignments and
#'                    additional clustering statistics}
#'   \item{recipe}{The prepped recipe object used for data normalization}
#'   \item{centers}{Matrix of cluster centers from the kmeans fit}
#'
#' @importFrom recipes recipe step_normalize all_predictors prep bake
#' @importFrom dplyr select all_of bind_cols group_split
#' @importFrom purrr keep map
#' @importFrom stats kmeans
#'
#' @export
create_clusters <- function(train_data, n_clusters, outcome_col = "y") {
  # Setup recipe for normalization
  cluster_recipe <- recipes::recipe(
    ~.,
    data = dplyr::select(train_data, -dplyr::all_of(outcome_col))
  ) |>
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
    dplyr::group_split(.data$cluster) |>
    purrr::keep(~ nrow(.) > 2) |> # Remove any clusters with 2 or fewer observations
    purrr::map(~ dplyr::select(., -"cluster"))

  list(
    clusters = clustered_data,
    kmeans_fit = kmeans_fit,
    recipe = cluster_prep,
    centers = kmeans_fit$centers
  )
}
