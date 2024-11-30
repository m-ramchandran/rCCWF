#' @import rlang
NULL

#' Simulate Gaussian mixture data
#'
#' @param nclusters Number of clusters to simulate for the training dataset
#' @param ncoef Number of coefficients
#' @param ntest Number of test datasets to simulate
#'
#' @returns Returns a list with two components:
#'   \item{clusters_list}{A list of training data tibbles, one per cluster. Each tibble contains:
#'     \itemize{
#'       \item y: The simulated outcome variable
#'       \item V1...Vn: The simulated predictor variables, where n equals ncoef
#'     }
#'   }
#'   \item{test_list}{A list of test data tibbles. Each tibble has the same structure as the training data}
#'
#' @importFrom stats rnorm runif
#' @importFrom clusterGeneration genRandomClust
#' @importFrom tibble as_tibble
#'
#' @export
sim_data_gaussian <- function(nclusters, ncoef, ntest) {
  nchoose <- min(10, ncoef)

  # Generate coefficients
  coefs <- sample(c(
    stats::runif(round(nchoose / 2), -5, -0.5),
    stats::runif(nchoose - round(nchoose / 2), 0.5, 5)
  ))
  vars <- sample(1:ncoef, nchoose)
  icoefs <- c(4, 1.8)
  n.noise <- 5

  # Generate clusters using clusterGeneration
  trainGenClust <- clusterGeneration::genRandomClust(
    numClust = nclusters,
    sepVal = 0.8,
    numNonNoisy = ncoef - n.noise,
    numNoisy = n.noise,
    numOutlier = 50,
    numReplicate = 1,
    fileName = "test",
    clustszind = 2,
    clustSizeEq = 200,
    rangeN = c(450, 460),
    covMethod = "eigen"
  )

  # Generate test clusters
  testGenClust <- clusterGeneration::genRandomClust(
    numClust = 2,
    sepVal = 0.8,
    numNonNoisy = ncoef - n.noise,
    numNoisy = n.noise,
    numOutlier = 50,
    numReplicate = ntest,
    fileName = "test",
    clustszind = 2,
    clustSizeEq = 50,
    rangeN = c(500, 600),
    covMethod = "eigen"
  )

  # Process outliers
  outliers <- as.data.frame(scale(
    as.data.frame(trainGenClust$datList)[which(trainGenClust$memList[[1]] == 0), ]
  ))

  # split outliers more evenly among training clusters
  outliers_list <- split(
    outliers,
    ceiling(seq_len(nrow(outliers)) / (nrow(outliers) / nclusters))
  )

  # Generate data for the training clusters and test sets
  clusters_list <- vector("list", nclusters)
  test_list <- vector("list", ntest)

  for (i in 1:(nclusters + ntest)) {
    curcoefs <- sapply(coefs, function(x) stats::runif(1, x - .5, x + .5))

    if (i <= nclusters) {
      data_i <- as.data.frame(trainGenClust$datList)[which(trainGenClust$memList[[1]] == i), ]
      data_i <- scale(rbind(data_i, outliers_list[[i]]))
    } else {
      data_i <- scale(as.data.frame(testGenClust$datList[[i - (nclusters)]]))
    }

    # Generate outcome 'y'
    y <- as.matrix((data_i[, vars]) %*% curcoefs) +
      # quadratic:
      icoefs[1] * (data_i[, vars[1]])^2 + icoefs[2] * (data_i[, vars[2]])^2 +
      # interactions:
      +icoefs[1] * data_i[, vars[1]] * data_i[, vars[2]] -
      icoefs[2] * data_i[, vars[1]] * data_i[, vars[3]] +
      cbind(stats::rnorm(nrow(data_i))) # Added noise

    if (i <= nclusters) {
      clusters_list[[i]] <- tibble::as_tibble(cbind(y = y, data_i)) |>
        stats::setNames(c("y", paste0("V", 1:ncoef)))
    } else {
      test_list[[i - nclusters]] <- tibble::as_tibble(cbind(y = y, data_i)) |>
        stats::setNames(c("y", paste0("V", 1:ncoef)))
    }
  }

  return(list(clusters_list = clusters_list, test_list = test_list))
}

#' Simulate non-gaussian clusters using the 'monte' function
#'
#' @param nclusters Number of clusters to simulate for the training dataset
#' @param ncoef Number of coefficients
#' @param ntest Number of test datasets to simulate
#'
#' @returns Returns a list with two components:
#'   \item{clusters_list}{A list of training data tibbles, one per cluster. Each tibble contains:
#'     \itemize{
#'       \item y: The simulated outcome variable generated using linear, quadratic, and interaction terms
#'       \item V1...Vn: The simulated predictor variables, where n equals ncoef, generated using monte carlo methods
#'     }
#'   }
#'   \item{test_list}{A list of test data tibbles. Each tibble has the same structure as the training data}
#'
#' @importFrom stats rnorm runif
#' @importFrom fungible monte
#' @importFrom tibble as_tibble
#'
#' @export
sim_data_monte <- function(nclusters, ncoef, ntest) {
  clusters_list <- vector("list", nclusters)
  test_list <- vector("list", ntest)
  nchoose <- min(10, ncoef)

  # general predictor-outcome rule:
  coefs <- sample(c(
    stats::runif(round(nchoose / 2), -5, -0.5),
    stats::runif(nchoose - round(nchoose / 2), 0.5, 5)
  ))
  vars <- sample(1:ncoef, nchoose)
  icoefs <- c(4, 1.8)

  cormat <- matrix(.80, ncoef, ncoef)
  diag(cormat) <- rep(1, ncoef)
  in.cor.list <- replicate(nclusters, cormat, simplify = FALSE)

  sim_clusters <- fungible::monte(
    seed = sample(1:1000, 1),
    nvar = ncoef,
    nclus = nclusters + ntest,
    clus.size = floor(stats::runif(nclusters + ntest, 500, 510)),
    eta2 = stats::runif(ncoef, .5, 1),
    cor.list = NULL,
    random.cor = FALSE,
    skew.list = NULL,
    kurt.list = NULL,
    secor = NULL,
    compactness = NULL,
    sortMeans = FALSE
  )

  for (i in 1:(nclusters + ntest)) {
    curcoefs <- sapply(coefs, function(x) {
      stats::runif(1, x - .5, x + .5)
    })

    data_i <- scale(as.data.frame(sim_clusters$data[sim_clusters$data[, 1] == i, ][, -1]))

    # generate outcome
    y <- as.matrix((data_i[, vars]) %*% curcoefs) +
      # quadratic:
      icoefs[1] * (data_i[, vars[1]])^2 + icoefs[2] * (data_i[, vars[2]])^2 +
      # interactions:
      +icoefs[1] * data_i[, vars[1]] * data_i[, vars[2]] -
      icoefs[2] * data_i[, vars[1]] * data_i[, vars[3]] +
      cbind(stats::rnorm(nrow(data_i))) # Added noise

    if (i <= nclusters) {
      clusters_list[[i]] <- tibble::as_tibble(cbind(y = y, data_i)) |>
        stats::setNames(c("y", paste0("V", 1:ncoef)))
    } else {
      test_list[[i - nclusters]] <- tibble::as_tibble(cbind(y = y, data_i)) |>
        stats::setNames(c("y", paste0("V", 1:ncoef)))
    }
  }

  return(list(clusters_list = clusters_list, test_list = test_list))
}
