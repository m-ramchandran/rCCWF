% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{sim_data_monte}
\alias{sim_data_monte}
\title{Simulate non-gaussian clusters using the 'monte' function}
\usage{
sim_data_monte(nclusters, ncoef, ntest)
}
\arguments{
\item{nclusters}{Number of clusters to simulate for the training dataset}

\item{ncoef}{Number of coefficients}

\item{ntest}{Number of test datasets to simulate}
}
\value{
List containing two sublists: 'cluster_list' has the simulated training clusters,
and 'test_list' has the simulated test sets
}
\description{
Simulate non-gaussian clusters using the 'monte' function
}
