% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeling.R
\name{crosscluster_fit}
\alias{crosscluster_fit}
\title{Fit cross-cluster weighted forests method}
\usage{
crosscluster_fit(
  train_data,
  outcome_col = "y",
  model_fit = rf_fit,
  model_predict = rf_predict,
  merged_trees = 500,
  cluster_trees = 100,
  cluster_ind = TRUE,
  n_clusters = 10,
  n_cores = 2
)
}
\arguments{
\item{train_data}{Either a single dataframe or a list of pre-clustered dataframes}

\item{outcome_col}{Name of outcome column}

\item{model_fit}{Function to fit models (defaults to rf_fit)}

\item{model_predict}{Function to make predictions (defaults to rf_predict)}

\item{merged_trees}{Number of trees for merged model}

\item{cluster_trees}{Number of trees for individual cluster models}

\item{cluster_ind}{Boolean representing if we should perform k-means clustering}

\item{n_clusters}{Number of clusters for k-means. Only used if cluster_ind = TRUE}

\item{n_cores}{Number of cores for parallel processing (default = 2)}
}
\value{
Returns a list containing:
\item{merged_model}{A single random forest model fitted on all data combined}
\item{cluster_models}{List of models, one fitted to each cluster}
\item{stack_ridge}{Fitted ridge regression model for the stacking component}
\item{stack_lasso}{Fitted lasso regression model for the stacking component}
\item{stack_ridge_lambda}{Optimal lambda value for ridge regression}
\item{stack_lasso_lambda}{Optimal lambda value for lasso regression}
\item{stack_ridge_coef}{Coefficients from ridge regression stacking model}
\item{stack_lasso_coef}{Coefficients from lasso regression stacking model}
\item{model_fit}{The base ensemble member fitting function used}
\item{model_predict}{The base ensemble member prediction function used}
}
\description{
Fit cross-cluster weighted forests method
}
