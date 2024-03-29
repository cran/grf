% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_scores.R
\name{get_scores.causal_survival_forest}
\alias{get_scores.causal_survival_forest}
\title{Compute doubly robust scores for a causal survival forest.}
\usage{
\method{get_scores}{causal_survival_forest}(forest, subset = NULL, num.trees.for.weights = 500, ...)
}
\arguments{
\item{forest}{A trained causal survival forest.}

\item{subset}{Specifies subset of the training examples over which we
estimate the ATE. WARNING: For valid statistical performance,
the subset should be defined only using features Xi, not using
the treatment Wi or the outcome Yi.}

\item{num.trees.for.weights}{Number of trees used to estimate Var[W | X = x]. Note: this
argument is only used in the case of a continuous treatment
(see \code{\link{get_scores.causal_forest}} for details).}

\item{...}{Additional arguments (currently ignored).}
}
\value{
A vector of scores.
}
\description{
For details see section 3.2 in the causal survival forest paper.
}
