% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_transitionProbabilities.R
\name{matrix_transitionProbabilities}
\alias{matrix_transitionProbabilities}
\title{matrix_transitionProbabilities}
\usage{
matrix_transitionProbabilities(com)
}
\arguments{
\item{com}{the Conditional Occurrence Matrix.}
}
\value{
a data.frame with probabilities. instead number of occurrences.
}
\description{
calculate the transition probabilities from a com (Conditional Occurrence Matrix).
basicaly what this will do id to divide each value for the sum of the values of their row.
}
\references{
Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
}
