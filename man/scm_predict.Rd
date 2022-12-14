% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scm_predict.R
\name{scm_predict}
\alias{scm_predict}
\title{scm_predict}
\usage{
scm_predict(scm, state, horizon, steadyState = TRUE)
}
\arguments{
\item{scm}{the stochastic matrix.}

\item{state}{the initial state. a numeric vector containing the names of the states and probabilities.}

\item{horizon}{a numeric is how much simulate.}

\item{steadyState}{whether the function should stop when found the steadyState probabilities (stable distribution).}
}
\value{
a data.frame with the columns: choosen state, and the probabilities of each state.
}
\description{
do a prediction/simulation using n-step transition probabilities.
}
\references{
Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
}
