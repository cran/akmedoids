% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alpha_label.R
\name{alpha_label}
\alias{alpha_label}
\title{Numerics ids to alphabetical ids}
\usage{
alpha_label(x)
}
\arguments{
\item{x}{A vector of numeric ids}
}
\value{
A vector of alphabetical ids.
}
\description{
Function to transform a list of numeric ids
to alphabetic ids
}
\details{
Given a vector of numeric cluster ids,
`alpha_label` converts each id to its corresponding alphabets.
It combines alphabets for ids greater than 26.
}
\examples{

data(TO1Risk)

set.seed(1000)
#pick 4 random clusters
center <- TO1Risk[runif(4,1,nrow(TO1Risk)), ]

#Assigning each individual to nearest centre
numeric_Labels <- kml::affectIndivC(TO1Risk, center)

mode(numeric_Labels)

#transform numeric cluster labels to alphabets
alphab_Labels <- alpha_label(numeric_Labels)

mode(alphab_Labels)

}
