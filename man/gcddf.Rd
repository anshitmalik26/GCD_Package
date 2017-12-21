\name{GCD}
\alias{GCD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Great Circle Distance
}
\description{
Takes in a dataframe with latitute and longitude columns and gives another column with distance between 2 consecutive pairs of location
}
\usage{
GCD(x)
}

\arguments{
  \item{data}{
a dataframe with latitude and longitude information
}
  \item{lat}{
Index of latitude column
}
  \item{long}{
Index of longitude column
}
}

\value{
Returns the original dataframe with a new column at the end that contains the distance between two consecutive pairs of latitude and longitude
}
\references{
https://en.wikipedia.org/wiki/Great-circle_distance
}
\author{
Anshit Malik
}
