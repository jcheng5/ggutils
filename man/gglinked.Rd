% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linked.R
\name{gglinked}
\alias{gglinked}
\title{Linked display of multiple ggplot objects}
\usage{
gglinked(df, ...)
}
\description{
Linked display of multiple ggplot objects
}
\examples{
\dontrun{
library(ggplot2)

gglinked(
  iris,
  ggplot(, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(color = selected_)),
  ggplot(, aes(Petal.Length, Petal.Width)) + geom_point(aes(color = selected_)),
  ggplot(, aes(Species)) + geom_bar(stat = "count", aes(fill = selected_)),
  ggplot(, aes(Species)) + geom_bar(stat = "count", aes(fill = selected_))
)

gglinked(
  mpg,
  ggplot(, aes(cyl, cty)) + geom_point(aes(color = selected_)),
  ggplot(, aes(displ, cty)) + geom_point(aes(color = selected_))
)
}

}

