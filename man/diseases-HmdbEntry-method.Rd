% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4.R
\name{diseases,HmdbEntry-method}
\alias{diseases,HmdbEntry-method}
\alias{diseases}
\title{extract disease associations}
\usage{
\S4method{diseases}{HmdbEntry}(x)
}
\arguments{
\item{x}{HmdbEntry instance}
}
\value{
DataFrame
}
\description{
extract disease associations
}
\examples{
data(hmdb1)
diseases(hmdb1)
}
