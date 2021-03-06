% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4.R
\docType{class}
\name{HmdbEntry-class}
\alias{HmdbEntry-class}
\title{basic container for an HMDB metabolite entry}
\description{
basic container for an HMDB metabolite entry
}
\section{Slots}{

\describe{
\item{\code{metabolite}}{character(1) institutional name of metabolite}

\item{\code{id}}{HMDB identifier}

\item{\code{diseases}}{S4Vectors DataFrame instance listing associated diseases}

\item{\code{biospecimens}}{S4Vectors DataFrame instance listing associated biospecimens}

\item{\code{tissues}}{S4Vectors DataFrame instance listing associated tissues}

\item{\code{store}}{contains parsed XML}
}}

\note{
Ontological tagging of diseases and other associated elements should be considered.
}
