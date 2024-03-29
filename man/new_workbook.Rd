% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_workbook.R
\name{new_workbook}
\alias{new_workbook}
\title{Create and load new excel workbook}
\usage{
new_workbook(filename, sheet = NULL, overwrite = TRUE)
}
\arguments{
\item{filename}{Workbook filename without extension (.xlsx)}

\item{sheet}{Name of sheet to be created}

\item{overwrite}{If \code{TRUE} overwrites an existing file. \code{default=TRUE}}
}
\value{
Creates and loads workbook
}
\description{
Creates and loads new excel workbook with custom sheet name
}
\examples{
# wb <- new_workbook("test")
}
\author{
Gutama Girja Urago
}
