% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coursekata_attach.R
\name{coursekata_attach}
\alias{coursekata_attach}
\title{Attach the CourseKata course packages}
\usage{
coursekata_attach(do_not_ask = FALSE, quietly = FALSE)
}
\arguments{
\item{do_not_ask}{Prevent asking the user to install missing packages (they are skipped).}

\item{quietly}{Whether to suppress messages.}
}
\value{
A named logical vector indicating which packages were attached.
}
\description{
Attach the CourseKata course packages
}
\examples{
coursekata_attach()
}
