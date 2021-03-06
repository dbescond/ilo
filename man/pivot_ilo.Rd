% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_ilo.r
\name{pivot_ilo}
\alias{pivot_ilo}
\title{R tools to visualyze pivot ilo data frame}
\usage{
pivot_ilo(df)
}
\arguments{
\item{df, }{ilo tbl data frame.}
}
\value{
pivot html table
}
\description{
R tools to visualyze pivot ilo data frame
}
\examples{
## Not run:

init_ilo()


### quarterly time serie of female unemployed as from 2000 in united states

X <- get_ilo(ref_area = 'AFG')
pivot_ilo(X)
## End(**Not run**)
}
