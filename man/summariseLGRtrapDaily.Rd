% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseLGRtrapDaily.R
\name{summariseLGRtrapDaily}
\alias{summariseLGRtrapDaily}
\title{Summarise LGR Trap Database}
\usage{
summariseLGRtrapDaily(
  trap_df,
  spp = c("Chinook", "Steelhead", "Coho"),
  incl_clip_sthd = TRUE,
  sthd_B_run = FALSE
)
}
\arguments{
\item{trap_df}{data.frame as created by \code{readLGRtrapDB}.}

\item{spp}{species to summarise trap data for. Possible species are: \code{Chinook}, \code{Steelhead}, and \code{Coho}.}

\item{incl_clip_sthd}{should clipped steelhead in the trap be included in this summary? Should match with the window counts. Default is \code{TRUE}.}

\item{sthd_B_run}{should numbers of B run steelhead be reported? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.}
}
\description{
Summarise LGR trap database on dailiy time-step
}
\examples{
#summariseLGRtrapDaily(readLGRtrapDB())
}
\author{
Kevin See
}
