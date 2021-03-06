% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HSM_9_35.R
\name{spf}
\alias{spf}
\title{spf}
\usage{
spf(
  AADTMAJ,
  AADTMIN,
  L,
  Base_Condition,
  Provide_Overdispersion_Factor,
  Segment
)
}
\arguments{
\item{AADTMAJ}{value for major volume AADT.}

\item{AADTMIN}{value for minor volume AADT.}

\item{L}{Length specified in miles. Applies only to segments.}

\item{Base_Condition}{Corresponding base condition or the title of the SPF.}

\item{Provide_Overdispersion_Factor}{TRUE or FALSE Parameter. Specify TRUE for overdispersion parameter k.}

\item{Segment}{TRUE or FALSE parameter. Specify TRUE for segment SPF. Depends on Major AADT and Length.}
}
\value{

}
\description{
spf
}
\examples{

spf(AADTMAJ=100,AADTMIN=NULL,L=100,Base_Condition= "HSM-RUR2U-KABCO",Provide_Overdispersion_Factor=FALSE,Segment=TRUE)

spf(AADTMAJ=100,AADTMIN=100,L=NULL,Base_Condition= "HSM-RUR2-3ST-KABCO",Provide_Overdispersion_Factor=FALSE,Segment=FALSE)

}
