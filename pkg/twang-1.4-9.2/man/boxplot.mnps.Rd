\name{boxplot.mnps}
\alias{boxplot.mnps}

\title{Boxplots for mnps objects}

\description{This function produces a collection of diagnostic plots for mnps objects. }

\usage{ 
\method{boxplot}{mnps}(x, stop.method = NULL, color = TRUE, \dots)
}

\arguments{
  \item{x}{A ps object}
\item{stop.method}{Only 1 \code{stop.method} can be presented at a time for \code{mnps} objects.  Use a numeric indicator of which \code{stop.method} (among those specified when fitting the \code{mnps} object) should be used.}
\item{color}{If \code{FALSE}, a grayscale figure will be returned.}
  \item{...}{Additional arguments that may be passed to the underlying \code{lattice} package plotting functions}
}



\details{
This function produces lattice-style graphics of diagnostic plots. 
 }

\references{
Dan McCaffrey, G. Ridgeway, Andrew Morral (2004). \dQuote{Propensity Score Estimation
with Boosted Regression for Evaluating Adolescent Substance Abuse Treatment,}
\emph{Psychological Methods} 9(4):403-425.
}


\seealso{ \code{\link{ps}} }
