\name{read.CEP}
\alias{read.CEP}
\alias{write.CEP}

\title{Reads and writes data in Cornell Ecology Program (CEP) format}
\description{
Reads and writes data in Cornell Ecology Program (CEP) format used by CANOCO and other programs.
}
\usage{
read.CEP(fName, mValue=-99.9, impZero=0)

write.CEP(x, fName, fTitle=fName, type = "condensed",
    nSig=5, nCouplets=4, mValue=-99.9)
}

\arguments{
  \item{fName}{ filename to read. }
  \item{mValue}{ missing value code in the CEP file, converted to NA on import. }
  \item{impZero}{ value to assign to \"missing\" taxa in a condensed CEP file. These are usually implicitly zero and the default is to read them as such. }
  \item{x}{ data frame to save in CEP format. }
  \item{fTitle}{ title for header in CEP file. }
  \item{type}{ format of CEP file. Should be an unambiguous abbreviation of either \code{condensed} or \code{full}. \code{condensed} is usually used for species data and \code{full} for environmental data}
  \item{nSig}{ number of significant digits to include in output. }
  \item{nCouplets}{ number of couplets per line to include in condensed file. }
}

\details{
  \code{read.CEP and write.CEP} are functions to read and write data in the Cornell Ecology Program (CEP) format, originally designed by Mark Hill for us in DECORANA and TWINSPAN, and subsequently used by CANOCO.  The CEP in \code{read.CEP} is capitalised to distinguish it from a similar function (\code{read.cep} in the \code{vegan} package.
}
\value{
Returns a data frame with species as columns and sites as rows.  Column and row names are taken from the CEP file.
}

\note{
The code uses a combination of C and C++, with several function converted to C from FORTRAN using f2c. It works on Windows but has not been test on other platforms.  Please let me know of any problems.
}


\author{ 
Steve Juggins
}

\seealso{ 
\code{\link[vegan]{read.cep}}.
}

\examples{
\dontrun{
pth <- system.file("example.datasets/RLGHLongCore.cep", package="rioja")
rlgh <- read.CEP(pth)
depth <- as.numeric(gsub("r_", "", rownames(rlgh)))
mx <- apply(rlgh, 2, max)
rlgh.sub <- rlgh[, mx > 5]
strat.plot(rlgh.sub, scale.percent=TRUE, yvar=depth, y.rev=TRUE,
wa.order="bottomleft")
}
}
\keyword{ IO }
\keyword{ file }
