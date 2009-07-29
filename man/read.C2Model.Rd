\name{read.C2Model}
\alias{read.C2Model}
\alias{print.C2}
\alias{summary.C2}

\title{Imports results from a C2 model into R}
\description{
Imports data from a C2 model saved in Excel format into R for further processing.
}
\usage{
read.C2Model(fName)

\method{print}{C2}(x, \dots)

\method{summary}{C2}(object, \dots)
}

\arguments{
  \item{fName}{ filename to read. }
  \item{x, object}{ object of class \code{C2} produced by \code{read.C2Model}. }
  \item{\dots}{ additional arguments to \code{plot} and \code{print}. }
  
}

\details{
Function to import data from a C2 model into R for further processing. C2 is a Windows program for the analysis and visualisation of palaeoecological data (Juggins (2007). The code uses the RDOBC package to read the Excel file.
}
\value{
Returns a list with names elements corresponding to the worksheets in the saved C2 model.
}

\author{
Steve Juggins
}

\source{
Juggins. S. (2007) \emph{C2 Version 1.5 User guide. Software for ecological and palaeoecological data analysis and visualisation}. Newcastle University, Newcastle upon Tyne, UK. 73pp.
}

\seealso{ 
\code{\link[rioja]{read.CEP}}.
}

\examples{
\dontrun{
pth <- system.file("example.datasets/C2_IK_Model.xls", package="rioja")
C2.IK <- read.C2Model(pth)
summary(C2.IK)
}
}
\keyword{ IO }
\keyword{ file }
