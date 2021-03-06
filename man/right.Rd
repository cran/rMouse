% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMouse-functions.R
\name{right}
\alias{right}
\title{Right Click}
\usage{
right(failSafe = TRUE)
}
\arguments{
\item{failSafe}{Logical flag to allow for emergency stops. Default is \code{TRUE}.}
}
\description{
Sends a right click, when called.
}
\note{
If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
}
\examples{
\dontrun{
move(50, 50)
delay(1000)
right()
}
}
\seealso{
\code{\link{delay}}, \code{\link{move}}, \code{\link{left}}, \code{\link{pos}}, \code{\link{record}}
}
\keyword{left}
