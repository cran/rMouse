% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMouse-functions.R
\name{type}
\alias{type}
\title{Type keys}
\usage{
type(string, failSafe = TRUE)
}
\arguments{
\item{string}{String to be typed.}

\item{failSafe}{Logical flag to allow for emergency stops. Default is \code{TRUE}.}
}
\description{
Type keyboard input by sending key press events.
}
\note{
If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.

Special characters like \code{"?", "!", ";", "<", ">", "+", "*"} are not allowed, since the mapping of the virtual keys in Java deviates from the R generated raw bytes.
}
\examples{
\dontrun{
type("This is a sentence. Hello.")  # types to R console
type("Hello World!")                # throws an error due to special character
}
}
\seealso{
\code{\link{specialKey}}, \code{\link{delay}}
}
\keyword{type}
