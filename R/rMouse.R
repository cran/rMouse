#' rMouse: Automate mouse clicks and send keyboard input
#'
#' \code{rMouse} is inspired by packages like \code{pyautogui} in Python. The goal of the package is to let users create simple mouse and keyboard macros to automate user input.
#' The \code{rMouse} package utilizes the Java Robot Class and depends on the \code{rJava} package. Make sure the R version corresponds to the Java version, e.g. 64-bit R with 64-bit Java. Troubleshooting \code{rJava} can be done via its documentation or online.
#'
#' \code{rMouse} creates a Java Robot Object (\code{jRobot}) and an Mouse Info Object (\code{jMouseInformation}) upon loading. The functions in the package are wrapper functions to call the methods of the Java objects.
#' The functions can be divided in three categories: delays, mouse input and keyboard input. Delay functions are \code{setAutoDelay(ms)} and \code{delay(ms)}, and work similar to Base R's \code{Sys.sleep(time)}. The delay functions specify the time interval between commands where the system waits.  \code{move(x,y)}, \code{left()} and \code{right()} are mouse input functions to move the mouse cursor and send a left or a right click.
#' With \code{coord()} and \code{pos()} it is possible to get the current position of the mouse cursor. \code{record()} is a work-around solution to record \code{n} mouse moves after a specified time interval has passed (e.g. every 5 seconds), as the Java Robot Class does not provide any methods to record mouse clicks. Moreover, it prints out an R code template to the console which can be used to rapidly build mouse macros. Finally, there are two functions to send keyboard input: \code{type(string)} allows to generate key presses for [0-9], [A-Z] and [a-z]. Spaces, periods and commas are allowed, however some special characters like "!" and "?" throw an error as the virtual key mapping in Java deviates from the generated raw bytes in R. \code{specialKey(key)} allows to send "ESC", "ENTER" or "TAB" key presses.
#'
#' Each function has a build-in emergency stop procedure. If the mouse cursor is in the top left corner at pixel(x = 0, y = 0), then the execution is suspended. By setting the \code{failSafe} parameter of the function to \code{FALSE}, the emergency stop procedure is turned off.
#'
#' @examples
#' \dontrun{
#' setAutoDelay(ms)  # automatic delay in milliseconds
#' delay(1000)       # wait 1000 miliseconds
#'
#' move(0,0)  # move to top left corner; pixel(0,0)
#' left()     # left click
#' right()    # right click
#' coord()    # return cursor coordinates as a list
#' pos()      # print cursor coordinates to console
#' record()   # record mouse moves and print template
#'
#' type(string)     # type a string; no special characters
#' specialKey(key)  # send "ESC", "ENTER" or "TAB"
#'}
#'
#' @docType package
#' @name rMouse
NULL
