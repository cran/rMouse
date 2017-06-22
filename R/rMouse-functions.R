is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
abs(x - round(x)) < tol
}

#' Set automatic delay between commands
#'
#' Set the time delay in milliseconds between commands.
#' @param ms (milliseconds). Default is \code{100}.
#' @keywords setAutoDelay
#' @export
#' @examples
#' \dontrun{
#' setAutoDelay()
#' setAutoDelay(1000)   # 1 Second between commands
#' move(100, 100)
#' move(50, 50)
#' }
#' @seealso \code{\link{delay}}
setAutoDelay <- function(ms = 100) {
  stopifnot(is.wholenumber(ms))
  jRobot$setAutoDelay(as.integer(ms))
}

#' Time Delay
#'
#' Set the time delay in milliseconds manually.
#' @param ms (milliseconds). Default is \code{500}.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords delay
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#' @export
#' @examples
#' \dontrun{
#' delay(2000)   # wait 2 seconds
#' move(0,0)
#' }
#' @seealso \code{\link{setAutoDelay}}
delay <- function(ms = 500, failSafe = TRUE) {
  if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {stop("Coord: 0,0 (Emergency Stop)")}
  stopifnot(is.wholenumber(ms))
  jRobot$delay(as.integer(ms))
}

#' Move Mouse Cursor to Pixel(x,y)
#'
#' Moves the mouse cursor to the specified pixel. Top left corner of screen is pixel(0,0).
#' @param x horizontal coordinate as integer.
#' @param y vertical coordinate as integer.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords move
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#' @export
#' @examples
#' \dontrun{
#' move(50, 50)
#' delay(1000)
#' move(364, 200)
#' }
#' @seealso \code{\link{delay}}, \code{\link{left}}, \code{\link{right}}, \code{\link{pos}}, \code{\link{record}}
move <- function(x, y, failSafe = TRUE) {
  if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {stop("Coord: 0,0 (Emergency Stop)")}
  stopifnot(is.wholenumber(x) && is.wholenumber(y))
  jRobot$mouseMove(as.integer(x), as.integer(y))
}


#' Left Click
#'
#' Sends a left click, when called.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords left
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#' @export
#' @examples
#' \dontrun{
#' move(50, 50)
#' delay(1000)
#' left()
#' }
#' @seealso \code{\link{delay}}, \code{\link{move}}, \code{\link{right}}, \code{\link{pos}}, \code{\link{record}}
left <- function(failSafe = TRUE) {
  if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {stop("Coord: 0,0 (Emergency Stop)")}
  jRobot$mousePress(as.integer(16))
  jRobot$mouseRelease(as.integer(16))
}


#' Right Click
#'
#' Sends a right click, when called.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords left
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#' @export
#' @examples
#' \dontrun{
#' move(50, 50)
#' delay(1000)
#' right()
#' }
#' @seealso \code{\link{delay}}, \code{\link{move}}, \code{\link{left}}, \code{\link{pos}}, \code{\link{record}}
right <- function(failSafe = TRUE) {
  if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {stop("Coord: 0,0 (Emergency Stop)")}
  jRobot$mousePress(as.integer(4))
  jRobot$mouseRelease(as.integer(4))
}

#' Get Coordinates of Mouse Cursor
#'
#' Get the coordinates of the current location of the mouse cursor.
#' @keywords coord, coordinates
#' @return Returns a list with the coordinates.
#' @export
#' @examples
#' \dontrun{
#' coord()
#' coord()$x
#' coord()$y
#' }
#' @seealso \code{\link{pos}}, \code{\link{record}}
coord <- function() {
  x <- jMouseInfo$getPointerInfo()$getLocation()$x
  y <- jMouseInfo$getPointerInfo()$getLocation()$y
  return(list(x = x, y = y))
}


#' Current position of mouse cursor
#'
#' Print the position of the current location of the mouse cursor.
#' @keywords position, coordinates
#' @return Prints "move(x,y)" to the console.
#' @note The console output can be pasted to a script, allowing to write mouse macros faster.
#' @export
#' @examples
#' \dontrun{
#' pos()
#' }
#' @seealso \code{\link{coord}}, \code{\link{record}}
pos <- function() {
  cat("move(", coord()$x, ",", coord()$y,")", sep = "")
}



#' Record mouse movements
#'
#' Record subsequent mouse movements and print to console.
#' @param n number of mouse moves to record. Default is \code{10}.
#' @param timeInterval (seconds). Record after how much seconds. Default is \code{5}.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords record, recording
#' @return Prints "delay(2000); move(x,y); left()" to the console.
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#'
#' The record function calls the \code{coord()} function after the specified time interval passed and repeats \code{n} times. The console output can be pasted to a script to rapidly build a mouse macro. Unfortunately, this function is a work-around since it is not possible to record mouse events with the Java Robot Class. The console output is basically a template and can be adjusted to the needs of the user.
#' @export
#' @examples
#' \dontrun{
#' record()
#' }
#' @seealso \code{\link{coord}}, \code{\link{pos}}
record <- function(n = 10, timeInterval = 5, failSafe = TRUE) {
  for (i in 1:n) {
    if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {break}
    delay(timeInterval * 1000, failSafe)
    if(coord() != c(0,0) || !failSafe) {
      cat("delay(2000); move(",coord()$x, ",", coord()$y, "); left() \n", sep = "")
    }
  }
}


#' Type keys
#'
#' Type keyboard input by sending key press events.
#' @param string String to be typed.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords type
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#'
#' Special characters like \code{"?", "!", ";", "<", ">", "+", "*"} are not allowed, since the mapping of the virtual keys in Java deviates from the R generated raw bytes.
#' @export
#' @examples
#' \dontrun{
#' type("This is a sentence. Hello.")  # types to R console
#' type("Hello World!")                # throws an error due to special character
#' }
#' @seealso \code{\link{specialKey}}, \code{\link{delay}}
type <- function(string, failSafe = TRUE) {
  setAutoDelay(ms = 10)
  string <- as.character(string)
  stopifnot(is.character(string))
  string <- unlist(strsplit(string, ""))

  VK <- function(l) {as.integer(charToRaw(l))}  # Make VK_KeyCode

  keyPress <- function(VK_KeyCode) {			# Key Press Function
    jRobot$keyPress(as.integer(VK_KeyCode))
    jRobot$keyRelease(as.integer(VK_KeyCode))
  }

  # Loop through string
  for (i in 1:length(string)) {
    if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {break} # Fail Condition
    if (string[i] %in% c("?", "!", ";", "<", ">", "+", "*")) {stop("special character used."); break}

    # Check if Upper Letter
    if (string[i] == toupper(string[i]) && VK(string[i]) >= 65){
      string[i] <- VK(string[i])
      jRobot$keyPress(as.integer(16))  # Shift
      keyPress(string[i])
      jRobot$keyRelease(as.integer(16))
    } else {
      # Lower Letter
      if(VK(string[i]) >= 65) {
        string[i] <- VK(string[i]) - 32
      } else {
        string[i] <- VK(string[i])
      }
      keyPress(string[i])
    }
  }
  setAutoDelay()
}


#' Send special key input
#'
#' Send special keys like "ESC", "ENTER" or "TAB"
#' @param key "ESC", "ENTER" or "TAB" as string.
#' @param failSafe Logical flag to allow for emergency stops. Default is \code{TRUE}.
#' @keywords type
#' @note If failSafe is \code{TRUE}, the function checks whether the mouse cursor is at the upper left position of the screen (pixel: x = 0, y = 0) . If so, the function fails and throws an error, allowing for emergency stops during code execution.
#'
#' @export
#' @examples
#' \dontrun{
#' specialKey("ENTER")  # send ENTER
#' specialKey("ESC")    # send ESC
#' }
#' @seealso \code{\link{type}}, \code{\link{delay}}
specialKey <- function(key = "ESC", failSafe = TRUE) {
  stopifnot(key %in% c("ESC", "ENTER", "TAB"))
  if (failSafe == TRUE && coord()$x == 0 && coord()$y == 0) {stop("Coord: 0,0 (Emergency Stop)")}
  # Key Mapping
  if (key == "ESC") {
    VK_KeyCode <- 27
  } else if (key == "ENTER") {
    VK_KeyCode <- 10
  } else if (key == "TAB") {
    VK_KeyCode <- 9
  } else {
    VK_KeyCode <- 32   # BackSpace
  }

  jRobot$keyPress(as.integer(VK_KeyCode))
  jRobot$keyRelease(as.integer(VK_KeyCode))
}
