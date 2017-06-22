# Initialisation
.onLoad <- function(libname, pkgname){
  rJava::.jinit()                                      # start JVM
  jRobot <<- rJava::.jnew("java/awt/Robot")           # Robot class
  jMouseInfo <<- rJava::.jnew("java/awt/MouseInfo")   # MouseInfo Class
}



