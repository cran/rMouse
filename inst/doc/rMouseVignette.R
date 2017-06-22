## ---- eval = FALSE-------------------------------------------------------
#  library(rMouse)
#  setAutoDelay()    # default

## ---- eval = FALSE-------------------------------------------------------
#  delay(1000)      # wait 1 second

## ---- eval = FALSE-------------------------------------------------------
#  move(0,0)       # move to top left corner (0,0)
#  move(50,30)     # move to pixel x = 50, y = 30
#  left()          # left click
#  right()         # right click

## ---- eval = FALSE-------------------------------------------------------
#  coord()       # returns a list
#  pos()         # prints to console
#  #> move(x,y)

## ---- eval = FALSE-------------------------------------------------------
#  # 3 records, 3 seconds in between recordings
#  record(n = 3, timeInterval = 3)
#  #> delay(2000); move(412,668); left()
#  #> delay(2000); move(155,636); left()
#  #> delay(2000); move(387,697); left()

## ---- eval = FALSE-------------------------------------------------------
#  type("This is a sentence.")  # a String to be typed, no spec char
#  type("!")                    # throws an error
#  specialKey("ESC")            # send ESC key
#  specialKey("ENTER")          # send ENTER key

