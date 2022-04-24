library(dplyr)
library(funtimes)

sourcePartial <- function(fn, startTag='#from here', endTag='#to here') {
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}
