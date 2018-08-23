# Telethon Kids Institute custom .Rprofile script
# see https://www.statmethods.net/interface/customizing.html
# Template v1.0 created by Paul Stevenson 17-May-2018

# Set default CRAN repository
# local({r <- getOption("repos")
# r["CRAN"] <- "https://cran.curtin.edu.au/"
# options(repos=r)})

# Things you might want to change
options(papersize="a4") 
# options(editor="notepad") 
# options(pager="internal")

# R interactive prompt 
# options(prompt="> ")
# options(continue="+ ") 

# to prefer Compiled HTML 
# help options(chmhelp=TRUE) 
# to prefer HTML help 
options(htmlhelp = TRUE) 

# General options 
options(tab.width = 2) 
options(width = 130)
options(graphics.record=TRUE) 

.First <- function(){
  library(stats)
  library(reshape2)
  library(plyr)
  library(tidyverse)
  library(lubridate)
  library(ProjectTemplate)
  library(repmis)
  cat("\nWelcome -", date(), "\n") 
}

.Last <- function(){ 
  cat("\nGoodbye at ", date(), "\n")
}

# Custom mappings
h <- utils::head