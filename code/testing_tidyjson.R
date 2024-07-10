install.packages("tidyjson")
library(tidyjson)
tidyjson::read_json() # to specify from jsonlite
library(purrr)
library(repurrrsive)

z <- tidyjson::read_json(got_chars)
# GoT won't read into same tbl_json we need for this
