# working with data in lists
# https://mybinder.org/v2/gh/ciakovx/ciakovx.github.io/master?filepath=jennybc_lists_lesson.ipynb
# based on https://jennybc.github.io/purrr-tutorial/ls00_inspect-explore.html 


install.packages("png")
install.packages("repurrrsive")
install.packages("purrr")
install.packages("listviewer")
install.packages("tibble")

# load libraries
library(png)
library(repurrrsive)
library(purrr)
library(listviewer)
library(tibble)


# wes anderson color palettes ---------------------------------------------

wesanderson <- wesanderson

str(wesanderson)

jsonedit(wesanderson, mode = "view", elementId = "wesanderson")


# game of thrones POV characters ------------------------------------------

got_chars <- got_chars

jsonedit(got_chars, mode = "view", elementId = "got_chars")

jsonedit(number_unnamed(got_chars), mode = "view", elementId = "got_chars")



# vectorized and "list-ized" operations -----------------------------------

# square of each element in the vector
(3:5) ^ 2

# square root of each element in the vector
sqrt(c(9, 16, 25))

# for each element in the vector, take the square root
# result will be returned as a list
map(c(9, 16, 25), sqrt)


## map(YOUR_LIST, YOUR_FUNCTION)

# for elements 1-4 in the list, extract the name value
map(got_chars[1:4], "name")

# for elements 5-8 in the list, extract the third value
map(got_chars[5:8], 3)

# piping
got_chars %>%
  map("name")
got_chars %>%
  map(3)

# for elements in the list, extract the name value
# map_chr will return results as a character vector
map_chr(got_chars[9:12], "name")


# rectangling: manually create data frame from list
got_chars_df <- got_chars %>% {
  tibble(
    name = map_chr(., pluck, "name", .default = NA_character_),
    culture = map_chr(., pluck, "culture", .default = NA_character_),
    gender = map_chr(., pluck, "gender", .default = NA_character_),       
    id = map_int(., pluck, "id", .default = NA_integer_),
    born = map_chr(., pluck, "born", .default = NA_character_),
    alive = map_lgl(., pluck, "alive", .default = NA)
  )
}




