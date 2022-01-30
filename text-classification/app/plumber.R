#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
library(tidytext)
#coefs = read_csv("text-classification/app/coefs.csv")
coefs = read_csv("coefs.csv")
intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

#* @apiTitle Plumber Example API

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}


#* Return language of a text EN/PL
#* @param q The text
#* @get /predict
function(q) {
  q %>% tibble() %>% rename(text = '.') %>% 
    mutate(document = 0) %>%
    unnest_tokens(word, text) %>%
    inner_join(coefs, by = c("word" = "term")) %>%
    group_by(document) %>%
    summarize(score = sum(estimate)) %>%
    mutate(probability = plogis(intercept + score)) %>%
    mutate(language = if_else(probability > 0.5, "EN", "PL")) %>%
    select(probability, language) #%>% pull()
}


#my_fun("I nie wiedziałem co się działo w około mnie")
#my_fun("Gerwazy sat down upon the floor, leaned against the wall, and bent down")
 