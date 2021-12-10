library(shiny)
library(tidyverse)
runGitHub(repo = 'r-cade-games', username = 'JacekPardyak', subdir = 'pong')

library(shiny)
if (!require("sets")) install.packages("sets")
library(sets)
if (!require("tictactoe")) install.packages("tictactoe")
library(tictactoe)

HEIGHT = 300
WIDTH = 300 

#  |A|B|C
# P|
# R|
# S|

A = interval(        0, WIDTH/3, "[)")
B = interval(  WIDTH/3, 2*WIDTH/3, "[)")
C = interval(2*WIDTH/3, WIDTH, "[]")

P = interval(2*HEIGHT/3, HEIGHT, "[]")
R = interval(  HEIGHT/3, 2*HEIGHT/3, "[)")
S = interval(         0, HEIGHT/3, "[)")


# mose click
plot_click <- list()
plot_click$x <- 300
plot_click$y <- 300

x = interval(plot_click$x, plot_click$x, "[]")
y = interval(plot_click$y, plot_click$y, "[]")

position = 0 # any number possible but effective 1 - 9

get_position <- function(x, y){
if(interval_intersection(A, x) == x & interval_intersection(P, y) == y) {position = 1}
if(interval_intersection(A, x) == x & interval_intersection(R, y) == y) {position = 2}
if(interval_intersection(A, x) == x & interval_intersection(S, y) == y) {position = 3}
if(interval_intersection(B, x) == x & interval_intersection(P, y) == y) {position = 4}
if(interval_intersection(B, x) == x & interval_intersection(R, y) == y) {position = 5}
if(interval_intersection(B, x) == x & interval_intersection(S, y) == y) {position = 6}
if(interval_intersection(C, x) == x & interval_intersection(P, y) == y) {position = 7}
if(interval_intersection(C, x) == x & interval_intersection(R, y) == y) {position = 8}
if(interval_intersection(C, x) == x & interval_intersection(S, y) == y) {position = 9}
position}

position = get_position(x, y)
position
# ===========================
game <- ttt_game()

tt <- game$show_board()
tt
game$show_board() %>% capture.output() %>% paste(collapse = "\n") %>% I() %>% read_csv()
  gsub(pattern = "A", replacement = "\s A")%>% 

#-> text #%>% I() %>% read_delim(delim = " ")

text <- text  %>% gsub(pattern = "-", replacement ="- ")

text %>% I() %>% read_delim(delim = " ")

 #%>% gsub(pattern = "A", replacement ="")


?gsub



# plot next state
position = 1
game <- ttt_game()
game$play(position)
game$play(2)
print(game)
#cat(game$next_state(position = 1 + ttt_rv$position), "\n")

game$state # -> state

library(plot.matrix)

plot(state, breaks=range(state))

