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
game$show_board() %>% capture.output( ) %>% I() #%>% read_csv()

%>% capture.output( )
game$next_state(position)

game$index_to_str(position)

game$nextmover

game$initialize()

game$to_index(position)

game$play(position)


p <- ttt_human()
p

p$getmove() <- 1

# from blog
game <- ttt_game()
game$play(position)
game$play(1)
print(game)

game$play(2)
game$play(7)
print(game)


# next_state returns matrx
m = game$next_state(position)

class(m)
position = 100
game$next_state(position)


# position is changing from 1 to 9


ttt(ttt_human(), ttt_ai())


## Not run: 
p <- ttt_human()
p$getmove() <- 1


encode_postion <- function(x, y){
  
}

## End(Not run)



sounds <- c(ping = "microwave_ping_mono.wav",
            coin = "smb_coin.wav",
            fanfare = "victory_fanfare_mono.wav",
            complete = "work_complete.wav",
            treasure = "new_item.wav",
            ready = "ready_master.wav",
            shotgun = "shotgun.wav",
            mario = "smb_stage_clear.wav",
            wilhelm = "wilhelm.wav",
            facebook = "facebook.wav",
            sword = "sword.wav")





####
library(tictactoe)
game <- ttt_game()

for (i in c(1:9)) {
  print(game$play(i))
  
}

game$play(1)
game$state
print(game)

game$nextmover
game$next_state(1) <- 0
class(game$next_state(2)[1,1] )
class(1)


game$play(2)
game$play(7)
print(game)

game$history

?ttt_game


game$state[1,1] <- 0


# ------------------------------------
# -------- app

HEIGHT = 300
WIDTH = 300 

A = interval(        0, WIDTH/3, "[)")
B = interval(  WIDTH/3, 2*WIDTH/3, "[)")
C = interval(2*WIDTH/3, WIDTH, "[]")

P = interval(2*HEIGHT/3, HEIGHT, "[]")
R = interval(  HEIGHT/3, 2*HEIGHT/3, "[)")
S = interval(         0, HEIGHT/3, "[)")

get_position <- function(p, q){
  x = interval(p, p, "[]"); y = interval(q, q, "[]")
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

get_position(0, 0)
