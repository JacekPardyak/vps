# pins
paths <- pins::pin_get("nltkdata/movie-review", "local")
# we only need the movie_review.csv file
path <- paths[1]

library(pins)

board <- board_temp()
board

board %>% pin_write(head(mtcars), "mtcars")

board %>% pin_read("mtcars")


# it doesnt work - so data downloades from 
# https://www.kaggle.com/nltkdata/movie-review#movie_review.csv