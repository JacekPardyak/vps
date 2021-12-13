library(shiny)
library(tidyverse)
library(tictactoe)

colours <- data.frame(State = c(0:2), Colour = c("grey", "white", "black"))
factors <- data.frame(State = c(0:2), Factor = c("board", "player", "computer"))
states <- matrix(sample(c(0:2), 9, TRUE), nrow = 3)

states %>% cbind(paste0("R", c(1:3))) %>% as.data.frame() %>%
  `colnames<-`(c(paste0("C", c(1:3)), "Row")) %>% gather("Column", "State", -Row) %>% 
  mutate(State = as.numeric(State)) %>% left_join(colours) %>%
  left_join(factors) %>% ggplot() + 
  aes(x = Column, y = Row, fill = Colour) +
  geom_tile() +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_discrete(limits=rev) 

#names(states) <- c(paste0("C", c(1:3)), "Row")