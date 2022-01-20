library(R6)


Fractal <- R6Class("Fractal",
                   public = list(),
                   private = list())



# ? " _ plot fractal"

# data

# ? " _ animate fractal"

# gganimate

? ttt

ttt(ttt_human(), ttt_random())

x <- ttt_game()
x$play(3)
x$play(5)
x$show_board()

x$undo()
x$show_board()

? R6Class


Game

Queue <- R6Class(
  "Queue",
  public = list(
    initialize = function(...) {
      for (item in list(...)) {
        self$add(item)
      }
    },
    add = function(x) {
      private$queue <- c(private$queue, list(x))
      invisible(self)
    },
    remove = function() {
      if (private$length() == 0)
        return(NULL)
      # Can use private$queue for explicit access
      head <- private$queue[[1]]
      private$queue <- private$queue[-1]
      head
    }
  ),
  private = list(
    queue = list(),
    length = function()
      base::length(private$queue)
  )
)


q <- Queue$new(5, 6, "foo")
q

q$add(10)

q$self

# ---------------------------------------------

fractal <- function(axiom, rules, angle, depth) {
  for (i in 1:depth) {
    axiom = gsubfn(".", rules, axiom)
  }
  
  actions = str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist()
  
  status = data.frame(x = numeric(0),
                      y = numeric(0),
                      alfa = numeric(0))
  points = data.frame(
    x1 = 0,
    y1 = 0,
    x2 = NA,
    y2 = NA,
    alfa = 90,
    depth = 1
  )
  
  
  for (action in actions)
  {
    if (action == "F")
    {
      x = points[1, "x1"] + cos(points[1, "alfa"] * (pi / 180))
      y = points[1, "y1"] + sin(points[1, "alfa"] * (pi / 180))
      points[1, "x2"] = x
      points[1, "y2"] = y
      data.frame(
        x1 = x,
        y1 = y,
        x2 = NA,
        y2 = NA,
        alfa = points[1, "alfa"],
        depth = points[1, "depth"]
      ) %>% rbind(points) -> points
    }
    if (action %in% c("+", "-")) {
      alfa = points[1, "alfa"]
      points[1, "alfa"] = eval(parse(text = paste0("alfa", action, angle)))
    }
    if (action == "[") {
      data.frame(x = points[1, "x1"],
                 y = points[1, "y1"],
                 alfa = points[1, "alfa"]) %>%
        rbind(status) -> status
      points[1, "depth"] = points[1, "depth"] + 1
    }
    
    if (action == "]") {
      depth = points[1, "depth"]
      points[-1, ] -> points
      data.frame(
        x1 = status[1, "x"],
        y1 = status[1, "y"],
        x2 = NA,
        y2 = NA,
        alfa = status[1, "alfa"],
        depth = depth - 1
      ) %>%
        rbind(points) -> points
      status[-1, ] -> status
    }
  }
  points %>%  na.omit()
}



library(tidyverse)

#Plant 3
axiom = "X"
rules = list("X" = "F[+X]F[-X]+X", "F" = "FF")
angle = 20
depth = 7

fractal(
  axiom = axiom,
  rules = rules,
  angle = angle,
  depth = depth
) %>%
  ggplot() +
  aes(x = x1,
      y = y1,
      xend = x2,
      yend = y2) +
  geom_segment(lineend = 'round', colour = 'white') +
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray20"))




# ----------------------------
library(R6)
library(tidyverse)
Fractal <- R6Class(
  "Fractal",
  public = list(
    axiom = character(),
    rules = list(),
    angle = numeric(),
    depth = numeric()
  )
  ,
  active = list(
    # self$axiom = gsubfn::gsubfn(".", self$rules, self$axiom)
    actions = function(value) {
      self$axiom = gsubfn::gsubfn(".", self$rules, self$axiom)
      if (missing(value))
        return(stringr::str_extract_all(self$axiom,
                                        "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% first())
    },
    points = function(value) {
      return(
        data.frame(x1 = 0, y1 = 0, x2 = NA, y2 = NA, alfa=90, depth=1))
      }
  )
)

n <- Fractal$new()

n$axiom <- "X"
n$rules <- list("X"="F[+X]F[-X]+X", "F"="FF")
n$angle <- 10
n$depth <- 1


n$axiom
n$actions 
n$points


status=data.frame(x=numeric(0), y=numeric(0), alfa=numeric(0))
points=data.frame(x1 = 0, y1 = 0, x2 = NA, y2 = NA, alfa=90, depth=1)

n$actions

for (action in n$actions) {
  if (action=="F")
  {
    x=points[1, "x1"]+cos(points[1, "alfa"]*(pi/180))
    y=points[1, "y1"]+sin(points[1, "alfa"]*(pi/180))
    points[1,"x2"]=x
    points[1,"y2"]=y
    data.frame(x1 = x, y1 = y, x2 = NA, y2 = NA, 
               alfa=points[1, "alfa"],
               depth=points[1,"depth"]) %>% rbind(points)->points
  }
  if (action %in% c("+", "-")){
    alfa=points[1, "alfa"]
    points[1, "alfa"]=eval(parse(text=paste0("alfa",action, n$angle)))
  }
  if(action=="["){ 
    data.frame(x=points[1, "x1"], y=points[1, "y1"], alfa=points[1, "alfa"]) %>% 
      rbind(status) -> status
    points[1, "depth"]=points[1, "depth"]+1
  }
  
  if(action=="]"){ 
    depth=points[1, "depth"]
    points[-1,]->points
    data.frame(x1=status[1, "x"], y1=status[1, "y"], x2=NA, y2=NA, 
               alfa=status[1, "alfa"],
               depth=depth-1) %>% 
      rbind(points) -> points
    status[-1,]->status
  }
}

points %>%
ggplot() + 
  aes(x = x1, y = y1, xend = x2, yend = y2) +
  geom_segment(lineend = 'round', colour='white') + 
  coord_fixed(ratio = 1) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "gray20"))

