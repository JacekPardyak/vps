# R -e "shiny::runApp('./mwe')"
# https://www.linode.com/docs/guides/how-to-deploy-rshiny-server-on-ubuntu-and-debian/
# https://linuxize.com/post/how-to-install-r-on-debian-9/
# https://cran.r-project.org/bin/linux/debian/
library(tidyverse)

df <- read_csv(I(
               "id, name, surname
                 1, Jacek, Popek
                 2, Tomek, Kinski
                 3, Szymek, Nowek
                 4, Jacek, Luby")) %>% mutate(name = factor(name))
df %>% glimpse()

names <- df %>% distinct(name) %>% pull() %>% as.list()

sub_names = c('Jacek', 'Szymek')

df_sub <- df %>% filter(name %in% sub_names) 

df_sub %>% glimpse()
