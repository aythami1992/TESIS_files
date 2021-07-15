library(igraph)
library(ggraph)
library(tidyverse)

movimientos <- MovementData_2014

mov_2014 <- movimientos %>% select("o_p","d_p", "IMO","p_cty","Continent_Origin", "d_cty","Continent_Destination")
mov_graph <- graph_from_data_frame(d=mov_2014)
class(mov_graph)
plot(mov_graph)
summary(mov_graph)
as.network(mov_graph)
