library(DiagrammeR)

# classical deming cicle

# apply transformation factor
k <- 2

d <- create_graph() %>%
  
  ## deming
  
  add_node(label = "PLAN",
           node_aes = node_aes(shape="circle",
                               height = 0.8)) %>%
  
  add_node(label = "DO",
           node_aes = node_aes(shape="circle", height = 0.8)) %>% 
  
  add_node(label = "CHECK",
           node_aes = node_aes(shape="circle", height = 0.8)) %>%
  
  add_node(label = "ACT",
           node_aes = node_aes(shape="circle", height = 0.8)) %>% 
  
  set_node_position(node = "PLAN", x = 0.5*k, y = 1*k, use_labels = TRUE) %>%
  
  set_node_position(node = "DO", x = 1*k, y = 0.5*k, use_labels = TRUE) %>%
  
  set_node_position(node = "CHECK", x = 0.5*k, y = 0*k, use_labels = TRUE) %>%
  
  set_node_position(node = "ACT", x = 0*k, y = 0.5*k, use_labels = TRUE)%>%
  
  add_edge(from="PLAN", to="DO", edge_aes = edge_aes(color ="gray"))%>%
  
  add_edge(from="DO", to="CHECK", edge_aes = edge_aes(color ="gray"))%>%
  
  add_edge(from="CHECK", to="ACT", edge_aes = edge_aes(color ="gray"))%>%
  
  add_edge(from="ACT", to="PLAN", edge_aes = edge_aes(color ="gray"))

#d %>% 
#  render_graph()



dm <- d %>%
  ## modeling 
  add_node(label="modeling", 
           node_aes = node_aes(shape="rectangle",
                               #color="red",
                               #fillcolor = "white",
                               fontcolor = "red",
                               fixedsize=FALSE
                               #, xlabel = "ex ante"
           )
  )%>%
  
  set_node_position(node=5, x=1.05*k, y=1.3*k) %>% 
  
  add_edge(from=5, to=1, edge_aes = edge_aes(color ="red", 
                                             style="dashed"
                                             #, label = "  ex ante"
  )
  )%>%
  
  add_edge(from=1, to=5, edge_aes = edge_aes(color ="red",
                                             style="dashed"
                                             #, label = "ex ante"
  )
  )%>%
  
  add_node(label="modeling", 
           node_aes = node_aes(shape="rectangle",
                               #color="red",
                               #fillcolor = "white",
                               fontcolor = "red",
                               fixedsize=FALSE
                               #, xlabel = "ex post"
           )
  )%>%
  
  set_node_position(node=6, x=-0.1*k, y=1*k) %>% 
  
  add_edge(from=4, to=6, edge_aes = edge_aes(color ="red", 
                                             style="dashed"
                                             #, label = "  ex post"
  )
  )%>%
  
  add_edge(from=6, to=1, edge_aes = edge_aes(color ="red", 
                                             style="dashed"
                                             #, label = "ex post"
  )
  )%>%
  
  # ## labels beside deming circles
  
  add_node(label = "fase\nistruttoria",
           node_aes = node_aes(shape="plaintext",
                               fixedsize = FALSE,
                               fillcolor = "white",
                               color = "white",
                               fontsize = 8
           )
  ) %>%
  
  set_node_position(node = 7, x = 0.5*k, y = 1*k+0.8) %>%
  
  add_node(label = "monitoraggio\nattività",
           node_aes = node_aes(shape="plaintext",
                               fixedsize = FALSE,
                               fillcolor = "white",
                               color = "white",
                               fontsize = 8
           )
  ) %>%
  
  set_node_position(node = 8, x = 1*k+0.9, y = 0.5*k) %>%
  
  add_node(label = "valutazione\nrisultati",
           node_aes = node_aes(shape="plaintext",
                               fixedsize = FALSE,
                               fillcolor = "white",
                               color = "white",
                               fontsize = 8
           )
  ) %>%
  
  set_node_position(node = 9, x = 0.5*k, y = 0*k-0.8) %>%
  
  add_node(label = "messa\na regime",
           node_aes = node_aes(shape="plaintext",
                               fixedsize = FALSE,
                               fillcolor = "white",
                               color = "white",
                               fontsize = 8
           )
  ) %>%
  
  set_node_position(node = 10, x = 0*k-0.9, y = 0.5*k,) %>%
  
  add_node(label="modeling", 
           node_aes = node_aes(shape="rectangle",
                               #color="red",
                               #fillcolor = "white",
                               fontcolor = "red",
                               fixedsize=FALSE
                               #, xlabel = "xxxxx"
           ))%>%
  
  set_node_position(node=11, x=1.1*k, y=0*k) %>% 
  
  add_edge(from=2, to=11, edge_aes = edge_aes(color ="red", 
                                              style="dashed"
                                              #, label = "  ex ante"
  )
  )%>%
  
  add_edge(from=11, to=3, edge_aes = edge_aes(color ="red", 
                                              style="dashed"
                                              #, label = "ex ante"
  )
  )


#dm %>%
#  render_graph()
