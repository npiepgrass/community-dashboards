
j <- counties(21)
jeffNLCD <- get_nlcd(as_Spatial(j[j$COUNTYFP==211,]), label = "jc", year = 2016,
                     force.redo = F)




interp <- function(x){
  x
}





get_developed <- function(extraction){
  map(extraction, function(x){
    length(which(x==21|x==22|x==23))
  })
}


map2(intersect,map(extract_ints, get_developed),
     function(x,y){
       x@data$partial<-unlist(y)
       x@data<-x@data %>% 
         left_join(tracts_dev) %>% 
         mutate(ratio=partial/developed)
       x@data
     }) %>% 
  reduce(bind_rows) %>% 
  tibble()












