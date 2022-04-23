

font_size <-  16

GetKable <-
  function(dfft, let_size){
    dfft %>% 
      mutate(name = str_to_title(name)) %>%
      mutate(name = str_replace_all(name, "_", " ")) %>%
      kbl(col.names = NULL ) %>%
      kable_paper("hover", full_width = F) %>%
      column_spec(column = 1, width = "1in") %>%
      # column_spec(column = 2, width = "2.5in") %>% 
      kable_styling( "striped", position = "left", font_size = let_size) 
  }






































