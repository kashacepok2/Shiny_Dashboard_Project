server <- function(input, output, session) {
  output$map_output_bed <- renderPlot(expr = {
    
    scotland_hb_xform %>%
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(label = ~hb_name,
                  fillColor = getcolour(beds_clean$percentage), 
                  opacity = 0.8)
  }
  )
  output$map_output_ae <- renderPlot(expr = {
    
    scotland_hb_xform %>%
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(label = ~hb_name,
                  fillColor = getcolour(ae_times_clean$percentage), 
                  opacity = 0.8)
  }
  )
  output$graph_output_bed <- renderPlot(expr = {
    beds_clean %>% 
      group_by(hb_name, date) %>%
      summarise(Percentage = mean(percentage)) %>% 
      ggplot() +
      aes(x = date, y = Percentage, colour = hb_name) +
      geom_line() +
      scale_colour_manual(values = c("yellow1", "darkorchid4", "burlywood3", "firebrick4", "forestgreen", "hotpink", "darkorange3",  "palegreen2", "cadetblue2", "orchid3", "black", "navy", "antiquewhite4", "sienna2")) +
      theme_classic()
  }
  )
  output$graph_output_ae <- renderPlot(expr = {
    ae_times_clean %>% 
      group_by(hb_name, date) %>%
      summarise(Percentage = mean(percentage)) %>% 
      ggplot() +
      aes(x = date, y = Percentage, colour = hb_name) +
      geom_line() +
      scale_colour_manual(values = c("yellow1", "darkorchid4", "burlywood3", "firebrick4", "forestgreen", "hotpink", "darkorange3",  "palegreen2", "cadetblue2", "orchid3", "black", "navy", "antiquewhite4", "sienna2")) +
      theme_classic()
  }
  )
}
