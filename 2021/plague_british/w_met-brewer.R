library(MetBrewer)
met_scale <- met.brewer(name="Greek",n=20,type="continuous") %>% as.character()

ggplot() + 
  geom_sf(data = grid_sf, aes(fill = delta),
          color = "white",
          size = .1) +
  scale_fill_gradientn(colours = rev(met_scale)) +
  geom_sf(data = map_sf,
          color = "white",
          size = .01,
          fill = "transparent") +
  labs(title = "The Black Death on the \nBritish Isles",
       subtitle = "Net population loss (k), 1300 - 1400",
       caption = "Graphics: Jeppe Vierø (@Vieroe)\nData: HYDE 3.2") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(color = "white",
                                  size = 18,
                                  family = "Cinzel"),
        plot.subtitle = element_text(color = "white",
                                     size = 10,
                                     family = "Cinzel"),
        plot.caption = element_text(color = "white",
                                    size = 7,
                                    family = "Cinzel",
                                    vjust = -5,
                                    hjust = 0),
        panel.background = element_rect(fill = "gray15",
                                        color = NA),
        plot.background = element_rect(fill = "gray15",
                                       color = NA),
        plot.margin = grid::unit(c(t = 0, r = 20, b = 5, l = 5), "mm")) +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = .5,
                               ticks = FALSE,
                               title = "",
                               frame.colour = "white",
                               label.theme = element_text(color = "white",
                                                          family = "Cinzel",
                                                          size = 7)))