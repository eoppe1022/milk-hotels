
library(tidyverse)
library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = "get your own from spotify's developer page")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "get your own from spotify's developer page")

access_token <- spotifyr::get_spotify_access_token()

mydata <- get_artist_audio_features("neutral milk hotel") %>%
  bind_rows(get_artist_audio_features("the microphones")) %>%
  bind_rows(get_artist_audio_features("the olivia tremor control")) %>%
  filter(tolower(album_name) %in% c("in the aeroplane over the sea", "the glow pt. 2", "black foliage: animation music")) %>%
  select(artist_name, album_name, valence)
  
mydata %>%
  mutate(new_album = str_c(artist_name, album_name, sep = " - ")) %>%
  ggplot(aes(y = valence, x = ordered(album_name, levels = c("The Glow Pt. 2", "In the Aeroplane Over the Sea", "Black Foliage: Animation Music")))) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 4) + 
  guides(color = FALSE) +
  labs(title = "Negative Milk Hotel, Neutral Milk Hotel, and Positive Milk Hotel", 
       subtitle = paste('Valence indicates how positive/negative a song sounds. Higher valence: more positive-sounding.',  'Data from {spotifyr}. Inspiration from vovaantonovich.', sep = "\n"),
       y = "Valence",
       x = "",
       caption = "@OppenheimerEvan") +
  theme_minimal(base_family = "Tw Cen MT Condensed") +
  stat_summary(fun.y = median, fun.ymax = median, fun.ymin = median, mapping = aes(group = 1), geom = "crossbar", width = 0.4) +
  ylim(0, 1) +
  scale_x_discrete(labels = c(paste("The Glow Pt. 2", "The Microphones", sep = "\n"), paste("In the Aeroplane Over the Sea", "Neutral Milk Hotel", sep = "\n"), paste("Black Foliage: Animation Music", "The Olivia Tremor Control", sep = "\n"))) +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = 50, margin = margin(1, 0, 20, 0)),
        axis.title = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "bold", size = 27),
        axis.text.x = element_text(face = "bold", size = 23),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
        plot.margin = margin(25, 15, 25, 15),
        axis.line = element_blank(),
        axis.ticks.length = unit(0.7, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.caption = element_text(size = 19, face = "bold", hjust = 0.95),
        plot.subtitle = element_text(size = 32, face = "bold", margin = margin(0, 0, 30, 0)))

ggsave("milk_hotels.png", dpi = 320, height = (2.72 * 4), width = (4.17 * 5))

