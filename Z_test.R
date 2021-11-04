# https://datacornering.com/how-to-calculate-weighted-mean-in-r/

df <- data.frame(
  'cat' = c('A', 'A', 'B', 'B'),
  'var amount' = c(88, 31, 84, 41),
  'var mean time' = c(312, 437, 211, 818)
)
df

df %>%
  group_by(cat) %>%
  summarise(wm_var = weighted.mean(var.mean.time, var.amount)) %>% as.data.frame()



# CODE.....

allTracksFeatures %>% 
  group_by(country, date) %>% 
  summarise(danceability = mean(danceability))

allTracksFeatures %>% 
  group_by(country, date) %>% 
  summarise(danceability = weighted.mean(danceability, nStreams))
