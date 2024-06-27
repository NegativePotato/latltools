library(pracma)
library(readxl)
library(dplyr)

# Data from Unige Excel File
unige.df <- read_excel('/Users/freya/My Drive/Post-Doc ShawnLab/Gens/Ezgi/VGQ/videogames_scored.xlsx', skip = 2)
ezgi.df <- read_excel('/Users/freya/My Drive/Post-Doc ShawnLab/Gens/Ezgi/VGQ/video_game_scores.xlsx')

# Raw data from Qualtrics
raw.df <- read.csv('/Users/freya/My Drive/Post-Doc ShawnLab/Gens/Ezgi/VGQ/videogame_data.csv') %>%
  mutate(Participant_ID = gsub('^ ', '', sub_id)) %>%
  filter(Participant_ID %in% ezgi.df$sub_id)
scored.df <- get_vgq_scores(raw.df = raw.df) %>%
  mutate(player_category_unige = unige.df$`group(2022)`,
         same_category =
           ifelse(!is.na(player_category_unige) &
                    (player_category_unige == player_category),
                  'OK',
                  ifelse(is.na(player_category_unige),
                         'OK', 'NOT OK'))) %>%
  relocate(same_category, .after = Participant_ID) %>%
  relocate(player_category_unige, .after = Participant_ID) %>%
  relocate(player_category, .after = Participant_ID)
write.csv(x = scored.df, file = '/Users/freya/My Drive/Post-Doc ShawnLab/Gens/Ezgi/VGQ/videogame_data_scored_freya.csv')
