library(pracma)
library(readxl)
library(dplyr)

###### FUNCTIONS ######
get_vgq_scores_latl <- function(raw.df) {
  # Input : 
  # Data frame with at least one column named "Participant_ID" with the unique 
  # IDs for each participant and the columns from the VGQ with the names 
  # following the pattern from the Qualtrics questionnaire from the Mini-copter 
  # study (from LATL), see here : 
  # https://uwmadison.co1.qualtrics.com/survey-builder/SV_7UGlBut4jMHOb4O/edit
  # 
  # 
  # Example use : 
  # scored.df <- get_vgq_scores(raw.df = raw.df)
  
  if(!"Participant_ID" %in% names(raw.df)) {
    warning('No Participant_ID column found. Default ID column added to the dataframe.')
    raw.df <- raw.df %>% 
      mutate(Participant_ID = 
               sprintf(sprintf('ID_%%0%ii', floor(log10(n())) + 1),row_number()))
  }
  raw.df <- make_vgq_col_names_better_latl(raw.df)
  
  # Question hours : 1 / 0 -> Never, {first number is in raw data, second one is recoded}
  #                   2 / 1 -> 0-1, 
  #                   3 / 2 -> 1-3, 
  #                   4 / 3 -> 3-5, 
  #                   5 / 4 -> 5-10,
  #                   6 / 5 -> 10+
  # Question small screen : 1 -> Yes, 2 -> No
  # Question touch screen : 1 -> Yes, 2 -> No
  vgp_categories.df <- raw.df %>% 
    select(Participant_ID, contains("past.year")) %>% 
    mutate(across(contains("year_hours"), function(x) {as.numeric(x) - 1 })) %>% 
    mutate(
      all.genre_past.year_hours =
        fps_past.year_hours + 
        arpg_past.year_hours + 
        sports_past.year_hours + 
        rts_past.year_hours +
        tbrpg_past.year_hours +
        music_past.year_hours +
        tbs_past.year_hours +
        other_past.year_hours,
      all.genre_before.past.year_hours = 
        fps_before.past.year_hours + 
        arpg_before.past.year_hours + 
        sports_before.past.year_hours + 
        rts_before.past.year_hours +
        tbrpg_before.past.year_hours +
        music_before.past.year_hours +
        tbs_before.past.year_hours +
        other_before.past.year_hours) %>% 
    mutate(player_category = 
             ifelse(fps_past.year_hours <= 1 &
                      arpg_past.year_hours <= 1 &
                      sports_past.year_hours <= 1 &
                      rts_past.year_hours <= 1 & 
                      tbrpg_past.year_hours <= 2 &
                      music_past.year_hours <= 2 &
                      tbs_past.year_hours <= 2 & 
                      other_past.year_hours <= 2 & 
                      all.genre_past.year_hours < 5 & 
                      fps_before.past.year_hours <= 1 &
                      arpg_before.past.year_hours <= 1 &
                      sports_before.past.year_hours <= 1 &
                      rts_before.past.year_hours <= 1 & 
                      tbrpg_before.past.year_hours <= 2 &
                      music_before.past.year_hours <= 2 &
                      tbs_before.past.year_hours <= 2 & 
                      other_before.past.year_hours <= 2 & 
                      all.genre_before.past.year_hours <= 5, 
                    "NVGP", 
                    ifelse((((fps_past.year_hours >= 4 & 
                                fps_past.year_small.screen == 2 & 
                                fps_past.year_touch.screen == 2) | 
                               (arpg_past.year_hours >= 4 & 
                                  arpg_past.year_small.screen == 2 & 
                                  arpg_past.year_touch.screen == 2)) & 
                              tbrpg_before.past.year_hours <= 2 &
                              music_before.past.year_hours <= 2 &
                              tbs_before.past.year_hours <= 2 & 
                              other_before.past.year_hours <= 2) |
                             (((fps_past.year_hours >= 3 & 
                                  fps_past.year_small.screen == 2 & 
                                  fps_past.year_touch.screen == 2) | 
                                 (arpg_past.year_hours >= 3 & 
                                    arpg_past.year_small.screen == 2 & 
                                    arpg_past.year_touch.screen == 2)) & 
                                (((fps_before.past.year_hours >= 4 & 
                                     fps_before.past.year_small.screen == 2 & 
                                     fps_before.past.year_touch.screen == 2) | 
                                    (arpg_before.past.year_hours >= 4 & 
                                       arpg_before.past.year_small.screen == 2 & 
                                       arpg_before.past.year_touch.screen == 2)) |
                                   (sports_past.year_hours >= 4 & 
                                      sports_past.year_small.screen == 2 & 
                                      sports_past.year_touch.screen == 2) | 
                                   (rts_past.year_hours >= 4 & 
                                      rts_past.year_small.screen == 2 & 
                                      rts_past.year_touch.screen == 2)) & 
                                tbrpg_past.year_hours <= 2 &
                                music_past.year_hours <= 2 &
                                tbs_past.year_hours <= 2 & 
                                other_past.year_hours <= 2), 
                           "AVGP", 
                           ifelse((fps_past.year_hours <= 1 &
                                     arpg_past.year_hours <= 1 &
                                     ((sports_past.year_hours <= 2 & rts_past.year_hours <= 1) | 
                                        (sports_past.year_hours <= 1 & rts_past.year_hours <= 2)) &
                                     fps_before.past.year_hours <= 3 &
                                     arpg_before.past.year_hours <= 3 &
                                     sports_before.past.year_hours <= 3 &
                                     rts_before.past.year_hours <= 3 & 
                                     ((all.genre_past.year_hours >= 2 & all.genre_past.year_hours <= 4) |
                                        (all.genre_past.year_hours <= 1 & all.genre_before.past.year_hours >= 4)) &
                                     tbrpg_past.year_hours <= 3 &
                                     music_past.year_hours <= 3 &
                                     tbs_past.year_hours <= 3 & 
                                     other_past.year_hours <= 3),
                                  "Low-Tweener", "Tweener"))))
  
  return(vgp_categories.df)
}

make_vgq_col_names_better_latl <- function(raw.df) {
  col_names <- names(raw.df)
  col_names <- case_when(
    col_names == "VG_Shooters_Freq_1" ~ "fps_past.year_hours",
    col_names == "VG_Shooters_Screen_1" ~ "fps_past.year_small.screen",
    col_names == "VG_Shooters_Screen_2" ~ "fps_past.year_touch.screen",
    col_names == "VG_Action_Freq_1" ~ "arpg_past.year_hours",
    col_names == "VG_Action_Screen_1" ~ "arpg_past.year_small.screen",
    col_names == "VG_Action_Screen_2" ~ "arpg_past.year_touch.screen",
    col_names == "VG_Sports_Freq_1" ~ "sports_past.year_hours",
    col_names == "VG_Sports_Screen_1" ~ "sports_past.year_small.screen",
    col_names == "VG_Sports_Screen_2" ~ "sports_past.year_touch.screen",
    col_names == "VG_RTStrat_Freq_1" ~ "rts_past.year_hours",
    col_names == "VG_RTStrat_Screen_1" ~ "rts_past.year_small.screen",
    col_names == "VG_RTStrat_Screen_2" ~ "rts_past.year_touch.screen",
    col_names == "VG_Fantasy_Freq_1" ~ "tbrpg_past.year_hours",
    col_names == "VG_Fantasy_Screen_1" ~ "tbrpg_past.year_small.screen",
    col_names == "VG_Fantasy_Screen_2" ~ "tbrpg_past.year_touch.screen",
    col_names == "VG_LifeSim_Freq_1" ~ "tbs_past.year_hours",
    col_names == "VG_LifeSim_Screen_1" ~ "tbs_past.year_small.screen",
    col_names == "VG_LifeSim_Screen_2" ~ "tbs_past.year_touch.screen",
    col_names == "VG_Music_Freq_1" ~ "music_past.year_hours",
    col_names == "VG_Music_Screen_1" ~ "music_past.year_small.screen",
    col_names == "VG_Music_Screen_2" ~ "music_past.year_touch.screen",
    col_names == "VG_Other_Freq_1" ~ "other_past.year_hours",
    col_names == "VG_Other_Screen_1" ~ "other_past.year_small.screen",
    col_names == "VG_Other_Screen_2" ~ "other_past.year_touch.screen",
    col_names == "VG_Shooters_Freq_P_1" ~ "fps_before.past.year_hours",
    col_names == "VG_Shooters_Screen_P_1" ~ "fps_before.past.year_small.screen",
    col_names == "VG_Shooters_Screen_P_2" ~ "fps_before.past.year_touch.screen",
    col_names == "VG_Action_Freq_P_1" ~ "arpg_before.past.year_hours",
    col_names == "VG_Action_Screen_P_1" ~ "arpg_before.past.year_small.screen",
    col_names == "VG_Action_Screen_P_2" ~ "arpg_before.past.year_touch.screen",
    col_names == "VG_Sports_Freq_P_1" ~ "sports_before.past.year_hours",
    col_names == "VG_Sports_Screen_P_1" ~ "sports_before.past.year_small.screen",
    col_names == "VG_Sports_Screen_P_2" ~ "sports_before.past.year_touch.screen",
    col_names == "VG_RTStrat_Freq_P_1" ~ "rts_before.past.year_hours",
    col_names == "VG_RTStrat_Screen_P_1" ~ "rts_before.past.year_small.screen",
    col_names == "VG_RTStrat_Screen_P_2" ~ "rts_before.past.year_touch.screen",
    col_names == "VG_Fantasy_Freq_P_1" ~ "tbrpg_before.past.year_hours",
    col_names == "VG_Fantasy_Screen_P_1" ~ "tbrpg_before.past.year_small.screen",
    col_names == "VG_Fantasy_Screen_P_2" ~ "tbrpg_before.past.year_touch.screen",
    col_names == "VG_LifeSim_Freq_P_1" ~ "tbs_before.past.year_hours",
    col_names == "VG_LifeSim_Screen_P_1" ~ "tbs_before.past.year_small.screen",
    col_names == "VG_LifeSim_Screen_P_2" ~ "tbs_before.past.year_touch.screen",
    col_names == "VG_Music_Freq_P_1" ~ "music_before.past.year_hours",
    col_names == "VG_Music_Screen_P_1" ~ "music_before.past.year_small.screen",
    col_names == "VG_Music_Screen_P_2" ~ "music_before.past.year_touch.screen",
    col_names == "VG_Other_Freq_P_1" ~ "other_before.past.year_hours",
    col_names == "VG_Other_Screen_P_1" ~ "other_before.past.year_small.screen",
    col_names == "VG_Other_Screen_P_2" ~ "other_before.past.year_touch.screen",  
    .default = col_names
  )
  names(raw.df) <- col_names
  
  return(raw.df)
}

####### TRY DATA ######
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
