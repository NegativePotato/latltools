#' Returns Video Game Player Category
#'
#' Given a data frame where each row is a participant and with the answers to the
#' VGQ in the columns, this functions returns the category of each participant in
#' terms of the VGQ (https://www.unige.ch/fapse/brainlearning/vgq/), 2022 version.
#'
#' The output data frame has columned named following the template : <vg category>_<time period>_<measure>.
#'
#' <vg catgory> can take the values "fps", "arpg", "sports", "rts", "tbrpg", "music", "tbs", or "other"
#' <time period> can take the values "past.year" or "before.past.year"
#' <measure> can take the values "hours", "small.screen", or "touch.screen"
#'
#'  Requires : pracma, dplyr
#'
#' @param raw.df Data frame with at least one column named "participant_id" with the unique
#'  IDs for each participant and the columns from the VGQ with the names
#'  following the pattern from the Qualtrics questionnaire from the Mini-copter
#'  study (from LATL), see here : https://uwmadison.co1.qualtrics.com/survey-builder/SV_7UGlBut4jMHOb4O/edit
#' @param raw_values_qualtrics (Default \code{TRUE})Logical variable indicating whether the scores need
#'  to be adjusted by adding one to values as the scores on Qualtrics start at 0 instead of 1
#'  (use \code{TRUE} if you have not corrected the values).
#'
#' @return A data frame with the player cateogry in the column "player_category" and the item columns renamed according to the mapping above.
#'
#' @examples
#' scored.df <- get_vgq_scores_latl(example_data.df, raw_values_qualtrics = T)
#'
#' @export

get_vgq_scores_latl <- function(raw.df, raw_values_qualtrics = T) {
  # Input :
  # Data frame with at least one column named "participant_id" with the unique
  # IDs for each participant and the columns from the VGQ with the names
  # following the pattern from the Qualtrics questionnaire from the Mini-copter
  # study (from LATL), see here :
  # https://uwmadison.co1.qualtrics.com/survey-builder/SV_7UGlBut4jMHOb4O/edit
  #
  #
  # Example use :
  # scored.df <- get_vgq_scores(raw.df = raw.df)

  if(!"participant_id" %in% names(raw.df)) {
    warning('No participant_id column found. Default ID column added to the dataframe.')
    raw.df <- raw.df %>%
      mutate(participant_id =
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
    select(participant_id, contains("past.year"))

  if(raw_values_qualtrics) {
    vgp_categories.df <- vgp_categories.df %>%
      mutate(across(contains("year_hours"), function(x) {as.numeric(x) - 1 }))
  }

  vgp_categories.df <- vgp_categories.df %>%
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

