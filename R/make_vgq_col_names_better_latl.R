#' Uniformizes the names of the columns in a dataset from the VGQ
#'
#' Converts the column names from the LATL VGQ Qualtrics questionnaire to a
#' uniformized model that follows this template : <vg category>_<time period>_<measure>.
#'
#' <vg catgory> can take the values "fps", "arpg", "sports", "rts", "tbrpg", "music", "tbs", or "other"
#' <time period> can take the values "past.year" or "before.past.year"
#' <measure> can take the values "hours", "small.screen", or "touch.screen"
#'
#' @param raw.df Data frame with at least one column named "participant_id" with the unique
#'  IDs for each participant and the columns from the VGQ with the names
#'  following the pattern from the Qualtrics questionnaire from the Mini-copter
#'  study (from LATL), see here : https://uwmadison.co1.qualtrics.com/survey-builder/SV_7UGlBut4jMHOb4O/edit
#'
#' @return A data frame with the columns renamed
#'
#' @examples
#' scored.df <- make_vgq_col_names_better_latl(raw.df)
#'
#' @keywords internal
#' @export

make_vgq_col_names_better_latl <- function(raw.df) {
  col_names <- tolower(names(raw.df))
  col_names <- case_when(
    col_names == "vg_shooters_freq_1" ~ "fps_past.year_hours",
    col_names == "vg_shooters_screen_1" ~ "fps_past.year_small.screen",
    col_names == "vg_shooters_screen_2" ~ "fps_past.year_touch.screen",
    col_names == "vg_action_freq_1" ~ "arpg_past.year_hours",
    col_names == "vg_action_screen_1" ~ "arpg_past.year_small.screen",
    col_names == "vg_action_screen_2" ~ "arpg_past.year_touch.screen",
    col_names == "vg_sports_freq_1" ~ "sports_past.year_hours",
    col_names == "vg_sports_screen_1" ~ "sports_past.year_small.screen",
    col_names == "vg_sports_screen_2" ~ "sports_past.year_touch.screen",
    col_names == "vg_rtstrat_freq_1" ~ "rts_past.year_hours",
    col_names == "vg_rtstrat_screen_1" ~ "rts_past.year_small.screen",
    col_names == "vg_rtstrat_screen_2" ~ "rts_past.year_touch.screen",
    col_names == "vg_fantasy_freq_1" ~ "tbrpg_past.year_hours",
    col_names == "vg_fantasy_screen_1" ~ "tbrpg_past.year_small.screen",
    col_names == "vg_fantasy_screen_2" ~ "tbrpg_past.year_touch.screen",
    col_names == "vg_lifesim_freq_1" ~ "tbs_past.year_hours",
    col_names == "vg_lifesim_screen_1" ~ "tbs_past.year_small.screen",
    col_names == "vg_lifesim_screen_2" ~ "tbs_past.year_touch.screen",
    col_names == "vg_music_freq_1" ~ "music_past.year_hours",
    col_names == "vg_music_screen_1" ~ "music_past.year_small.screen",
    col_names == "vg_music_screen_2" ~ "music_past.year_touch.screen",
    col_names == "vg_other_freq_1" ~ "other_past.year_hours",
    col_names == "vg_other_screen_1" ~ "other_past.year_small.screen",
    col_names == "vg_other_screen_2" ~ "other_past.year_touch.screen",
    col_names == "vg_shooters_freq_p_1" ~ "fps_before.past.year_hours",
    col_names == "vg_shooters_screen_p_1" ~ "fps_before.past.year_small.screen",
    col_names == "vg_shooters_screen_p_2" ~ "fps_before.past.year_touch.screen",
    col_names == "vg_action_freq_p_1" ~ "arpg_before.past.year_hours",
    col_names == "vg_action_screen_p_1" ~ "arpg_before.past.year_small.screen",
    col_names == "vg_action_screen_p_2" ~ "arpg_before.past.year_touch.screen",
    col_names == "vg_sports_freq_p_1" ~ "sports_before.past.year_hours",
    col_names == "vg_sports_screen_p_1" ~ "sports_before.past.year_small.screen",
    col_names == "vg_sports_screen_p_2" ~ "sports_before.past.year_touch.screen",
    col_names == "vg_rtstrat_freq_p_1" ~ "rts_before.past.year_hours",
    col_names == "vg_rtstrat_screen_p_1" ~ "rts_before.past.year_small.screen",
    col_names == "vg_rtstrat_screen_p_2" ~ "rts_before.past.year_touch.screen",
    col_names == "vg_fantasy_freq_p_1" ~ "tbrpg_before.past.year_hours",
    col_names == "vg_fantasy_screen_p_1" ~ "tbrpg_before.past.year_small.screen",
    col_names == "vg_fantasy_screen_p_2" ~ "tbrpg_before.past.year_touch.screen",
    col_names == "vg_lifesim_freq_p_1" ~ "tbs_before.past.year_hours",
    col_names == "vg_lifesim_screen_p_1" ~ "tbs_before.past.year_small.screen",
    col_names == "vg_lifesim_screen_p_2" ~ "tbs_before.past.year_touch.screen",
    col_names == "vg_music_freq_p_1" ~ "music_before.past.year_hours",
    col_names == "vg_music_screen_p_1" ~ "music_before.past.year_small.screen",
    col_names == "vg_music_screen_p_2" ~ "music_before.past.year_touch.screen",
    col_names == "vg_other_freq_p_1" ~ "other_before.past.year_hours",
    col_names == "vg_other_screen_p_1" ~ "other_before.past.year_small.screen",
    col_names == "vg_other_screen_p_2" ~ "other_before.past.year_touch.screen",
    .default = col_names
  )
  names(raw.df) <- col_names

  return(raw.df)
}
