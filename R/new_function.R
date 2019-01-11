#' Matches the current get_play_data functionality for setting binary indicators.
#' all functions and variables that are created have the prefix new_
#'
#' @importFrom dplyr %>%
#' @import nnet
#'
#' @param new_play list containing data for the play. This is the same as the play parameter currently taken by get_play_data
#'
#' @export
#'
new_function <- function(new_play) {
  # Start by defining the lookup structures that will be used for processing the play
  # This includes the complete mapping of all stats as well as the sets of stats that define
  # the various binary indicators
  # new_stat_map is pretty much nfl_stat_map using vectors.
  new_stat_map <- c(
    2L:16L,
    19L:64L,
    68L:80L,
    82L:89L,
    91L,
    93L,
    95L:96L,
    99L,
    100L,
    102L:108L,
    110L:113L,
    115L,
    120L,
    301L,
    402L:406L,
    410L,
    420L
  )
  names(new_stat_map) <- c(
    "punt_blocked",
    "first_down_rush",
    "first_down_pass",
    "first_down_penalty",
    "third_down_converted",
    "third_down_failed",
    "fourth_down_converted",
    "fourth_down_failed",
    "rushing_yards",
    "rushing_yards_td",
    "lateral_rushing_yards",
    "lateral_rushing_yards_td",
    "incomplete_pass",
    "passing_yards",
    "passing_yards_td",
    "interception",
    "sack_yards",
    "receiving_yards",
    "receiving_yards_td",
    "lateral_receiving_yards",
    "lateral_receiving_yards_td",
    "interception_return_yards",
    "interception_return_yards_td",
    "lateral_interception_return_yards",
    "lateral_interception_return_yards_td",
    "punting_yards",
    "punt_inside_twenty",
    "punt_in_endzone",
    "punt_touchback_kicking",
    "punt_return_yards",
    "punt_return_yards_td",
    "lateral_punt_return_yards",
    "lateral_punt_return_yards_td",
    "punt_out_of_bounds",
    "punt_downed",
    "punt_fair_catch",
    "punt_touchback_receiving",
    "kickoff_yards",
    "kickoff_inside_twenty",
    "kickoff_in_endzone",
    "kickoff_touchback_kicking",
    "kickoff_return_yards",
    "kickoff_return_yards_td",
    "lateral_kickoff_return_yards",
    "lateral_kickoff_return_yards_td",
    "kickoff_out_of_bounds",
    "kickoff_fair_catch",
    "kickoff_touchback_receiving",
    "fumble_forced",
    "fumble_not_forced",
    "fumble_out_of_bounds",
    "own_fumble_recovery_yards",
    "own_fumble_recovery_yards_td",
    "lateral_own_fumble_recovery_yards",
    "lateral_own_fumble_recovery_yards_td",
    "opp_fumble_recovery_yards",
    "opp_fumble_recovery_yards_td",
    "lateral_opp_fumble_recovery_yards",
    "lateral_opp_fumble_recovery_yards_td",
    "miscellaneous_yards",
    "miscellaneous_yards_td",
    "timeout",
    "field_goal_yards_missed",
    "field_goal_yards_made",
    "field_goal_yards_blocked",
    "extra_point_good",
    "extra_point_failed",
    "extra_point_blocked",
    "two_point_rush_good",
    "two_point_rush_failed",
    "two_point_pass_good",
    "two_point_pass_failed",
    "solo_tackle",
    "assisted_tackle",
    "tackle_assist",
    "solo_sack_yards",
    "assist_sack_yards",
    "pass_defense_player",
    "punt_blocked_player",
    "extra_point_blocked_player",
    "field_goal_blocked_player",
    "safety_tackle",
    "forced_fumble_player",
    "penalty_yards",
    "tackled_for_loss",
    "extra_point_safety",
    "two_point_rush_safety",
    "two_point_pass_safety",
    "kickoff_downed",
    "lateral_sack_yards",
    "two_point_pass_reception_good",
    "two_point_pass_reception_failed",
    "fumble_lost",
    "own_kickoff_recovery",
    "own_kickoff_recovery_td",
    "qb_hit",
    "air_yards_complete",
    "air_yards_incomplete",
    "yards_after_catch",
    "targeted_receiver",
    "tackle_for_loss_player",
    "extra_point_aborted",
    "tackle_for_loss_yards",
    "defensive_two_point_attempt",
    "defensive_two_point_conv",
    "defensive_extra_point_attempt",
    "defensive_extra_point_conv",
    "kickoff_yard_length",
    "two_point_return"
  )
  new_indicator_stats <- new_stat_map[c(
    "punt_blocked", "first_down_rush",
    "first_down_pass", "first_down_penalty",
    "third_down_converted", "third_down_failed",
    "fourth_down_converted", "fourth_down_failed",
    "incomplete_pass", "interception",
    "punt_inside_twenty", "punt_in_endzone",
    "punt_out_of_bounds", "punt_downed",
    "punt_fair_catch", "kickoff_inside_twenty",
    "kickoff_in_endzone", "kickoff_out_of_bounds",
    "kickoff_fair_catch", "fumble_forced",
    "fumble_not_forced", "fumble_out_of_bounds",
    "timeout", "field_goal_yards_missed",
    "field_goal_yards_made",
    "field_goal_yards_blocked",
    "extra_point_good", "extra_point_failed",
    "extra_point_blocked", "two_point_rush_good",
    "two_point_rush_failed",
    "two_point_pass_good", "two_point_pass_failed",
    "solo_tackle", "safety_tackle", "penalty_yards",
    "tackled_for_loss", "extra_point_safety",
    "two_point_rush_safety", "two_point_pass_safety",
    "kickoff_downed", "two_point_pass_reception_good",
    "two_point_pass_reception_failed",
    "fumble_lost", "own_kickoff_recovery",
    "own_kickoff_recovery_td", "qb_hit", "extra_point_aborted",
    "two_point_return", "defensive_two_point_attempt",
    "defensive_two_point_conv",
    "defensive_extra_point_attempt",
    "defensive_extra_point_conv"
  )]

  # this returns a set of all the stats that include stat_phrase as part of their name
  new_find_stats_with_name <- function(new_stat_phrase) {
    return(new_stat_map[stringr::str_detect(names(new_stat_map), new_stat_phrase)])
  }

  new_pass_attempt_indicator_stats <- new_stat_map[c(
    "passing_yards",
    "passing_yards_td",
    "incomplete_pass",
    "interception",
    "sack_yards",
    "receiving_yards",
    "receiving_yards_td",
    "lateral_receiving_yards",
    "lateral_receiving_yards_td",
    "interception_return_yards",
    "interception_return_yards_td",
    "lateral_interception_return_yards",
    "lateral_interception_return_yards_td",
    "air_yards_complete",
    "air_yards_incomplete",
    "yards_after_catch",
    "targeted_receiver",
    "two_point_pass_good",
    "two_point_pass_failed",
    "two_point_pass_safety",
    "two_point_pass_reception_good",
    "two_point_pass_reception_failed"
  )]

  new_rush_attempt_indicator_stats <- new_stat_map[c(
    "rushing_yards",
    "rushing_yards_td",
    "lateral_rushing_yards",
    "lateral_rushing_yards_td",
    "two_point_rush_good",
    "two_point_rush_failed",
    "two_point_rush_safety"
  )]

  new_sack_indicator_stats <- new_stat_map[c(
    "sack_yards", "solo_sack_yards",
    "assist_sack_yards"
  )]

  new_touchdown_indicator_stats <- new_stat_map[c(
    "rushing_yards_td",
    "lateral_rushing_yards_td",
    "passing_yards_td",
    "receiving_yards_td",
    "lateral_receiving_yards_td",
    "interception_return_yards_td",
    "lateral_interception_return_yards_td",
    "kickoff_return_yards_td",
    "lateral_kickoff_return_yards_td",
    "own_fumble_recovery_yards_td",
    "lateral_own_fumble_recovery_yards_td",
    "opp_fumble_recovery_yards_td",
    "lateral_opp_fumble_recovery_yards_td",
    "miscellaneous_yards_td",
    "own_kickoff_recovery_td",
    "punt_return_yards_td",
    "lateral_punt_return_yards_td"
  )]

  new_pass_touchdown_indicator_stats <- new_stat_map[c(
    "passing_yards_td",
    "receiving_yards_td",
    "lateral_receiving_yards_td"
  )]

  new_rush_touchdown_indicator_stats <- new_stat_map[c(
    "rushing_yards_td",
    "lateral_rushing_yards_td"
  )]
  # interception returns are considered a return td, but fumble recoveries are not.
  # is this the intended logic?
  new_return_touchdown_indicator_stats <- new_stat_map[c(
    "interception_return_yards_td",
    "lateral_interception_return_yards_td",
    "kickoff_return_yards_td",
    "lateral_kickoff_return_yards_td",
    "punt_return_yards_td",
    "lateral_punt_return_yards_td"
  )]

  new_extra_point_attempt_indicator_stats <- new_stat_map[c(
    "extra_point_good",
    "extra_point_failed",
    "extra_point_blocked",
    "extra_point_safety",
    "extra_point_aborted"
  )]

  new_two_point_attempt_indicator_stats <- new_stat_map[c(
    "two_point_rush_good",
    "two_point_rush_failed",
    "two_point_pass_good",
    "two_point_pass_failed",
    "two_point_rush_safety",
    "two_point_pass_safety",
    "two_point_pass_reception_good",
    "two_point_pass_reception_failed",
    "two_point_return"
  )]

  new_field_goal_attempt_indicator_stats <- new_find_stats_with_name("field_goal")
  new_kickoff_attempt_indicator_stats <- new_find_stats_with_name("kickoff")
  new_punt_attempt_indicator_stats <- new_find_stats_with_name("punt")
  new_fumble_indicator_stats <- new_find_stats_with_name("fumble")
  new_complete_pass_indicator_stats <- new_stat_map[c(
    "passing_yards",
    "passing_yards_td",
    "receiving_yards",
    "receiving_yards_td",
    "lateral_receiving_yards",
    "lateral_receiving_yards_td",
    "air_yards_complete",
    "yards_after_catch"
  )]

  new_lateral_reception_indicator_stats <- new_stat_map[c(
    "lateral_receiving_yards",
    "lateral_receiving_yards_td"
  )]

  new_lateral_rush_indicator_stats <- new_stat_map[c(
    "lateral_rushing_yards",
    "lateral_rushing_yards_td"
  )]

  new_lateral_return_indicator_stats <- new_stat_map[c(
    "lateral_interception_return_yards",
    "lateral_interception_return_yards_td",
    "lateral_punt_return_yards",
    "lateral_punt_return_yards_td",
    "lateral_kickoff_return_yards",
    "lateral_kickoff_return_yards_td"
  )]

  new_lateral_recovery_indicator_stats <- new_stat_map[c(
    "lateral_own_fumble_recovery_yards",
    "lateral_own_fumble_recovery_yards_td",
    "lateral_opp_fumble_recovery_yards",
    "lateral_opp_fumble_recovery_yards_td"
  )]

  new_assist_tackle_indicator_stats <- new_stat_map[c(
    "assisted_tackle",
    "tackle_assist",
    "assist_sack_yards"
  )]




  # Now that the lookup structures have been defined,
  # information from the play will be extracted
  # and converted into a more convenient structure
  # This is currently achieved by the play_player_data data frame
  # I create two additional matrices that contain the same relevent
  # data
  # play_player_data will be created as well, since it is needed later on by nflscrapR
  # this whole process should actually speed up the creation of play_player_data
  tictoc::tic(msg = "creating_new_matrices_and_new_play_player_data")
  tictoc::tic(msg = "creating_new_matrices")
  new_players <- new_play$players
  # this probably isn't needed, but it is created by the current implementation
  new_player_ids <- names(new_players)
  new_play_length <- 0
  for (new_player in new_players)
  {
    new_play_length <- new_play_length + length(new_player)
  }
  # matrix with two rows, one for the stat_id and one for the yards the row number corresponds to the sequence in which the stats occured
  # I have added a third row, for the sequence, to make creating the play_player_data table easier
  # I don't think this is necessary, since all lookups can be done using the row number, and the row number is the sequence
  new_play_stat_matrix <-
    matrix(numeric(),
      nrow = new_play_length,
      ncol = 3,
      dimnames = list(NULL, c("statId", "yards", "sequence"))
    )
  # matrix with three rows, identifying the player associated with each stat, ordered in the sequence in which the stats occured
  # I have added a fourth row, statId, to make creating the play_player_data table easier
  # this goes here only because it is a character value and can't be stored in the numeric play stat matrix,
  # where it logically would go
  # I don't think this is necessary because any time nfl_stat is needed, the statId from
  # the play_stat_matrix can be looked up in the stat_map vector, which should be quicker than a data frame lookup
  new_player_id_matrix <- matrix(character(),
    nrow = new_play_length,
    ncol = 4,
    dimnames = list(NULL, c(
      "player_id", "player_name", "team", "nfl_stat"
    ))
  )
  new_null_to_NA <- function(new_value) {
    ifelse(is.null(new_value), NA, new_value)
  }
  # Go though each player's stats and fill in the matrices using sequence to identify which row to fill in
  for (new_player_number in 1:length(new_players))
  {
    new_player <- new_players[[new_player_number]]
    new_player_id <- names(new_players)[new_player_number]
    for (new_player_stat in new_player)
    {
      new_play_stat_matrix[new_player_stat$sequence, ] <-
        c(
          new_null_to_NA(as.numeric(new_player_stat$statId)),
          new_null_to_NA(as.numeric(new_player_stat$yards)),
          new_null_to_NA(as.numeric(new_player_stat$sequence))
        )
      new_player_id_matrix[new_player_stat$sequence, ] <-
        c(
          new_null_to_NA(new_player_id), new_null_to_NA(new_player_stat$playerName), new_null_to_NA(new_player_stat$clubcode), new_null_to_NA(names(new_stat_map)[new_player_stat$statId == new_stat_map])
        )
    }
  }
  # creation of plad_data_matrices complete
  tictoc::toc(log = TRUE, quiet = TRUE)


  # now create new_play_player_data. This will exactly match play_player_data in the current algorithm.
  # I won't use this, but it will be needed to allow my changes to be dropped in place of existing code, while
  # maintaing full compatablity.
  new_play_player_data <- cbind(as.data.frame(new_play_stat_matrix), as.data.frame(new_player_id_matrix, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  # order the rows to match the current table
  new_play_player_data <- dplyr::select(new_play_player_data, sequence, team, player_name, statId, yards, player_id, nfl_stat)
  # This is a bit funky, but it is need to pass the identical check against the current play_player_data
  # when all values in a this column are NA, nflscrapR defaults to NA (logical) rather than NA_character_
  if (all(is.na(new_play_player_data$player_name))) {
    new_play_player_data$player_name <- NA
  }
  # GENERAL NOTE THIS IS NECESSARY BECAUSE OF GAMES LIKE 2009080950 WHERE THE NFL DATA IS NOT ENTIRELY CORRECT
  if (all(is.na(new_play_player_data$team))) {
    new_play_player_data$team <- NA
  }
  tictoc::toc(log = TRUE, quiet = TRUE)
  # new_play_player_data is complete

  # initialize the play data
  # these columns will be discarded when matching current algorithm but doing this here could save work elsewhere in the program
  new_play_data <- as.data.frame(lapply(new_play[1:9], function(x) ifelse(is.null(x), NA, x)), stringsAsFactors = FALSE)

  # This numeric vector is used to check for the binary indicators. Theres no reason to check for the presence of a stat twice,
  # so removing duplicates will save a few checks on plays with repeated stats.
  new_unique_player_stats <- unique(new_play_stat_matrix[, "statId"])

  # now set the indicators that have a one to one correspondance to a single statId
  # rename the indicator stats that have overly specific names. The renaming doesn't affect matching,
  # since the stats are matched by their numeric value rather than by their name
  new_original_names <- c("safety_tackle", "penalty_yards", "field_goal_yards_missed", "field_goal_yards_made", "field_goal_yards_blocked")
  new_names_in_table <- c("safety", "penalty", "field_goal_missed", "field_goal_made", "field_goal_blocked")
  names(new_indicator_stats)[match(new_original_names, names(new_indicator_stats))] <- new_names_in_table
  new_play_data[names(new_indicator_stats)] <- as.numeric(new_indicator_stats %in% new_unique_player_stats)
  # revert the indicator stat names to match their true values
  names(new_indicator_stats)[match(new_names_in_table, names(new_indicator_stats))] <- new_original_names
  rm(new_original_names, new_names_in_table)




  # The logic for determining the indicators is unchanged. the stats for each play are checked for inclusion
  # in the vector of stats that defines each indicator. This is done using the numeric values, rather than the names.
  # integer matching should in theory be faster than string matching, but the difference is probably extremely negligible
  # in the context of this progam.

  new_play_data$rush_attempt <- as.numeric(any(new_rush_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$pass_attempt <- as.numeric(any(new_pass_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$sack <- as.numeric(any(new_sack_indicator_stats %in% new_unique_player_stats))
  new_play_data$touchdown <- as.numeric(any(new_touchdown_indicator_stats %in% new_unique_player_stats))
  new_play_data$pass_touchdown <- as.numeric(any(new_pass_touchdown_indicator_stats %in% new_unique_player_stats))
  new_play_data$rush_touchdown <- as.numeric(any(new_rush_touchdown_indicator_stats %in% new_unique_player_stats))
  new_play_data$return_touchdown <- as.numeric(any(new_return_touchdown_indicator_stats %in% new_unique_player_stats))
  new_play_data$extra_point_attempt <- as.numeric(any(new_extra_point_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$two_point_attempt <- as.numeric(any(new_two_point_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$field_goal_attempt <- as.numeric(any(new_field_goal_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$kickoff_attempt <- as.numeric(any(new_kickoff_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$punt_attempt <- as.numeric(any(new_punt_attempt_indicator_stats %in% new_unique_player_stats))
  new_play_data$fumble <- as.numeric(any(new_fumble_indicator_stats %in% new_unique_player_stats))
  new_play_data$complete_pass <- as.numeric(any(new_complete_pass_indicator_stats %in% new_unique_player_stats))
  new_play_data$assist_tackle <- as.numeric(any(new_assist_tackle_indicator_stats %in% new_unique_player_stats))
  new_play_data$lateral_reception <- as.numeric(any(new_unique_player_stats %in% new_lateral_reception_indicator_stats))
  new_play_data$lateral_rush <- as.numeric(any(new_unique_player_stats %in% new_lateral_rush_indicator_stats))
  new_play_data$lateral_return <- as.numeric(any(new_unique_player_stats %in% new_lateral_return_indicator_stats))
  new_play_data$lateral_recovery <- as.numeric(any(new_unique_player_stats %in% new_lateral_recovery_indicator_stats))



  # Thoughts on speeding up the logic
  # in the future, we can avoid evalution many of the matching operations
  # because many indicators preclude other indicators. for example, there cannot be a rushing touchdown without
  # a rush attempt.
  # &&  and || will stop evaluation in the case that further evaluation cannot change the result
  # for && this means that the right hand side of the expression is FALSE, nothing to the left is evaluated and FALSE is returned
  # we can avoid evaluation several of the tests for several indicators with very quick checks of already calculated indicators
  # several stats also generate a note: punts, kickoff, touchdowns, fumbles
  # note isn't a perfect replacement for these inclusion tests. note only takes a single value when multiple note triggering
  # stats happen. but on plays with no note, we might be able to skip quite a few of the tests for less common stats like two point
  # conversions simply based on the fact that there isn't a note
  # i'd have to do some tests to confirm that the note is reliably generated before these are considered
  # The following commented code does a little bit of this
  #
  # new_touchdown_ind <- any(new_touchdown_indicator_stats %in% new_unique_player_stats)
  # new_rush_attempt_ind <- any(new_rushing_indicator_stats %in% new_unique_player_stats)
  # new_rush_touchdown_ind <- new_touchdown_ind && new_rush_attempt_ind && any(new_unique_player_stats %in% new_rushing_touchdown_indicator_stats)
  #
  # new_pass_attempt_ind <- !new_rush_attempt_ind && any(new_passing_attempt_indicator_stats %in%
  #    new_unique_player_stats)
  # new_complete_pass_ind <- new_pass_attempt_ind && any(new_complete_pass_indicator_stats %in% new_unique_player_stats)
  #
  # new_pass_touchdown_ind <- new_touchdown_ind && new_complete_pass_ind && any(new_unique_player_stats %in% new_passing_touchdown_indicator_stats)
  #
  #
  # new_sack_ind <- any(new_sack_indicator_stats %in% new_unique_player_stats)
  #
  # new_return_touchdown_ind <- new_touchdown_ind && (!(new_pass_touchdown_ind || new_rush_touchdown_ind)) && any(new_unique_player_stats %in% new_return_touchdown_indicator_stats)
  #
  # new_extra_point_attempt_ind <- any(new_extra_point_attempt_indicator_stats %in%
  #   new_unique_player_stats)
  #
  # new_two_point_attempt_ind <- any(new_two_point_attempt_indicator_stats %in%
  #   new_unique_player_stats)
  #
  # new_field_goal_attempt_ind <- any(new_field_goal_attempt_indicator_stats %in% new_unique_player_stats)
  # new_kickoff_attempt_ind <- any(new_kickoff_attempt_indicator_stats %in% new_unique_player_stats)
  #
  # new_punt_attempt_ind <- any(new_punt_attempt_indicator_stats %in% new_unique_player_stats)
  #
  # new_fumble_ind <- any(new_fumble_indicator_stats %in% new_unique_player_stats)
  #
  #
  # new_assist_tackle_ind <- any(new_assist_tackle_indicator_stats %in% new_unique_player_stats)
  #
  # new_lateral_reception_ind <- any(new_unique_player_stats %in% new_lateral_reception_indicator_stats)
  # new_lateral_rush_ind <- any(new_unique_player_stats %in% new_lateral_rush_indicator_stats)
  # new_lateral_return_ind <- any(new_unique_player_stats %in% new_lateral_return_indicator_stats)
  # new_lateral_recovery_ind <- any(new_unique_player_stats %in% new_lateral_recovery_indicator_stats)

  # new_play_data$rush_attempt <- as.numeric(new_rush_attempt_ind)
  # new_play_data$pass_attempt <- as.numeric(new_pass_attempt_ind)
  # new_play_data$sack <- as.numeric(new_sack_ind)
  # new_play_data$touchdown <- as.numeric(new_touchdown_ind)
  # new_play_data$pass_touchdown <- as.numeric(new_pass_touchdown_ind)
  # new_play_data$rush_touchdown <- as.numeric(new_rush_touchdown_ind)
  # new_play_data$return_touchdown <- as.numeric(new_return_touchdown_ind)
  # new_play_data$extra_point_attempt <- as.numeric(new_extra_point_attempt_ind)
  # new_play_data$two_point_attempt <- as.numeric(new_two_point_attempt_ind)
  # new_play_data$field_goal_attempt <- as.numeric(new_field_goal_attempt_ind)
  # new_play_data$kickoff_attempt <- as.numeric(new_kickoff_attempt_ind)
  # new_play_data$punt_attempt <- as.numeric(new_punt_attempt_ind)
  # new_play_data$fumble <- as.numeric(new_fumble_ind)
  # new_play_data$complete_pass <- as.numeric(new_complete_pass_ind)
  # new_play_data$assist_tackle <- as.numeric(new_assist_tackle_ind)
  # new_play_data$lateral_reception <- as.numeric(new_lateral_reception_ind)
  # new_play_data$lateral_rush <- as.numeric(new_lateral_rush_ind)
  # new_play_data$lateral_return <- as.numeric(new_lateral_return_ind)
  # new_play_data$lateral_recovery <- as.numeric(new_lateral_recovery_ind)

  # discard rows not created by current algorithm
  new_play_data[names(new_play)[1:9]] <- NULL

  return(list(new_play_data = new_play_data, new_player_ids = new_player_ids, new_play_player_data = new_play_player_data, new_indicator_stats = names(new_indicator_stats)))
}
