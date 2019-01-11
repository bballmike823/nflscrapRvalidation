#' utility for viewing and summarizing timing data that is recorded
#' #TODO clear ticlog and save only the number of timings and total time for each event
#' @export
timing_summary <- function() {
  extract_timings <- function(timing_log) {
    as.numeric(stringr::str_extract(timing_log, "[:digit:]+([.][:digit:]{2})?"))
  }
  time_log <- unlist(tictoc::tic.log(), 1)
  events_tracked <- sort(unique(stringr::str_match(time_log, "([[:lower:]+[_]]*[:lower:]+)[:]")[, 1]))
  for (timed_event in events_tracked) {
    recorded_times <- extract_timings(time_log[stringr::str_detect(time_log, timed_event)])
    writeLines(paste0(timed_event, " number of timings ", as.character(length(recorded_times)), " total time ", sum(recorded_times)))
    writeLines(paste0("average time ", as.character(mean(recorded_times))))
  }
}

#' test the speed of the hashmap and vector methods of looking up stats
#' several possible syntax options for the vector lookup are tested
#' run a benchmark that compares the speed and accuracy of hashmap and named vectors for the needs of nflscrapR
#' nfl_stat_map is used once, in a situation where it only looks up scalar values that are always hits
#' in nflscrapR, the stats are force cast to character beforehand so included a test that shows the cost of doing so
#' as well as a test where the cost of doing so is not included
#' the == notation matches functionality only under those conditions, while match should
#' perform in the event of a non match or you want to look up a vector of stat ids.

#' @param number_of_trials number of microbenchmark trials to run. 10000 is the default
#'
#' @export
hashmap_vs_vector_test <- function(number_of_trials = 10000) {
  # I have copied and pasted the code when nfl_stat_map is created
  nfl_stat_map <- hashmap::hashmap(
    as.character(c(
      2:16, 19:64, 68:80, 82:89,
      91, 93, 95:96, 99, 100, 102:108,
      110:113, 115, 120, 301, 402,
      410, 420, 403, 404, 405, 406
    )),
    c(
      "punt_blocked", "first_down_rush",
      "first_down_pass", "first_down_penalty",
      "third_down_converted", "third_down_failed",
      "fourth_down_converted", "fourth_down_failed",
      "rushing_yards", "rushing_yards_td",
      "lateral_rushing_yards", "lateral_rushing_yards_td",
      "incomplete_pass", "passing_yards",
      "passing_yards_td", "interception",
      "sack_yards", "receiving_yards",
      "receiving_yards_td", "lateral_receiving_yards",
      "lateral_receiving_yards_td",
      "interception_return_yards",
      "interception_return_yards_td",
      "lateral_interception_return_yards",
      "lateral_interception_return_yards_td",
      "punting_yards", "punt_inside_twenty",
      "punt_in_endzone", "punt_touchback_kicking",
      "punt_return_yards", "punt_return_yards_td",
      "lateral_punt_return_yards",
      "lateral_punt_return_yards_td",
      "punt_out_of_bounds", "punt_downed",
      "punt_fair_catch", "punt_touchback_receiving",
      "kickoff_yards", "kickoff_inside_twenty",
      "kickoff_in_endzone", "kickoff_touchback_kicking",
      "kickoff_return_yards", "kickoff_return_yards_td",
      "lateral_kickoff_return_yards",
      "lateral_kickoff_return_yards_td",
      "kickoff_out_of_bounds", "kickoff_fair_catch",
      "kickoff_touchback_receiving",
      "fumble_forced", "fumble_not_forced",
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
      "timeout", "field_goal_yards_missed",
      "field_goal_yards_made",
      "field_goal_yards_blocked",
      "extra_point_good", "extra_point_failed",
      "extra_point_blocked",
      "two_point_rush_good", "two_point_rush_failed",
      "two_point_pass_good", "two_point_pass_failed",
      "solo_tackle", "assisted_tackle",
      "tackle_assist", "solo_sack_yards",
      "assist_sack_yards", "pass_defense_player",
      "punt_blocked_player", "extra_point_blocked_player",
      "field_goal_blocked_player", "safety_tackle",
      "forced_fumble_player", "penalty_yards",
      "tackled_for_loss", "extra_point_safety",
      "two_point_rush_safety", "two_point_pass_safety",
      "kickoff_downed", "lateral_sack_yards",
      "two_point_pass_reception_good",
      "two_point_pass_reception_failed",
      "fumble_lost", "own_kickoff_recovery",
      "own_kickoff_recovery_td",
      "qb_hit", "air_yards_complete",
      "air_yards_incomplete",
      "yards_after_catch",
      "targeted_receiver",
      "tackle_for_loss_player",
      "extra_point_aborted",
      "tackle_for_loss_yards",
      "kickoff_yard_length",
      "two_point_return",
      "defensive_two_point_attempt",
      "defensive_two_point_conv",
      "defensive_extra_point_attempt",
      "defensive_extra_point_conv"
    )
  )

  # create new_stat_map
  # I did it like this to avoid copy and pasting the vectors, and make the code more concise
  new_stat_map <- as.numeric(nfl_stat_map$keys())
  names(new_stat_map) <- nfl_stat_map$values()

  #create the character_vector_map
  character_vector_map<-rep(NA,420)
  for(new_index in 1:length(new_stat_map)){
    character_vector_map[new_stat_map[new_index]]<-names(new_stat_map)[new_index]
  }


  rand_stat <- 0
  counter <- 7
  return(microbenchmark::microbenchmark(
    names(new_stat_map[new_stat_map == rand_stat]),
    names(new_stat_map)[new_stat_map == rand_stat],
    nfl_stat_map$find(as.character(rand_stat)),
    nfl_stat_map$find(rand_stat_char),
    names(new_stat_map)[match(rand_stat, new_stat_map)],
    names(new_stat_map[match(rand_stat, new_stat_map)]),
    character_vector_map[rand_stat],
    setup = {
      if (counter == 7) {
        counter <- -1
        rand_stat <- as.numeric(sample(nfl_stat_map$keys(), 1))
        rand_stat_char <- as.character(rand_stat)
      }
      counter <- counter + 1
    },
    control = list(order = "inorder"),
    check = function(x)
      identical(x[[1]], x[[2]]) && identical(x[[2]], x[[3]]) && identical(x[[3]], x[[4]]) && identical(x[[4]], x[[5]]) && identical(x[[5]], x[[6]]) && identical(x[[6]],x[[7]]),
    times = number_of_trials
  ))
}

#' runs the new code and the current code for every available play
#' the current code is copy and pasted into a function that
#' takes play as an input and returns a list of the data it creates
#' the outside data it needs(nfl_stat_map) is provided for it
#'
#' @param number_of_games number of random games to test. not used if smoke_test is true
#' @param smoke_test TRUE/FALSE when true, run every available game
#'
#' @export
test_new_function_validity <- function(number_of_games = 25, smoke_test = FALSE) {
  # I have copied and pasted the code when nfl_stat_map is created
  nfl_stat_map <- hashmap::hashmap(
    as.character(c(
      2:16, 19:64, 68:80, 82:89,
      91, 93, 95:96, 99, 100, 102:108,
      110:113, 115, 120, 301, 402,
      410, 420, 403, 404, 405, 406
    )),
    c(
      "punt_blocked", "first_down_rush",
      "first_down_pass", "first_down_penalty",
      "third_down_converted", "third_down_failed",
      "fourth_down_converted", "fourth_down_failed",
      "rushing_yards", "rushing_yards_td",
      "lateral_rushing_yards", "lateral_rushing_yards_td",
      "incomplete_pass", "passing_yards",
      "passing_yards_td", "interception",
      "sack_yards", "receiving_yards",
      "receiving_yards_td", "lateral_receiving_yards",
      "lateral_receiving_yards_td",
      "interception_return_yards",
      "interception_return_yards_td",
      "lateral_interception_return_yards",
      "lateral_interception_return_yards_td",
      "punting_yards", "punt_inside_twenty",
      "punt_in_endzone", "punt_touchback_kicking",
      "punt_return_yards", "punt_return_yards_td",
      "lateral_punt_return_yards",
      "lateral_punt_return_yards_td",
      "punt_out_of_bounds", "punt_downed",
      "punt_fair_catch", "punt_touchback_receiving",
      "kickoff_yards", "kickoff_inside_twenty",
      "kickoff_in_endzone", "kickoff_touchback_kicking",
      "kickoff_return_yards", "kickoff_return_yards_td",
      "lateral_kickoff_return_yards",
      "lateral_kickoff_return_yards_td",
      "kickoff_out_of_bounds", "kickoff_fair_catch",
      "kickoff_touchback_receiving",
      "fumble_forced", "fumble_not_forced",
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
      "timeout", "field_goal_yards_missed",
      "field_goal_yards_made",
      "field_goal_yards_blocked",
      "extra_point_good", "extra_point_failed",
      "extra_point_blocked",
      "two_point_rush_good", "two_point_rush_failed",
      "two_point_pass_good", "two_point_pass_failed",
      "solo_tackle", "assisted_tackle",
      "tackle_assist", "solo_sack_yards",
      "assist_sack_yards", "pass_defense_player",
      "punt_blocked_player", "extra_point_blocked_player",
      "field_goal_blocked_player", "safety_tackle",
      "forced_fumble_player", "penalty_yards",
      "tackled_for_loss", "extra_point_safety",
      "two_point_rush_safety", "two_point_pass_safety",
      "kickoff_downed", "lateral_sack_yards",
      "two_point_pass_reception_good",
      "two_point_pass_reception_failed",
      "fumble_lost", "own_kickoff_recovery",
      "own_kickoff_recovery_td",
      "qb_hit", "air_yards_complete",
      "air_yards_incomplete",
      "yards_after_catch",
      "targeted_receiver",
      "tackle_for_loss_player",
      "extra_point_aborted",
      "tackle_for_loss_yards",
      "kickoff_yard_length",
      "two_point_return",
      "defensive_two_point_attempt",
      "defensive_two_point_conv",
      "defensive_extra_point_attempt",
      "defensive_extra_point_conv"
    )
  )
  # The current implementation,again copied and pasted
  # a list of all variables created is returned.
  # this is defined here so that it has access to nfl_stat_map, which is defined elsewhere in nflscrapR
  current_code <- function(play) {
    tictoc::tic(msg = "creating_play_player_data_currently")
    # Vector of player ids involved in the play:
    player_ids <- names(play$players)

    # Generate a data frame containing the player level information:
    play_player_data <- lapply(
      c(1:length(player_ids)),
      function(x) as.data.frame(do.call(
          rbind,
          play$players[[x]]
        ))
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_all(function(x) lapply(x, function(y) ifelse(is.null(y), NA, y))) %>%
      dplyr::mutate_all(unlist) %>%
      # suppressWarnings(purrr::flatten_dfr(play$players)) %>%
      # as.data.frame() %>%
      # Add the player ids:
      dplyr::mutate(
        player_id = purrr::map(
          c(1:length(player_ids)),
          function(x) {
            rep(
              player_ids[x],
              length(play$players[[x]])
            )
          }
        ) %>% purrr::flatten_chr(),
        # Look-up the NFL stat names to use for each statId
        nfl_stat = purrr::map_chr(as.character(statId), function(x) nfl_stat_map$find(x))
      ) %>%
      # Remove duplicates
      dplyr::distinct() %>%
      # Rename the team and player name columns:
      dplyr::rename(team = clubcode, player_name = playerName) %>%
      # Arrange by the sequence number:
      dplyr::arrange(sequence) %>%
      dplyr::mutate(yards = as.numeric(yards))
    tictoc::toc(log = TRUE, quiet = TRUE)
    # Now use this dataframe to generate a single row summarizing the NFL's API
    # data, but simplest way is to do this in pieces by breaking up the certain
    # NFL stats which are for: (1) indicators, (2) players, (3) teams, and
    # (4) yards. All of the desired columns that are not in the play will be
    # just populated with NA (this will be found by comparing at the end
    # which columns are there)

    # (1)
    # First the indicator variables, this will serve as a way of initializing
    # the row of play data to return. Start by making a vector of the indicator
    # statistics to check for:
    indicator_stats <- c(
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
    )

    # Go through each of the indicator variables to see if the player level
    # dataset has it the NFL stat or not, initializing the play_data row:
    play_data <- purrr::map(
      indicator_stats,
      function(x) {
        dplyr::if_else(
          x %in% play_player_data$nfl_stat,
          1, 0
        )
      }
    ) %>%
      purrr::set_names(indicator_stats) %>%
      as.data.frame() %>%
      # Rename some of the variables that were used for indicators that also
      # had additional info not necessary in their name:
      dplyr::rename(
        safety = safety_tackle, penalty = penalty_yards,
        field_goal_missed = field_goal_yards_missed,
        field_goal_made = field_goal_yards_made,
        field_goal_blocked = field_goal_yards_blocked
      ) %>%
      # Now make indicators that are based on the presence of potentially
      # mutliple variables:
      dplyr::mutate(
        rush_attempt = dplyr::if_else(
          any(c(
            "rushing_yards",
            "rushing_yards_td",
            "lateral_rushing_yards",
            "lateral_rushing_yards_td",
            "two_point_rush_good",
            "two_point_rush_failed",
            "two_point_rush_safety"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        pass_attempt = dplyr::if_else(
          any(c(
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
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        sack = dplyr::if_else(any(c(
          "sack_yards", "solo_sack_yards",
          "assist_sack_yards"
        ) %in%
          play_player_data$nfl_stat), 1, 0),
        touchdown = dplyr::if_else(
          any(c(
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
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        pass_touchdown = dplyr::if_else(
          any(c(
            "passing_yards_td",
            "receiving_yards_td",
            "lateral_receiving_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        rush_touchdown = dplyr::if_else(
          any(c(
            "rushing_yards_td",
            "lateral_rushing_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        return_touchdown = dplyr::if_else(
          any(c(
            "interception_return_yards_td",
            "lateral_interception_return_yards_td",
            "kickoff_return_yards_td",
            "lateral_kickoff_return_yards_td",
            "punt_return_yards_td",
            "lateral_punt_return_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        extra_point_attempt = dplyr::if_else(
          any(c(
            "extra_point_good",
            "extra_point_failed",
            "extra_point_blocked",
            "extra_point_safety",
            "extra_point_aborted"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        two_point_attempt = dplyr::if_else(
          any(c(
            "two_point_rush_good",
            "two_point_rush_failed",
            "two_point_pass_good",
            "two_point_pass_failed",
            "two_point_rush_safety",
            "two_point_pass_safety",
            "two_point_pass_reception_good",
            "two_point_pass_reception_failed",
            "two_point_return"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        field_goal_attempt = dplyr::if_else(
          any(c(
            "field_goal_yards_missed",
            "field_goal_yards_made",
            "field_goal_yards_blocked",
            "field_goal_blocked_player"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        kickoff_attempt = dplyr::if_else(
          any(c(
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
            "kickoff_downed",
            "own_kickoff_recovery",
            "own_kickoff_recovery_td",
            "kickoff_yard_length"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        punt_attempt = dplyr::if_else(
          any(c(
            "punt_blocked",
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
            "punt_blocked_player"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        fumble = dplyr::if_else(
          any(c(
            "fumble_forced", "fumble_not_forced",
            "fumble_out_of_bounds",
            "own_fumble_recovery_yards",
            "own_fumble_recovery_yards_td",
            "lateral_own_fumble_recovery_yards",
            "lateral_own_fumble_recovery_yards_td",
            "opp_fumble_recovery_yards",
            "opp_fumble_recovery_yards_td",
            "lateral_opp_fumble_recovery_yards",
            "lateral_opp_fumble_recovery_yards_td",
            "forced_fumble_player",
            "fumble_lost"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        complete_pass = dplyr::if_else(
          any(c(
            "passing_yards",
            "passing_yards_td",
            "receiving_yards",
            "receiving_yards_td",
            "lateral_receiving_yards",
            "lateral_receiving_yards_td",
            "air_yards_complete",
            "yards_after_catch"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        assist_tackle = dplyr::if_else(
          any(c(
            "assisted_tackle",
            "tackle_assist",
            "assist_sack_yards"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        lateral_reception = dplyr::if_else(
          any(c(
            "lateral_receiving_yards",
            "lateral_receiving_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        lateral_rush = dplyr::if_else(
          any(c(
            "lateral_rushing_yards",
            "lateral_rushing_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        lateral_return = dplyr::if_else(
          any(c(
            "lateral_interception_return_yards",
            "lateral_interception_return_yards_td",
            "lateral_punt_return_yards",
            "lateral_punt_return_yards_td",
            "lateral_kickoff_return_yards",
            "lateral_kickoff_return_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        ),
        lateral_recovery = dplyr::if_else(
          any(c(
            "lateral_own_fumble_recovery_yards",
            "lateral_own_fumble_recovery_yards_td",
            "lateral_opp_fumble_recovery_yards",
            "lateral_opp_fumble_recovery_yards_td"
          ) %in%
            play_player_data$nfl_stat),
          1, 0
        )
      )

    return(list(play_data = play_data, player_ids = player_ids, play_player_data = play_player_data, indicator_stats = indicator_stats))
  }


  # this is the test that run for each game.
  # every play that has players is run against the current code and new_function
  # error when any variables created do not match
  individual_game_test <- function(new_url) {
    new_json <- tryCatch(RJSONIO::fromJSON(RCurl::getURL(new_url)),
      error = function(cond) {
        message("Connection to NFL.com disrupted, please re-run code.")
        message(paste("Here is the game's url:", new_url))
        message("Here's the original error message:")
        message(cond)
        # Just return NA in case of error
        return(NA)
      }
    )
    if (assertthat::is.scalar(new_json) && is.na(new_json)) {
      writeLines("skipping unavailable game")
      return()
    }
    new_json[[1]]$drives$crntdrv <- NULL
    for (new_drive in new_json[[1]]$drives)
    {
      for (new_play in new_drive$plays)
      {
        # plays with no players will never reach this point in the code so let's skip them
        if (length(new_play$players) == 0) {
          next()
        }
        tictoc::tic(msg = "smoke_test_current_code")
        current_results <- current_code(new_play)
        tictoc::toc(log = TRUE, quiet = TRUE)
        tictoc::tic(msg = "smoke_test_new_function")
        new_function_results <- new_function(new_play)
        tictoc::toc(log = TRUE, quiet = TRUE)
        assertthat::assert_that(
          identical(
            new_function_results$new_play_data,
            current_results$play_data
          ),
          msg = paste0(
            "play_data non match on ",
            as.character(season),
            " ",
            type,
            " ",
            names(new_json)[1]
          )
        )
        assertthat::assert_that(
          identical(
            new_function_results$new_play_player_data,
            current_results$play_player_data
          ),
          msg = paste0(
            "play_player_data non match on ",
            as.character(season),
            " ",
            type,
            " ",
            names(new_json)[1]
          )
        )
        assertthat::assert_that(
          identical(
            new_function_results$new_player_ids,
            current_results$player_ids
          ),
          msg = paste0(
            "player_ids non match on ",
            as.character(season),
            " ",
            type,
            " ",
            names(new_json)[1]
          )
        )
        assertthat::assert_that(
          identical(
            new_function_results$new_indicator_stats,
            current_results$indicator_stats
          ),
          msg = paste0(
            "indicator_stats non match on ",
            as.character(season),
            " ",
            type,
            " ",
            names(new_json)[1]
          )
        )
      }
    }
  }


  if (smoke_test) {
    # keep track of how many games have been tested and give periodic updates
    # this will take a while to run
    game_counter <- 0
    # now we can run the tests
    for (season in 2009:2018) {
      for (type in c("pre", "reg", "post")) {
        new_ids <- nflscrapR::scrape_game_ids(season, type)
        for (new_url in new_ids$game_url)
        {
          individual_game_test(new_url)
          game_counter <- game_counter + 1
          if (game_counter %% 5 == 0) {
            writeLines(paste(as.character(game_counter), "games tested"))
            timing_summary()
            # This is to ensure that tictoc doesn't slow things down
            # It stores every log entry in a list, and R lists get slow when
            # they get large and new elements are added one by one.
            # wtf kind of data structure are lists in R?
            # Is there some sort of dynamic array, like ArrayList in Java that could be used?
            # On initial runs of this test, I noticed everything getting significantly
            # slower as the test progressed. well over a million events had been logged
            # and the log was nearing 100MB.
            # this keeps the list small enough that it doesn't impact speed.
            tictoc::tic.clearlog()
          }
        }
        message(paste(as.character(season), type, "passed"))
        timing_summary()
      }
    }
    writeLines("new_function passed for all available plays")
    return(TRUE)
  } else {
    seasons <- 2009:2018
    types <- c("pre", "reg", "post")
    for (game_counter in 1:number_of_games)
    {
      new_season <- sample(seasons, 1)
      new_type <- sample(types, 1)
      new_week <- NULL
      # if pre or regular season, choose a random week to scrape ids from
      # scraping all ids is slower
      if (new_type == "reg") {
        new_week <- sample.int(17, 1)
      } else if (new_type == "pre") {
        new_week <- sample.int(4, 1)
      }
      new_ids <- nflscrapR::scrape_game_ids(new_season, new_type,new_week)
      individual_game_test(sample(new_ids$game_url, 1))
      if (game_counter %% 5 == 0) {
        writeLines(paste(as.character(game_counter), "games tested"))
        timing_summary()
        tictoc::tic.clearlog()
      }
    }
  }
  writeLines(paste("new_function passed test of", as.character(number_of_games), "games"))
  return(TRUE)
}
