% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_scrape_play_by_play.R
\name{validation_scrape_season_play_by_play}
\alias{validation_scrape_season_play_by_play}
\title{Scrape season play-by-play for a given NFL season (either pre, regular, or post-season)}
\usage{
validation_scrape_season_play_by_play(season, type = "reg",
  weeks = NULL, teams = NULL)
}
\arguments{
\item{season}{Numeric 4-digit year associated with an NFL season}

\item{type}{String indicating the type of game play-by-play, must either be
"pre", "reg", or "post" (default is "reg").}

\item{weeks}{Numeric vector indicating which weeks of preseason (0 to 4 except
for 2011 season without Hall of Fame game) or regular season games (1 to 17)
to grab (default value of NULL corresponds to selecting all available weeks).}

\item{teams}{String vector indicating which teams (based on the abbreviation)
the function should grab game info for (default value of NULL corresponds to
selecting all available teams).}
}
\value{
Data frame where each individual row represents a single play in
the game containing detailed information depending on the availability of
data for the game. See \code{\link{validation_scrape_json_play_by_play}} and
\code{\link{scrape_html_play_by_play}} for documentation about the
columns returned.
}
\description{
Depending on the year and type of the given game id, this function returns
the play-by-play data available from NFL.com from either JSON (games starting
in 2009) or parsed HTML (regular and post-season games starting in 1998,
pre-season games starting in 2000). The necessary info regarding the game's
play-by-play data to be scraped (type and season) is provided by the
\code{\link{validation_scrape_game_ids}} function. In addition to all of the play-level data
provided by the NFL, outputs from the `nflscrapR` expected points and win
probability models are also included, allowing an individual to conduct their
own analysis with more advanced football statistics. A detailed description
of the models methodologies can be found here: \url{https://arxiv.org/abs/1802.00998}.
This function acts as a wrapper function for calling the
\code{\link{validation_scrape_game_play_by_play}} function over all of the games meeting
the criteria of the user's input.
}
\examples{
# Scrape the play-by-play data for the 2017 Super Bowl by first getting all
# of the post-season game ids then use the required info to scrape the
# play-by-play data (knowing that it's the last game):
playoff_game_ids_17 <- validation_scrape_game_ids(2017, type = "post")
sb_17_id <- playoff_game_ids_17$game_id[nrow(playoff_game_ids_17)]
sb_17_pbp <- validation_scrape_game_play_by_play(game_id = sb_17_id, type = "post",
                                      season = 2017)
}
