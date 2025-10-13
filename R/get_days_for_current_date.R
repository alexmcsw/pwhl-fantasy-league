library(dplyr)
library(rlang)

#' @title  **Get PWHL Last/Next Game Dates and Game Info for Current Date**
#' @description Foir the current date, get the last/next game days, the season
#' ID, the season and the season type
#'
#' @param current_date Current Date
#' @param season_dates_and_type data.frame of all PWHL season dates and types
#' @return Named list of last/next games, season ID, season  and game type
#' @import dplyr
#' @import rlang
#' @export

get_days_for_current_date <- function(
  current_date,
  season_dates_and_type
) {
  idx_for_current_date <- season_dates_and_type |>
    (\(x) x$start_date < current_date & x$end_date >= current_date)() |>
    which()

  last_game_day_date <- "tbd"
  next_game_day_date <- "tbd"
  season_id <- season_dates_and_type[
    idx_for_current_date,
    "season_id"
  ]

  if (
    length(
      idx_for_current_date
    ) ==
      0
  ) {
    idx_for_current_date <- season_dates_and_type |>
      (\(x) x$start_date >= current_date)() |>
      which() |>
      first()

    if (idx_for_current_date == 1) {
      last_game_day_date <- NULL
    } else {
      last_game_day_date <- season_dates_and_type[
        idx_for_current_date - 1,
        "end_date"
      ]
    }

    next_game_day_date <- season_dates_and_type[
      idx_for_current_date,
      "start_date"
    ]

    season_id <- season_dates_and_type[
      idx_for_current_date,
      "season_id"
    ]
  }

  if (
    is.na(
      idx_for_current_date
    )
  ) {
    idx_for_current_date <- rownames(
      season_dates_and_type
    ) |>
      max()

    last_game_day_date <- season_dates_and_type[
      idx_for_current_date,
      "end_date"
    ]

    next_game_day_date <- NULL
    season_id <- NULL
  }

  if (
    !is.null(
      season_id
    )
  ) {
    season = season_dates_and_type |>
      filter(
        season_id == .env$season_id
      ) |>
      select(
        season_yr
      ) |>
      as.numeric()

    game_type <- season_dates_and_type |>
      filter(
        season_id == .env$season_id
      ) |>
      select(
        game_type_label
      ) |>
      as.character()
  }

  return(
    data.frame(
      last_game_day_date = last_game_day_date,
      next_game_day_date = next_game_day_date,
      season_id = season_id,
      season = season,
      game_type = game_type
    )
  )
}
