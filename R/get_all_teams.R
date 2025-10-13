library(dplyr)

#' @title  **Get all PWHL player info and stats**
#' @description Get all PWHL player info and stats
#'
#' @param season_id Current season ID
#' @param season Current season
#' @param game_type Current season game type
#' @return data.frame of PWHL player info and stats
#' @import dplyr
#' @export
#' #' #' @examples
#' \donttest{
#'   try(get_all_teams(2026,8,"regular"))
#' }

get_all_teams <- function(
  season_id,
  season,
  game_type
) {
  teams <- pwhl_teams(
    season_id = season_id
  )

  team_codes <- teams$team_code

  names(team_codes) <- teams$team_label |>
    recode(
      "Montreal" = "Montréal"
    )

  all_teams <- team_stats(
    season = season,
    teams = teams,
    game_type = game_type
  )
}
