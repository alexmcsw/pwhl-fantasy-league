library(dplyr)

#' @title  **Compute PWHL Fantasy Points from Game**
#' @description Compute how many points each PWHL fantasy team earned from a
#' game
#'
#' @param team_rosters data.frame of Fantasy team rosters
#' @param player_box_for_game Player box stats for a given game
#' @return data.frame of points earned by each fantasy team
#' @import dplyr
#' @export

get_roster_points_from_game <- function(
  team_rosters,
  player_box_for_game
) {
  game_points <- list()

  for (team_name in names(team_rosters)) {
    game_points[[team_name]] <- player_box_for_game$skaters |>
      filter(
        player_id %in% team_rosters[team_name][[1]]$player_id
      ) |>
      select(
        points
      ) |>
      sum() +
      player_box_for_game$goalies |>
        filter(
          player_id %in% team_rosters[team_name][[1]]$player_id
        ) |>
        select(
          points
        ) |>
        sum() |>
        as.numeric()
  }

  return(game_points)
}
