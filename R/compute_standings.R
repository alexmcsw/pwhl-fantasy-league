library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Team Standings**
#' @description Compute PWHL fantasy team standings to date.
#'
#' @param roster_points_per_game data.frame of points earned by each fantasy
#' team, per game
#' @param team_images Images associated to each team
#' @return A data frame with aggregated fantasy team standings
#' @import dplyr
#' @import magrittr
#' @export

compute_standings <- function(
  roster_points_per_game,
  team_images
) {
  standings <- roster_points_per_game |>
    summarise(
      across(
        -game_date,
        ~ sum(
          .,
          na.rm = TRUE
        )
      )
    )

  standings <- standings[
    order(
      unlist(
        standings
      ),
      decreasing = TRUE
    )
  ] |>
    stack() |>
    set_names(
      c(
        "points",
        "team_name"
      )
    )

  standings <- standings |>
    mutate(
      Team = recode(
        team_name,
        !!!team_images
      )
    ) |>
    rename(
      Name = team_name,
      Points = points
    ) |>
    select(
      c(
        "Name",
        "Team",
        "Points"
      )
    )

  return(standings)
}
