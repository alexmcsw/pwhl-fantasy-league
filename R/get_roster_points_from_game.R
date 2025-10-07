get_roster_points_from_game <- function(
    team_rosters,
    player_box_for_game
) {

    game_points <- list()

    for (team_name in names(team_rosters)) {
        game_points[[team_name]] <- player_box_for_game$skaters |> filter(
            player_id %in% team_rosters[team_name][[1]]$player_id
        ) |> select(
            points
        ) |> sum() + player_box_for_game$goalies |> filter(
            player_id %in% team_rosters[team_name][[1]]$player_id
        ) |> select(
            points
        ) |> sum() |> as.numeric()
    }

    return(game_points)
}
