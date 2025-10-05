get_roster_points_from_game <- function(
    team_rosters,
    game_id
) {
    game_box_score <- pwhl_player_box(game_id=game_id)

    game_points <- c(game_id)

    for (team_name in names(team_rosters)) {
        game_points <- c(
            game_points,
            game_box_score$skaters |> filter(
                player_id %in% team_rosters[team_name][[1]]$player_id
            ) |> select(
                points
            ) |> sum() + game_box_score$goalies |> filter(
                player_id %in% team_rosters[team_name][[1]]$player_id
            ) |> select(
                points
            ) |> sum() |> as.numeric()
        )
    }

    names(game_points) <- c(
        "game_id",
        names(
            team_rosters
        )
    )

    return(game_points)
}
