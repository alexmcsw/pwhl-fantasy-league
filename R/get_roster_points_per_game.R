get_roster_points_per_game <- function(
    team_rosters,
    player_boxes_per_game
) {

    return(
        player_boxes_per_game |> map(
            get_roster_points_from_game,
            team_rosters = team_rosters
        ) |> bind_rows() |> mutate(
            game_id = names(
                player_boxes_per_game
            )
        ) |> column_to_rownames(
            "game_id"
        )
    )
}