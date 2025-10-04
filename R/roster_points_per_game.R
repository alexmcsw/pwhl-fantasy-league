roster_points_per_game <- function(
    team_rosters,
    season,
    n_games = -1
) {
    game_idx <- pwhl_schedule(
        season
    )$game_id

    if (n_games < 0) {
        n_games = length(
            game_idx
        )
    }

    lapply(
        game_idx[1:n_games],
        get_roster_points_from_game,
        team_rosters = team_rosters
    ) |> bind_rows()
}