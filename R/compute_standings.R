compute_standings <- function(
    roster_points_per_game
) {
    standings <- roster_points_per_game |>
        select(-game_id) |>
        mutate_all(
            as.numeric
        ) |> map(
            sum
        )

    standings <- standings[
        order(
            unlist(
                standings
            ),
            decreasing=TRUE
        )
    ] |>
    stack() |>
    set_names(
        c(
            "points",
            "team_name"
        )
    )

    standings <- standings[
        c(
            "team_name",
            "points"
        )
    ]
}