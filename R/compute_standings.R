compute_standings <- function(
    roster_points_per_game
) {
    standings <- roster_points_per_game |>
    summarise(
        across(
            everything(),
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