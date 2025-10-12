library(dplyr)
library(magrittr)

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
