parse_schedule_year <- function(
    game_date,
    season_start
) {
    game_date_with_year <- ymd(
        paste0(
            year(season_start),
            " ",
            game_date
        )
    )

    if (game_date_with_year < season_start) {
        year(game_date_with_year) <- year(game_date_with_year) + 1
    }

    return(
        game_date_with_year
    )
}
