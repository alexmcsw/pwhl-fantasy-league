get_season_dates_and_type <- function() {
    df <- pwhl_season_id()

    df$start_date <- df %>%
        mutate(
            start_date = {
                map(
                    .$season_id,
                    function(x) {
                        if (x < 9) {
                            pwhl_schedule(
                                season_id = x
                            ) |>
                                select(
                                    game_date
                                ) |>
                                first()
                        } else {
                            pwhl_schedule(
                                season_id = 8
                            ) |>
                                select(
                                    game_date
                                ) |>
                                last()
                        }
                    }
                )
            }
        ) %>%
        mutate(
            temp = {
                mapply(
                    str_split,
                    .$start_date,
                    pattern = ", "
                )
            } |>
                map(
                    last
                )
        ) %>%
        {
            ifelse(
                .$game_type_label == "playoffs",
                paste0(
                    .$season_yr,
                    " ",
                    .$temp
                ),
                paste0(
                    .$season_yr - 1,
                    " ",
                    .$temp
                )
            )
        } |>
        ymd()

    df$end_date <- df %>%
        mutate(
            end_date = {
                map(
                    .$season_id,
                    function(x) {
                        if (x < 9) {
                            pwhl_schedule(
                                season_id = x
                            ) |>
                                select(
                                    game_date
                                ) |>
                                last()
                        } else {
                            pwhl_schedule(
                                season_id = 8
                            ) |>
                                select(
                                    game_date
                                ) |>
                                last()
                        }
                    }
                )
            }
        ) %>%
        mutate(
            temp = {
                mapply(
                    str_split,
                    .$end_date,
                    pattern = ", "
                )
            } |>
                map(
                    last
                )
        ) %>%
        {
            ifelse(
                .$game_type_label == "preseason",
                paste0(
                    .$season_yr - 1,
                    " ",
                    .$temp
                ),
                paste0(
                    .$season_yr,
                    " ",
                    .$temp
                )
            )
        } |>
        ymd()

    return(df)
}
