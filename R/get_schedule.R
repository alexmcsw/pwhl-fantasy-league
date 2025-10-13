get_schedule <- function(
  season_id,
  team_codes,
  team_logo_urls,
  season_dates_and_type
) {
  schedule <- pwhl_schedule(
    season_id = season_id
  )

  schedule$home_team_code <- team_codes[
    match(
      schedule$home_team,
      names(team_codes)
    )
  ]

  schedule$home_team_logo <- team_logo_urls[
    match(
      schedule$home_team_code,
      names(team_logo_urls)
    )
  ]

  schedule$home_team_code <- team_codes[
    match(
      schedule$home_team,
      names(team_codes)
    )
  ]

  schedule$away_team_code <- team_codes[
    match(
      schedule$away_team,
      names(team_codes)
    )
  ]

  schedule$away_team_logo <- team_logo_urls[
    match(
      schedule$away_team_code,
      names(team_logo_urls)
    )
  ]

  schedule$game_date <- schedule$game_date |>
    str_split_i(
      pattern = ",",
      i = -1
    ) |>
    trimws() |>
    map(
      parse_schedule_year,
      season_start = season_dates_and_type[
        season_id,
        "start_date"
      ]
    )

  return(schedule)
}
