# create a nice function to lapply to our team vector


# get data
team_stats <- function(
    season
) {
 
  # get vector of team abbreviations
  teams <- fastRhockey::pwhl_teams()$team_label

  all_teams <- data.frame()

  for (team in teams){

  # here we use our modified functions
  df_stats <- pwhl_stats_fix(position = "skater", team = team, season = season)

  df_team <- pwhl_team_roster(
    team = team,
    season = season
  ) %>%
      mutate(
        sign = DescTools::Zodiac(as.Date(dob))
      ) %>%
      merge(
        df_stats,
        by = c("player_id")
      )
    
    all_teams <- rbind(all_teams, df_team) |>
      filter(current_team == 1)
  }

  return(all_teams)

}