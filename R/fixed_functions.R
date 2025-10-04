#' @title  **PWHL Stats**
#' @description PWHL Stats lookup
#'
#' @param position either goalie or skater. If skater, need to select a team.
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @import tidyverse
#' @export

pwhl_stats_fix <- function(
  position = "goalie",
  team = "BOS",
  season = 2023,
  regular = TRUE
) {
  team_id <- pwhl_teams() %>%
    dplyr::filter(.data$team_label == team) %>%
    dplyr::select(team_id)

  if (regular) {
    season_id <- 1
  } else if (!regular) {
    season_id <- 2
  }

  tryCatch(
    expr = {
      if (position == "goalie") {
        browser()
        URL <- glue::glue(
          "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team=all&position=goalies&rookies=0&statsType=expanded&rosterstatus=undefined&site_id=2&first=0&limit=20&sort=gaa&league_id=1&lang=en&division=-1&qualified=all&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._5"
        )

        res <- httr::RETRY(
          "GET",
          URL
        )

        res <- res %>%
          httr::content(as = "text", encoding = "utf-8")

        res <- gsub("angular.callbacks._5\\(", "", res)
        res <- gsub("}}]}]}])", "}}]}]}]", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data = r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {
          player_df <- data.frame(
            player_id = c(data[[y]]$row$player_id),
            player_name = c(data[[y]]$row$name),
            team = c(data[[y]]$row$team_code),
            games_played = c(data[[y]]$row$games_played),
            minutes = c(data[[y]]$row$minutes_played),
            shots_faced = c(data[[y]]$row$shots),
            goals_against = c(data[[y]]$row$goals_against),
            goals_against_avg = c(data[[y]]$row$goals_against_average),
            save_percentage = c(data[[y]]$row$save_percentage),
            shutouts = c(data[[y]]$row$shutouts),
            wins = c(data[[y]]$row$wins),
            losses = c(data[[y]]$row$losses),
            so_att = c(data[[y]]$row$shootout_attempts),
            so_goals_against = c(data[[y]]$row$shootout_goals_against),
            so_save_percentage = c(data[[y]]$row$shootout_percentage)
          )

          players <- dplyr::bind_rows(players, player_df)
        }

        players <- players %>%
          tidyr::separate(
            "minutes",
            into = c("minute", "second"),
            sep = ":",
            remove = FALSE
          )
      } else {
        URL <- glue::glue(
          "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team={team_id}&position=skaters&rookies=0&statsType=standard&rosterstatus=undefined&site_id=2&first=0&limit=20&sort=points&league_id=1&lang=en&division=-1&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._6"
        )

        res <- httr::RETRY(
          "GET",
          URL
        )

        res <- res %>%
          httr::content(as = "text", encoding = "utf-8")

        res <- gsub("angular.callbacks._6\\(", "", res)
        res <- sub(")", "", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data = r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {
          player_df <- data.frame(
            player_id = c(data[[y]]$row$player_id),
            player = c(data[[y]]$row$name),
            current_team = c(data[[y]]$row$active),
            position = c(data[[y]]$row$position),
            team = c(data[[y]]$row$team_code),
            games_played = c(data[[y]]$row$games_played),
            goals = c(data[[y]]$row$goals),
            shots = c(data[[y]]$row$shots),
            shooting_pct = c(data[[y]]$row$shooting_percentage),
            assists = c(data[[y]]$row$assists),
            points = c(data[[y]]$row$points),
            points_per_game = (data[[y]]$row$points_per_game),
            plus_minus = c(data[[y]]$row$plus_minus),
            penalty_minutes = c(data[[y]]$row$penalty_minutes),
            penalty_minutes_per_game = c(
              data[[y]]$row$penalty_minutes_per_game
            ),
            power_play_goals = c(data[[y]]$row$power_play_goals),
            power_play_assists = c(data[[y]]$row$power_play_assists),
            short_handed_goals = c(data[[y]]$row$short_handed_goals),
            short_handed_assists = c(data[[y]]$row$short_handed_assists),
            shootout_goals = c(data[[y]]$row$shootout_goals),
            shootout_attempts = c(data[[y]]$row$shootout_attempts),
            shootout_pct = c(data[[y]]$row$shootout_percentage),
            shootout_winning_goals = c(data[[y]]$row$shootout_winning_goals),
            faceoff_attempts = c(data[[y]]$row$faceoff_attempts),
            faceoff_wins = c(data[[y]]$row$faceoff_wins),
            faceoff_pct = c(data[[y]]$row$faceoff_pct)
          )
          players <- dplyr::bind_rows(players, player_df)
        }
      }
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  return(players)
}


#' @title  **PWHL Teams**
#' @description PWHL Teams lookup
#'
#' @return A data frame with team data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_teams())
#' }

pwhl_teams <- function() {
  full_url = "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=teamsForSeason&season=2&key=694cfeed58c932ee&client_code=pwhl&site_id=2&callback=angular.callbacks._4"

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._4\\(", "", res)
  res <- gsub("}]})", "}]}", res)

  r <- res %>%
    jsonlite::parse_json()

  team_info <- r$teamsNoAll
  teams <- data.frame()

  tryCatch(
    expr = {
      for (i in 1:length(team_info)) {
        team_df <- data.frame(
          "team_id" = c(team_info[[i]]$id),
          "team_name" = c(team_info[[i]]$name),
          "team_code" = c(team_info[[i]]$team_code),
          "team_nickname" = c(team_info[[i]]$nickname),
          "division" = c(team_info[[i]]$division_id),
          "team_logo" = c(team_info[[i]]$logo)
        )

        t <- data.frame(
          team_name = c(
            "PWHL Boston",
            "PWHL Minnesota",
            "PWHL Montreal",
            "PWHL New York",
            "PWHL Ottawa",
            "PWHL Toronto"
          ),
          team_label = c(
            "Boston",
            "Minnesota",
            "Montreal",
            "New York",
            "Ottawa",
            "Toronto"
          )
        )

        teams <- rbind(
          teams,
          team_df %>%
            dplyr::left_join(t, by = c("team_name"))
        )

        teams <- teams %>%
          dplyr::select(
            c(
              "team_name",
              "team_id",
              "team_code",
              "team_nickname",
              "team_label",
              "division",
              "team_logo"
            )
          )
      }
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  return(teams)
}


#' @title  **PWHL Rosters**
#' @description PWHL Rosters lookup
#'
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_team_roster(season = 2023, team = "Toronto"))
#' }

pwhl_team_roster <- function(team, season, regular = TRUE) {
  # team_id <- 1 # will need the team/season look ups
  team_id <- pwhl_teams() %>%
    dplyr::filter(.data$team_label == team)
  # season_id <- 2 # 1 is regular season, 2 is pre-season
  if (regular) {
    season_id <- 1
  } else if (!regular) {
    season_id <- 2
  }
  # base_url <- "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=1&season_id=2&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  full_url <- paste0(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=",
    team_id$team_id,
    "&season_id=",
    season_id,
    "&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  )

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._h\\(", "", res)
  res <- gsub("}}]}]}]})", "}}]}]}]}", res)

  r <- res %>%
    jsonlite::parse_json()

  team_name <- r[[1]]
  team_logo <- r[[2]]
  roster_year <- r[[3]]
  league <- r[[4]]

  players <- r[[5]][[1]]$sections

  roster_data <- data.frame()
  staff_data <- data.frame()

  player_types <- c("Forwards", "Defenders", "Goalies")

  tryCatch(
    expr = {
      for (i in seq_along(players)) {
        # i = 1

        if (players[[i]]$title %in% player_types) {
          # print('yes')

          for (p in seq_along(players[[i]]$data)) {
            if (is.null(players[[i]]$data[[p]]$row$shoots)) {
              hand <- players[[i]]$data[[p]]$row$catches
            } else {
              hand <- players[[i]]$data[[p]]$row$shoots
            }

            "player_id" %in% names(players[[i]]$data[[p]]$row)

            suppressWarnings(
              player_info <- data.frame(
                "player_id" = c(
                  if ("player_id" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$player_id
                  } else {
                    NA
                  }
                ),
                "player_name" = c(
                  if ("name" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$name
                  } else {
                    NA
                  }
                ),
                "primary_hand" = c(hand),
                "dob" = c(
                  if ("birthdate" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$birthdate
                  } else {
                    NA
                  }
                ),
                "height" = c(
                  if (
                    "height_hyphenated" %in% names(players[[i]]$data[[p]]$row)
                  ) {
                    players[[i]]$data[[p]]$row$height_hyphenated
                  } else {
                    NA
                  }
                ),
                "position" = c(
                  if ("position" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$position
                  } else {
                    NA
                  }
                ),
                "home_town" = c(
                  if ("hometown" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$hometown
                  } else {
                    NA
                  }
                )
              ) %>%
                tidyr::separate(
                  player_name,
                  into = c("first_name", "last_name"),
                  remove = FALSE,
                  sep = " "
                )
            )

            # players[[i]]$data[[p]]$prop

            roster_data <- rbind(
              roster_data,
              player_info
            )
          }
        } else {
          next
        }
      }

      roster_data <- roster_data %>%
        dplyr::mutate(
          league = "pwhl",
          age = round(lubridate::time_length(
            as.Date(paste0(season, "-01-01")) - as.Date(.data$dob),
            "years"
          )),
          player_headshot = paste0(
            "https://assets.leaguestat.com/pwhl/240x240/",
            .data$player_id,
            ".jpg"
          ),
          regular_season = ifelse(season_id == 1, TRUE, FALSE),
          season = season,
          player_id = as.numeric(player_id),
          team_id = as.numeric(team_id$team_id),
          team = team
        ) %>%
        dplyr::relocate("team_id", .after = player_id) %>%
        dplyr::relocate("season", .after = team_id)
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  return(roster_data)
}

#' @title  **PWHL Schedule**
#' @description PWHL Schedule lookup
#'
#' @param season Season (YYYY) to pull the schedule from, the concluding year in XXXX-YY format
#' @return A data frame with schedule data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_schedule(season = 2023))
#' }

pwhl_schedule <- function(season, game_type = "regular") {

  seasons <- pwhl_season_id() %>%
    dplyr::filter(season_yr == season, game_type_label == game_type)

  season_id <- seasons$season_id

  base_url = glue::glue(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=schedule&team=-1&season={season_id}&month=-1&location=homeaway&key=694cfeed58c932ee&client_code=pwhl&site_id=2&league_id=1&division_id=-1&lang=en&callback=angular.callbacks._1"
  )
  full_url = base_url

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  callback_pattern <- "angular.callbacks._\\d+\\("
  res <- gsub(callback_pattern, "", res)
  # res <- gsub("\\}\\]\\)$", "}}]", res)
  # res <- gsub("angular.callbacks._1\\(", "", res)
  res <- gsub("}}]}]}])", "}}]}]}]", res)

  r <- res %>%
    jsonlite::parse_json()

  gm <- r[[1]]$sections[[1]]$data

  schedule_data <- data.frame()

  tryCatch(
    expr = {
      for (i in 1:length(gm)) {

        if (is.null(gm[[i]]$prop$venue_name$venueUrl)) {
          venue <-'TBD'
        } else {
          venue <- gm[[i]]$prop$venue_name$venueUrl
        }

        game_info <- data.frame(
          "game_id" = c(gm[[i]]$row$game_id),
          "season" = c(season),
          "game_date" = c(gm[[i]]$row$date_with_day),
          "game_status" = c(gm[[i]]$row$game_status),
          "home_team" = c(gm[[i]]$row$home_team_city),
          "home_team_id" = c(gm[[i]]$prop$home_team_city$teamLink),
          "away_team" = c(gm[[i]]$row$visiting_team_city),
          "away_team_id" = c(gm[[i]]$prop$visiting_team_city$teamLink),
          "home_score" = c(gm[[i]]$row$home_goal_count),
          "away_score" = c(gm[[i]]$row$visiting_goal_count),
          "venue" = c(gm[[i]]$row$venue_name),
          "venue_url" = c(venue)
        )

        schedule_data <- rbind(
          schedule_data,
          game_info
        )

      }

      schedule_data <- schedule_data %>%
        dplyr::mutate(
          winner = dplyr::case_when(
            .data$home_score == '' | .data$away_score == "-" ~ '-',
            .data$home_score > .data$away_score ~ .data$home_team,
            .data$away_score > .data$home_score ~ .data$away_team,
            .data$home_score == .data$away_score & .data$home_score != "-" ~ "Tie",
            TRUE ~ NA_character_
          ),
          season = season
        ) %>%
        dplyr::select(
          c(
              "game_id",
              "game_date",
              "game_status",
              "home_team",
              "home_team_id",
              "away_team",
              "away_team_id",
              "home_score",
              "away_score",
              "winner",
              "venue",
              "venue_url"
            )
        )
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(schedule_data)

}

#' @title  **PWHL Season IDs**
#' @description PWHL Season IDs lookup
#'
#' @return A data frame with season ID data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_season_id())
#' }

pwhl_season_id <- function(season, game_type = "regular") {

  season_id <- data.frame(
    "season_yr" = c(2024, 2024, 2024, 2025, 2025, 2025),
    "game_type_label" = c("preseason", "regular", "playoffs",
                    "preseason", "regular", "playoffs"),
    "season_id" = c(2, 1, 3, 4, 5, 6)
  )

  return(season_id)

}

#' @title  **PWHL Player Game Box Scores**
#' @description PWHL Player Box Scores
#'
#' @param game_id Game ID that you want play-by-play for
#' @return A data frame with play-by-play data from the PWHL
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   try(pwhl_player_box(game_id = 27))
#' }

pwhl_player_box <- function(game_id) {

  tryCatch(
    expr = {
      URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameSummary&game_id={game_id}&key=694cfeed58c932ee&site_id=2&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._6")

      res <- httr::RETRY("GET", URL)
      res <- res %>%
        httr::content(as = "text", encoding = "utf-8")
      callback_pattern <- "angular.callbacks._\\d+\\("
      res <- gsub(callback_pattern, "", res)
      res <- gsub("}}})", "}}}", res)

      r <- res %>% jsonlite::parse_json()

      suppressWarnings(
        expr = {

        home_goalie <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$homeTeam$goalies, function(x) unlist(x))))) %>% janitor::clean_names()
        away_goalie <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$visitingTeam$goalies, function(x) unlist(x))))) %>% janitor::clean_names()

        home_skaters <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$homeTeam$skaters, function(x) unlist(x))))) %>% janitor::clean_names()
        away_skaters <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$visitingTeam$skaters, function(x) unlist(x))))) %>% janitor::clean_names()

      })

      home_goalie$team_id <- r$homeTeam$info$id
      away_goalie$team_id <- r$visitingTeam$info$id
      home_skaters$team_id <- r$homeTeam$info$id
      away_skaters$team_id <- r$visitingTeam$info$id

      # home_skatersi

      goalies <- dplyr::bind_rows(home_goalie, away_goalie)
      skaters <- dplyr::bind_rows(home_skaters, away_skaters)
      skaters$game_id <- r$details$id
      goalies$game_id <- r$details$id

      skaters <- skaters %>%
        dplyr::select(-c("info_birth_date", "info_jersey_number", "info_player_image_url", "status")) %>%
        dplyr::mutate(league = "pwhl")

      goalies <- goalies %>%
        dplyr::select(-c("info_birth_date", "info_jersey_number", "info_player_image_url", "status")) %>%
        dplyr::mutate(starting = ifelse(starting != "1", 0, 1)) %>%
        dplyr::mutate(league = "pwhl")

      skater_cols <- c("player_id", "first_name", "last_name", "position", "goals", "assists", "points", "penalty_minutes", "plus_minus", "faceoff_attempts",
                       "faceoff_wins", "shots", "hits", "blocked_shots", "toi", "starting", "team_id", "game_id", "league")
      goalie_cols <- c("player_id", "first_name", "last_name", "position", "goals", "assists", "points", "penalty_minutes", "plus_mins", "faceoff_attempts",
                       "faceoff_wins", "toi", "shots_against", "goals_against", "saves", "starting", "team_id", "game_id", "league")
      colnames(skaters) <- skater_cols
      colnames(goalies) <- goalie_cols

      suppressWarnings(
        expr = {

        skaters <- skaters %>%
          dplyr::mutate_at(c( "goals", "assists", "points", "penalty_minutes", "plus_minus", "faceoff_attempts",
                              "faceoff_wins", "shots", "hits", "blocked_shots"), as.numeric) %>%
          dplyr::mutate(
            faceoff_losses = faceoff_attempts - faceoff_wins,
            faceoff_pct = faceoff_wins / faceoff_attempts
          ) %>%
          tidyr::separate(toi, into = c("minute", "second"), remove = FALSE) %>%
          dplyr::mutate(time_on_ice = round((as.numeric(minute) * 60 + as.numeric(second)) / 60, 1)) %>%
          dplyr::select(c("player_id", "first_name", "last_name", "position", "team_id", "game_id", "league", "toi", "time_on_ice",
                          "goals", "assists", "points", "shots", "hits", "blocked_shots",
                          "penalty_minutes", "plus_minus", "faceoff_attempts", "faceoff_wins", "faceoff_losses", "faceoff_pct", "starting"))

        goalies <- goalies %>%
          dplyr::mutate_at(c( "goals", "assists", "points", "penalty_minutes", "faceoff_attempts",
                              "faceoff_wins", "saves", "shots_against",
                              "goals_against"), as.numeric) %>%
          dplyr::mutate(
            faceoff_losses = faceoff_attempts - faceoff_wins,
            faceoff_pct = faceoff_wins / faceoff_attempts
          ) %>%
          tidyr::separate(toi, into = c("minute", "second"), remove = FALSE) %>%
          dplyr::mutate(time_on_ice = round((as.numeric(minute) * 60 + as.numeric(second)) / 60, 1)) %>%
          dplyr::select(c("player_id", "first_name", "last_name", "position", "team_id", "game_id", "league", "toi", "time_on_ice",
                          "saves", "goals_against", "shots_against",
                          "goals", "assists", "points",
                          "penalty_minutes", "faceoff_attempts", "faceoff_wins", "faceoff_losses", "faceoff_pct", "starting"))

        })

      player_box <- c(list(skaters), list(goalies))
      names(player_box) <- c("skaters", "goalies")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no player box data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(player_box)
}