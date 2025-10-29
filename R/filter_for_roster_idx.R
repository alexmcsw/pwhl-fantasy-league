library(magrittr)

#' @title  **Filter for PWHL Fantasy Rosters**
#' @description Filter all players for those in PWHL fantasy rosters
#'
#' @param all_teams data.frame of the PWHL players
#' @param roster_idx List of Fantasy rosters indices
#' @return A data frame of the fantasy roster players
#' @import magrittr
#' @export

filter_for_roster_idx <- function(
  all_teams,
  roster_idx
) {
  all_teams %>%
    filter(
      player_id %in% roster_idx
    )
}
