filter_for_roster_idx <- function(
    all_teams,
    roster_idx
) {
    all_teams %>% filter(
        player_id %in% roster_idx
    )
}
