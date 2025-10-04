filter_for_roster_idx <- function(
    roster_idx
) {
    all_teams %>% filter(
        player_id %in% roster_idx
    ) |> select(
        c(
            player_headshot,
            player_name,
            primary_hand,
            dob,
            home_town,
            team.x,
            position.y,
            points,
            player_id
        )
    )
}
