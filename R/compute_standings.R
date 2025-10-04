compute_standings <- function(
    team_rosters
) {
    standings <- data.frame(
        team_names=names(team_rosters)
    )

    standings$points <- apply(
        standings,
        1,
        function(x) team_rosters[x][[1]]$points %>% as.numeric %>% sum
    )

    standings <- standings %>% arrange(
        desc(
            points
        )
    )
}