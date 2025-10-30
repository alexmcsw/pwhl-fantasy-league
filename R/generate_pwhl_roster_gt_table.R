library(dplyr)
library(gt)

#" @title  **Generate PWHL Roster GT Table**
#" @description Generate the GT table for the PWHL Rosters page
#"
#" @param team_label PWHL team label
#" @param all_teams All PWHL team rosters
#" @return GT table of PWHL teams roster
#" @import dplyr
#" @import gt
#" @export

generate_pwhl_roster_gt_table <- function(
    all_teams,
    team_label
) {
    return(
        all_teams |>
            filter(
                team == team_label
            ) |>
            mutate(
                group = recode(
                    position,
                    "F" = "Forwards",
                    "C" = "Forwards",
                    "LW" = "Forwards",
                    "RW" = "Forwards",
                    "D" = "Defenders",
                    "LD" = "Defenders",
                    "RD" = "Defenders",
                    "G" = "Goalies",
                )
            ) |>
            select(
                c(
                    "group",
                    "jersey_number",
                    "player_headshot",
                    "player_name",
                    "position",
                    "primary_hand",
                    "dob",
                    "home_town"
                )
            ) |>
            rename(
                c(
                    "#" = "jersey_number",
                    Headshot = "player_headshot",
                    Name = "player_name",
                    Pos = "position",
                    Shoots = "primary_hand",
                    DOB = "dob",
                    "Home town" = "home_town"
                )
            ) |>
            gt(
                groupname_col = "group"
            ) |>
            fmt_image(
                columns = "Headshot",
                width = "50px",
                height = "50px"
            ) |>
            opt_stylize(style = 1)
    )
}
