library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

################################################################################
# Constants which change set from set to set.
mythic_upgrade_rate <- 1/8

# Constants.
gold_to_gem_ratio <- 0.2
gem_to_gold_ratio <- 5
pack_cost_gold <- 1000
pack_cost_gem <- 200

# Pack constants
rare_wc_rate <- 1/30
mythic_wc_rate <- 1/30
base_nr_rares_per_pack <- (1 - mythic_upgrade_rate) * (1 - (rare_wc_rate + mythic_wc_rate))

# Wildcard track progress when opening packs.
wildcard_track_rare_wcs <- 4/30
base_nr_rare_wcs_per_pack <- rare_wc_rate + wildcard_track_rare_wcs

# Golden packs.
mythic_upgrade_rate_golden <- 1/6
nr_rares_golden <- 5  * (1 - mythic_upgrade_rate_golden)
golden_pack_opening_rate <- 1/10
golden_pack_wildcard_track_rare_wc <- wildcard_track_rare_wcs * golden_pack_opening_rate

# Total number of rares + rare WCs.
total_nr_rares <- base_nr_rares_per_pack + base_nr_rare_wcs_per_pack
total_nr_rares_golden <- total_nr_rares + nr_rares_golden * golden_pack_opening_rate + golden_pack_wildcard_track_rare_wc

################################################################################
# Functions for calculating EV.
calc_ev_negBinom <- function(df, win_rate, avg_nr_new_rares, play_in_point_value, gem_to_gold_ratio_arg = gem_to_gold_ratio) {
    loss_end_condition <- unique(df$losses) - 1
    win_end_condition <- df |> filter(stopping_condition == "yes") |> pull(wins)
    
    df |>
        mutate(
            pr = 
                if_else(
                    stopping_condition == "no",
                    dnbinom(wins, losses, 1 - win_rate),
                    sum(dnbinom(0:loss_end_condition, win_end_condition, win_rate))
                ),
            pr_gold = pr * net_gold,
            pr_gems = pr * net_gems,
            pr_rares = pr * nr_rares_from_packs,
            pr_play_in_point = pr * play_in_points,
            avg_nr_new_rares = avg_nr_new_rares,
            win_rate = win_rate,
            play_in_point_value = play_in_point_value
        ) |>
        group_by(name, win_rate, avg_nr_new_rares, play_in_point_value) |>
        summarise(
            ev_gold = sum(pr_gold),
            ev_gems = sum(pr_gems),
            ev_rares = sum(pr_rares),
            ev_play_in_point = sum(play_in_points)
        ) |>
        ungroup() |>
        mutate(
            play_in_points_in_gems = ev_play_in_point * play_in_point_value,
            ev_gold = (ev_gold + play_in_points_in_gems) * gem_to_gold_ratio_arg,
            ev_gems = (ev_gems + play_in_points_in_gems),
            ev_rares = ev_rares + avg_nr_new_rares,
            ratio_gold_per_rare = -ev_gold / ev_rares,
            ratio_gems_per_rare = -ev_gems / ev_rares
        )
}

calc_ev_binom <- function(df, win_rate, avg_nr_new_rares, play_in_point_value, gem_to_gold_ratio_arg = gem_to_gold_ratio) {
    n_trials <- unique(df$nr_trials)
    
    df |>
        mutate(
            pr = dbinom(wins, n_trials, win_rate),
            pr_gold = pr * net_gold,
            pr_gems = pr * net_gems,
            pr_rares = pr * nr_rares_from_packs,
            pr_play_in_point = pr * play_in_points,
            avg_nr_new_rares = avg_nr_new_rares,
            win_rate = win_rate,
            play_in_point_value = play_in_point_value
        ) |>
        group_by(name, win_rate, avg_nr_new_rares, play_in_point_value) |>
        summarise(
            ev_gold = sum(pr_gold),
            ev_gems = sum(pr_gems),
            ev_rares = sum(pr_rares),
            ev_play_in_point = sum(pr_play_in_point)
        ) |>
        ungroup() |>
        mutate(
            play_in_points_in_gems = ev_play_in_point * play_in_point_value,
            ev_gold = (ev_gold + play_in_points_in_gems) * gem_to_gold_ratio_arg,
            ev_gems = (ev_gems + play_in_points_in_gems),
            ev_rares = ev_rares + avg_nr_new_rares,
            ratio_gold_per_rare = -ev_gold / ev_rares,
            ratio_gems_per_rare = -ev_gems / ev_rares
        )
}

################################################################################
event_draft_q <-
    tibble(
        name = "Draft (Quick)",
        entry_fee_gold = 5000,
        entry_fee_gems = 750,
        stopping_condition = c("no", "no", "no", "no", "no", "no", "no", "yes"),
        wins = 0:7,
        losses = 3,
        reward_gems = c(50, 100, 200, 300, 450, 650, 850, 950),
        reward_packs = c(1.2, 1.22, 1.24, 1.26, 1.3, 1.35, 1.4, 2),
        play_in_points = 0
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_draft_quick <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 4, 1),
        play_in_points_value = 0
    )

# Calculating the EV.
ev_draft_quick <-
    pmap(
        list(args_draft_quick$win_rate, args_draft_quick$avg_nr_new_rares, args_draft_quick$play_in_points_value),
        calc_ev_negBinom,
        df = event_draft_q
    ) |>
    bind_rows()

################################################################################
event_draft_p <-
    tibble(
        name = "Draft (Premier)",
        entry_fee_gold = 10000,
        entry_fee_gems = 1500,
        stopping_condition = c("no", "no", "no", "no", "no", "no", "no", "yes"),
        wins = 0:7,
        losses = 3,
        reward_gems = c(50, 100, 250, 1000, 1400, 1600, 1800, 2200),
        reward_packs = c(1, 1, 2, 2, 3, 4, 5, 6),
        play_in_points = 0
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_draft_p <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 8, 1),
        play_in_points_value = 0
    )

# Calculating the EV.
ev_draft_p <-
    pmap(
        list(args_draft_p$win_rate, args_draft_p$avg_nr_new_rares, args_draft_p$play_in_points_value),
        calc_ev_negBinom,
        df = event_draft_p
    ) |>
    bind_rows()

################################################################################
event_draft_t <-
    tibble(
        name = "Draft (Traditional)",
        entry_fee_gold = 10000,
        entry_fee_gems = 1500,
        wins = 0:3,
        nr_trials = 3,
        reward_gems = c(100, 250, 1000, 2500),
        reward_packs = c(1, 1, 3, 6),
        play_in_points = c(0, 0, 0, 2)
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_draft_t <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 8, 1),
        play_in_points_value = c(45, 55, 65, 75, 105, 210)
    ) |>
    mutate(
        win_rate_bo3 = win_rate * win_rate + (1 - win_rate) * win_rate * win_rate + win_rate * (1 - win_rate) * win_rate
    )

# Calculating the EV.
ev_draft_t <-
    pmap(
        list(args_draft_t$win_rate_bo3, args_draft_t$avg_nr_new_rares, args_draft_t$play_in_points_value),
        calc_ev_binom,
        df = event_draft_t
    ) |>
    bind_rows() |>
    rename(win_rate_bo3 = win_rate) |>
    left_join(distinct(select(args_draft_t, win_rate, win_rate_bo3)), by = "win_rate_bo3")

################################################################################
event_draft_p2 <-
    tibble(
        name = "Draft (P2)",
        entry_fee_gold = 7500,
        entry_fee_gems = 900,
        wins = 0:4,
        losses = 2,
        reward_gems = c(50, 150, 800, 1000, 1300),
        reward_packs = c(1, 1, 1, 2, 3),
        play_in_points = 0,
        stopping_condition = c("no", "no", "no", "no", "yes")
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_draft_p2 <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 8, 1),
        play_in_points_value = 0
    )

# Calculating the EV.
ev_draft_p2 <-
    pmap(
        list(args_draft_p2$win_rate, args_draft_p2$avg_nr_new_rares, args_draft_p2$play_in_points_value),
        calc_ev_negBinom,
        df = event_draft_p2
    ) |>
    bind_rows()

################################################################################
event_sealed <-
    tibble(
        name = "Sealed",
        entry_fee_gold = NA,
        entry_fee_gems = 2000,
        wins = 0:7,
        losses = 3,
        reward_gems = c(200, 400, 600, 1200, 1400, 1600, 2000, 2200),
        reward_packs = 3,
        play_in_points = 0,
        stopping_condition = c("no", "no", "no", "no", "no", "no", "no", "yes")
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_sealed <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 7, 1),
        play_in_points_value = 0
    )

# Calculating the EV.
ev_sealed <-
    pmap(
        list(args_sealed$win_rate, args_sealed$avg_nr_new_rares, args_sealed$play_in_points_value),
        calc_ev_negBinom,
        df = event_sealed
    ) |>
    bind_rows()

################################################################################
event_sealed_t <-
    tibble(
        name = "Sealed (Traditional)",
        entry_fee_gold = NA,
        entry_fee_gems = 2000,
        wins = 0:4,
        losses = 2,
        reward_gems = c(200, 500, 1200, 1800, 2200),
        reward_packs = 3,
        play_in_points = 0,
        stopping_condition = c("no", "no", "no", "no", "yes")
    ) |>
    mutate(
        nr_rares_from_packs = reward_packs * total_nr_rares,
        net_gems = reward_gems - entry_fee_gems,
        net_gold = reward_gems - entry_fee_gold * gold_to_gem_ratio
    )

# Arguments
args_sealed_t <-
    expand_grid(
        win_rate = seq(0.35, 0.75, 0.05),
        avg_nr_new_rares = seq(0, 7, 1),
        play_in_points_value = 0
    ) |>
    mutate(
        win_rate_bo3 = win_rate * win_rate + (1 - win_rate) * win_rate * win_rate + win_rate * (1 - win_rate) * win_rate
    )

# Calculating the EV.
ev_sealed_t <-
    pmap(
        list(args_sealed_t$win_rate_bo3, args_sealed_t$avg_nr_new_rares, args_sealed_t$play_in_points_value),
        calc_ev_negBinom,
        df = event_sealed_t
    ) |>
    bind_rows() |>
    rename(win_rate_bo3 = win_rate) |>
    left_join(distinct(select(args_sealed_t, win_rate, win_rate_bo3)), by = "win_rate_bo3")

################################################################################
# Compare events
ev_limited <-
    bind_rows(
        ev_draft_p, ev_draft_p2, ev_draft_quick, ev_sealed, ev_draft_t, ev_sealed_t
    )

store_pack_gold_rare_ratio <- pack_cost_gold / total_nr_rares
store_pack_gem_rare_ratio <- pack_cost_gem / total_nr_rares
golden_pack_gold_rare_ratio <- pack_cost_gold / total_nr_rares_golden
golden_pack_gem_rare_ratio <- pack_cost_gem / total_nr_rares_golden

# Graph EV without packs.
ev_limited_no_packs <-
    ev_limited |>
    filter(name != "Draft (Traditional)" | play_in_point_value %in% c(45, 210)) |>
    distinct(name, win_rate, play_in_point_value, ev_gold, ev_gems) |>
    mutate(name = paste0(name, " (Play-in point = ", play_in_point_value, ")"))

graph_ev_gold_no_packs <-
    ev_limited_no_packs |>
    filter(!(name %in% c("Sealed (Play-in point = 0)", "Sealed (Traditional) (Play-in point = 0)"))) |>
    ggplot(aes(x = win_rate, y = ev_gold)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name, group = name)) +
    theme_bw() +
    labs(
        x = "Win rate",
        y = "EV (paid in gold)",
        title = "Comparison of EV for limited events in MTG Arena (OM1) w/out considering packs",
        color = "Event"
    )
ggsave(
    here("om1_graphs", "graph_ev_gold_no_packs.png"),
    graph_ev_gold_no_packs,
    height = 8,
    width = 12
)

graph_ev_gems_no_packs <-
    ggplot(ev_limited_no_packs, aes(x = win_rate, y = ev_gems)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name, group = name)) +
    theme_bw() +
    labs(
        x = "Win rate",
        y = "EV (paid in gems)",
        title = "Comparison of EV for limited events in MTG Arena (OM1) w/out considering packs",
        color = "Event"
    )
ggsave(
    here("om1_graphs", "graph_ev_gems_no_packs.png"),
    graph_ev_gems_no_packs,
    height = 8,
    width = 12
)

# Graph EV with packs.
ev_limited_with_packs <-
    map(
        list(1, 3, 5, 7),
        function(df, nr_rares_drafted) {
            df |>
                filter(name != "Draft (Traditional)" | play_in_point_value %in% c(45, 210)) |>
                filter(
                    case_when(
                        nr_rares_drafted >= 0 & nr_rares_drafted <= 4 ~ avg_nr_new_rares == nr_rares_drafted,
                        nr_rares_drafted >= 5 & nr_rares_drafted <= 6 ~
                            avg_nr_new_rares == nr_rares_drafted |
                            (name == "Draft (Quick)" & avg_nr_new_rares == 4),
                        nr_rares_drafted >= 7 ~
                            avg_nr_new_rares == nr_rares_drafted |
                            (name == "Draft (Quick)" & avg_nr_new_rares == 4) |
                            (name %in% c("Sealed", "Sealed (Traditional)") & avg_nr_new_rares == 7)
                    )
                ) |>
                mutate(
                    name = paste0(name, " (Play-in point = ", play_in_point_value, ")"),
                    avg_nr_new_rares = nr_rares_drafted
                )
        },
        df = ev_limited
    ) |>
    bind_rows()

graph_ev_gold_packs <-
    ev_limited_with_packs |>
    filter(!(name %in% c("Sealed (Play-in point = 0)", "Sealed (Traditional) (Play-in point = 0)"))) |>
    filter(win_rate > 0.4 & win_rate <= 0.65) |>
    ggplot(aes(x = win_rate, y = ratio_gold_per_rare)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name, group = name)) +
    facet_wrap(~avg_nr_new_rares, scale = "free_y") +
    theme_bw() +
    geom_hline(yintercept = store_pack_gold_rare_ratio, color = "red") +
    geom_hline(yintercept = golden_pack_gold_rare_ratio, color = "black") +
    labs(
        x = "Win rate",
        y = "Ratio of gold to rares (how much one rare costs in gold)",
        title = "Comparison of EV for limited events in MTG Arena (OM1) considering packs",
        color = "Event"
    )
ggsave(
    here("om1_graphs", "graph_ev_gold_packs.png"),
    graph_ev_gold_packs,
    height = 8,
    width = 12
)

# Graph EV with packs and sealed is fixed.
ev_limited_with_packs_fixed_sealed <-
    map(
        list(1, 3, 5, 7),
        function(df, nr_rares_drafted) {
            df |>
                filter(name != "Draft (Traditional)" | play_in_point_value %in% c(45, 210)) |>
                filter(!(name %in% c("Sealed", "Sealed (Traditional)")) | avg_nr_new_rares == 7) |>
                filter(
                    case_when(
                        nr_rares_drafted >= 0 & nr_rares_drafted <= 4 ~
                            avg_nr_new_rares == nr_rares_drafted |
                            name %in% c("Sealed", "Sealed (Traditional)"),
                        nr_rares_drafted >= 5 ~
                            avg_nr_new_rares == nr_rares_drafted |
                            (name == "Draft (Quick)" & avg_nr_new_rares == 4) |
                            name %in% c("Sealed", "Sealed (Traditional)")
                    )
                ) |>
                mutate(
                    name = paste0(name, " (Play-in point = ", play_in_point_value, ")"),
                    avg_nr_new_rares = nr_rares_drafted
                )
        },
        df = ev_limited
    ) |>
    bind_rows()

graph_ev_gems_packs <-
    ev_limited_with_packs_fixed_sealed |>
    filter(win_rate > 0.4 & win_rate <= 0.65) |>
    ggplot(aes(x = win_rate, y = ratio_gems_per_rare)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name, group = name)) +
    facet_wrap(~avg_nr_new_rares, scale = "free_y") +
    theme_bw() +
    geom_hline(yintercept = store_pack_gem_rare_ratio, color = "red") +
    geom_hline(yintercept = golden_pack_gem_rare_ratio, color = "black") +
    labs(
        x = "Win rate",
        y = "Ratio of gems to rares (how much one rare costs in gems)",
        title = "Comparison of EV for limited events in MTG Arena (OM1) considering packs",
        color = "Event"
    )
ggsave(
    here("om1_graphs", "graph_ev_gems_packs.png"),
    graph_ev_gems_packs,
    height = 8,
    width = 12
)

################################################################################
# Graph trade-off in win rate vs. rares.
# Arguments
args_tradeoff_draft_q <-
    expand_grid(
        win_rate = seq(0.01, 0.99, 0.0025),
        avg_nr_new_rares = seq(0, 4, 1),
        play_in_points_value = 0
    )

args_tradeoff_draft_pp2 <-
    expand_grid(
        win_rate = seq(0.01, 0.99, 0.0025),
        avg_nr_new_rares = seq(0, 8, 1),
        play_in_points_value = 0
    )

args_tradeoff_draft_t <-
    expand_grid(
        win_rate = seq(0.01, 0.99, 0.0025),
        avg_nr_new_rares = seq(0, 8, 1),
        play_in_points_value = 210
    ) |>
    mutate(
        win_rate_bo3 =
            win_rate * win_rate +
            (1 - win_rate) * win_rate * win_rate +
            win_rate * (1 - win_rate) * win_rate
    )

# Calculating the EV.
ev_tradeoff_draft_t_45 <-
    pmap(
        list(
            args_tradeoff_draft_t$win_rate_bo3,
            args_tradeoff_draft_t$avg_nr_new_rares,
            args_tradeoff_draft_t$play_in_points_value
        ),
        calc_ev_binom,
        df = event_draft_t
    ) |>
    bind_rows() |>
    rename(win_rate_bo3 = win_rate) |>
    left_join(
        distinct(select(args_tradeoff_draft_t, win_rate, win_rate_bo3)),
        by = "win_rate_bo3"
    ) |>
    select(win_rate, avg_nr_new_rares, ratio_gold_per_rare, ratio_gems_per_rare)









a4 <- ev_tradeoff_draft_t_45 |> filter(win_rate == 0.6, avg_nr_new_rares == 3)

a5 <-
    ev_tradeoff_draft_t_45 |>
    filter(ratio_gold_per_rare < a$ratio_gold_per_rare & avg_nr_new_rares != a$avg_nr_new_rares) |>
    mutate(
        diff_ratio = abs(ratio_gold_per_rare - a$ratio_gold_per_rare),
        diff_win_rate = win_rate - a$win_rate
    ) |>
    group_by(avg_nr_new_rares) |>
    filter(diff_ratio == min(diff_ratio)) |>
    ungroup()
    
a6 <-
    bind_rows(a, a2)

ggplot(a3, aes(x = avg_nr_new_rares, y = diff_win_rate)) +
    geom_bar(stat = "identity") +
    theme_bw()

ggplot(a6, aes(x = avg_nr_new_rares, y = diff_win_rate)) +
    geom_bar(stat = "identity") +
    theme_bw()

