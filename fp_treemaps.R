easypackages::packages("treemapify", "tidyverse", "nflreadr", "janitor", "glue", "ffsimulator", prompt =F)


pbp = nflreadr::load_pbp(seasons = CURRENT_SEASON) %>% mutate(season = as.numeric(season))


ff_opp_23 = ffopportunity::ep_build(season = CURRENT_SEASON)
ff_opp_23 = ff_opp_23$ep_weekly
#ff_opp = load_ff_opportunity(CURRENT_SEASON)
teams = load_teams(current = F)
nflreadr::load_schedules()
game_info = weekly_game_info_nfl %>% 
  group_by(game_id) %>% 
  mutate(game_label = paste(unique(team), sep = ", ", collapse = " vs. ")) %>% 
  ungroup() %>%
  select(team, game_label) #FROM 4for4 SIM OWNERSHIP MODEL
  



showdown_teams = c("PHI", "TB", "PIT", "LV", "NYG", "SF", "LAR", "LA", "CIN")


heatmap_df = ff_opp_23 %>%
  filter((season == CURRENT_SEASON & week <= CURRENT_WEEK )) %>%
  select(
    player_id, week, name = full_name, pos = position, team = posteam, points = total_fantasy_points, season
  ) %>%
  arrange(player_id, desc(season), desc(week)) %>%
  group_by(player_id) %>%
  mutate(pt_change = (points - lag(points))/lag(points)-1,
         sum_pts = sum(points)/n()) %>%
  group_by(team) %>%
  mutate(tm_points = sum(points)/n_distinct(week)) %>%
  group_by(team, week) %>%
  mutate(tm_wk_pts = sum(points)) %>%
  left_join(teams %>% select(team = team_abbr, division = team_division, conf = team_conf)) %>%
  ungroup() %>%
  left_join(game_info) %>%
  group_by(game_label) %>%
  mutate(game_pts = sum(points)) %>%
  filter(!is.na(player_id), season == CURRENT_SEASON, week == CURRENT_WEEK, (points > 3 | sum_pts > 3), !team %in% showdown_teams) %>%
  mutate(initials = map_chr(strsplit(name, " |-"), 
                            ~ str_c(toupper(substr(., 1, 1)), collapse = ""))) %>%
  arrange(game_label, team, -tm_points, -sum_pts, -points)


write_csv(heatmap_df, glue("scoring_heatmap_{CURRENT_SEASON}_{CURRENT_WEEK}.csv"))

# Generate treemap
heatmap_df %>% 
  arrange(game_label, -tm_wk_pts, -points) %>%
  mutate(
    name = reorder(name, sum_pts),
  ) %>%
  #filter(conf == "AFC") %>%
  ggplot(aes(area = sum_pts, fill = points, 
                      label  = name,
                 #label = paste0("Player: ", name, "\nPoints: ", points, "\nChange: ", round(pt_change, 2)),
                 subgroup = team, 
                 subgroup2 = game_label)) +
    geom_treemap(layout = "scol") +
    geom_treemap_subgroup_border(colour = "black", size = 6, layout = "scol",) +
    geom_treemap_subgroup_text(place = "top", grow = F, size = 9, layout = "scol") +
    geom_treemap_text(
      fontface = "italic", colour = "black", size = 8, 
      place = "center", reflow = T, layout = "scol") +
    scale_fill_gradient2(low = "red", high = "dark green", mid = "grey", 
                         na.value = "grey", midpoint = mean(heatmap_df$points)) +
    theme_dark()+
    theme(plot.title = element_text(color = "grey"),
      legend.position = "none", 
      plot.background = element_rect(fill = "black")
      ) +
    ggtitle(paste0("Week ",CURRENT_WEEK))


