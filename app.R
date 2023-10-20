# Fantasy Football Player App
library(shiny)
library(tidyverse)
library(knitr)
library(nflfastR)
library(ggrepel)
library(ggthemes)
library(gt)
library(gtExtras)
library(DT) 

# Load Fantasy App Data ----
load(file = "ff_data_week_6.RData")

# Fantasy Football Shiny App ----
stats_yearly <- stats_yearly %>% arrange(desc(season), player_display_name)
stats_weekly <- stats_weekly %>% arrange(desc(season), player_display_name)

# Server ----
server <- function(input, output, session) {
  
  # Year filter sync
  selected_season <- reactiveVal(max(stats_yearly$season))
  
  observeEvent(input$year1, {
    updateSelectInput(session, "year1", selected=input$year1)
    selected_season(input$year1)
  })
  
  # Position filter sync
  selected_position <- reactiveVal("QB")
  
  observeEvent(input$position1, {
    updateSelectInput(session, "position1", selected=input$position1)
    selected_position(input$position1)
  })
  
  # Player filter sync
  selected_player <- reactiveVal(stats_yearly$player_display_name[1])
  
  observeEvent(input$player1, {
    updateSelectInput(session, "player1", selected=input$player1)
    selected_player(input$player1)
  })
  
  # Update the available players based on the selected position
  observeEvent(selected_position(), {
    players <- stats_yearly %>%
      filter(position == selected_position()) %>%
      pull(player_display_name) %>%
      unique()
    updateSelectInput(session, "player1", choices = players, selected = players[1])
  })
  
  # Update the available seasons based on the selected player
  observeEvent(selected_player(), {
    seasons <- stats_yearly %>%
      filter(player_display_name == selected_player()) %>%
      pull(season) %>%
      unique()
    updateSelectInput(session, "year1", choices = seasons, selected = max(seasons))
  })
  
  # Reactive player
  player <- reactive({
    stats_yearly %>%
      filter(player_display_name == selected_player() & season == selected_season()) %>%
      select(player_display_name, position, recent_team) %>%
      left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))
  })
  
  max_week <- reactive({
    stats_weekly %>%
      filter(season == selected_season()) %>%
      select(week) %>%
      max()
  })
  
  pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
               "#d9f0d3", "#7fbf7b", "#1b7837")
  
  pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                   "#e7d4e8", "#af8dc3", "#762a83")
  
  finishes_pct_tbl <- reactive({
    stats_weekly %>%
      filter(position == selected_position() & season == selected_season()) %>%
      arrange(week) %>%
      group_by(player_display_name) %>%
      mutate(average_points = round(cummean(total_points),2),
             run_total_points = round(cumsum(total_points),2)) %>%
      fill(run_total_points, .direction = "down") %>%
      replace_na(list(total_points = 0)) %>%
      fill(total_points, average_points) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
      ungroup() %>%
      group_by(week) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first")),
             week_rank = if_else(total_points == 0,
                                 NA, rank(-total_points,ties.method = "first"))) %>%
      ungroup() %>%
      select(player_display_name, week, total_points, week_rank, average_points, pos_rank)
  })
  
  
  # week and average points by week
  output$plot1 <- renderPlot({
    all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                    position = player()$position,
                                    week = 1:max_week())
    
    stats_weekly %>%
      filter(position == selected_position() & season == selected_season()) %>%
      arrange(week) %>%
      group_by(player_display_name) %>%
      mutate(average_points = round(cummean(total_points),2)) %>%
      mutate(run_total_points = round(cumsum(total_points),2)) %>%
      ungroup() %>%
      complete(all_combinations) %>%
      group_by(player_display_name) %>%
      fill(run_total_points, .direction = "down") %>%
      replace_na(list(total_points = 0)) %>%
      fill(total_points, average_points) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
      ungroup() %>%
      group_by(week) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
      ungroup() %>%
      filter(player_display_name == player()$player_display_name) %>%
      select(player_display_name, position, week,
             total_points, average_points, pos_rank) %>%
      ggplot(aes(week)) +
      geom_line(aes(y = total_points, color = player()$team_color), linetype = "dashed") +
      geom_line(aes(y = average_points, color = player()$team_color2), linetype = "solid") +
      geom_point(aes(y = total_points, color = player()$team_color)) +
      scale_color_manual(
        name = "",
        values = c(player()$team_color, player()$team_color2),
        labels = c("Week Points", "Average Points"),
        guide = guide_legend(override.aes = list(linetype = c("dashed", "solid"),
                                                 color = c(player()$team_color, player()$team_color2)))) +
      labs(title = "Week and Average Points by Week",
           subtitle = "Week points are the points scored in a particular week",
           caption = "Figure: @MambaMetrics | Data: @nflfastR") +
      ylab("Week Points") +
      xlab("Week") +
      scale_y_continuous(
        breaks = seq(0, max(stats_weekly$total_points), 5),
        sec.axis = sec_axis(~., breaks = seq(0, max(stats_weekly$total_points), 5),
                            name = "Average Points", labels = function(x) round(x, 2))) +
      scale_x_continuous(breaks = seq(min(stats_weekly$week), max(stats_weekly$week), 1)) +
      theme_bw() + 
      theme(legend.position="bottom")
    
  })
  
  # fantasy performance by week
  output$plot2 <- render_gt({
    all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                    position = selected_position(),
                                    week = 1:max_week())
    
    stats_weekly %>%
      filter(position == selected_position() & season == selected_season()) %>%
      arrange(week) %>%
      group_by(player_display_name) %>%
      mutate(average_points = round(cummean(total_points),2),
             run_total_points = round(cumsum(total_points),2)) %>%
      ungroup() %>%
      complete(all_combinations) %>%
      group_by(player_display_name) %>%
      fill(run_total_points, .direction = "down") %>%
      replace_na(list(total_points = 0)) %>%
      fill(total_points, average_points) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first"))) %>%
      ungroup() %>%
      group_by(week) %>%
      mutate(pos_rank = round(rank(-run_total_points, ties.method = "first")),
             week_rank = if_else(total_points == 0,
                                 NA, rank(-total_points, ties.method = "first")),
             total_points = if_else(total_points == 0, NA, total_points)) %>%
      ungroup() %>%
      filter(player_display_name == player()$player_display_name) %>%
      select(week, total_points, week_rank, average_points, pos_rank) %>%
      gt() %>%
      gt_theme_538() %>%
      tab_options(
        heading.align = "center",
      ) %>%
      tab_header(
        title = "Weekly Fantasy Performance",
        subtitle = "Rolling Position Rank by Total Points",
      ) %>%
      cols_align(
        "center"
      ) %>%
      cols_label(
        week = "Week",
        total_points = "Weekly Points",
        week_rank = "Weekly Points Rank",
        average_points = "Rolling Average Points",
        pos_rank = "Position Rank",
      ) %>%
      fmt_number(
        columns = c(total_points, average_points),
        decimals = 1
      ) %>%
      cols_width(
        columns = everything() ~ px(80)
      ) %>%
      gt_color_rows(
        total_points, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(total_points), na.rm = T)
      ) %>%
      gt_color_rows(
        week_rank, 
        palette = pal_hex_rev, 
        domain = range(finishes_pct_tbl() %>% pull(week_rank), na.rm = T)
      ) %>%
      gt_color_rows(
        average_points, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(average_points), na.rm = T)
      ) %>%
      gt_color_rows(
        pos_rank, 
        palette = pal_hex_rev, 
        domain = range(finishes_pct_tbl() %>% pull(pos_rank), na.rm = T)
      ) %>%
      tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
      )
  })
  
  # opportunities totals
  output$plot3 <- renderPlot({
    
    # opportunities by position
    if (selected_position() == "QB") {
      
      hvo_qb %>%
        filter(hvo_type == "hvo_pct" &
                 season == selected_season()) %>%
        arrange(desc(attempts)) %>%
        head(20) %>%
        ggplot(aes(attempts, reorder(player_name, attempts), fill = rush_attempts)) +
        geom_col() +
        scale_x_continuous() +
        scale_fill_gradientn(colors = pal_hex) +
        labs(x = "Pass Attempts",
             y = "",
             title = paste0("Total Pass Attempts (Top 20 QB)"),
             caption = "Figure: @MambaMetrics | Data: @nflfastR",
             fill = "Rush Att.")+
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        theme_dark()
      
      
    } else if (selected_position() == "RB") {
      
      hvo_rb %>%
        filter(hvo_type == "hvo_pct" & 
                 season == selected_season()) %>%
        arrange(desc(total_touches)) %>%
        head(40) %>%
        ggplot(aes(total_touches, reorder(player_name, total_touches), fill = touch_pct)) +
        geom_col() +
        scale_x_continuous() +
        scale_fill_gradientn(colors = pal_hex, labels=scales::percent) +
        labs(x = "Total Touches",
             y = "",
             title = paste0("Total Touches (Top 40 RB)"),
             subtitle = "HVO %: carry inside the 10 yard line or catch as a percent of total touches",
             caption = "Figure: @MambaMetrics | Data: @nflfastR",
             fill = "HVO %") +
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        theme_dark()
      
    } else if (selected_position() == "WR") {
      
      hvo_wr %>%
        filter(season == selected_season()) %>%
        arrange(desc(tgt)) %>%
        head(40) %>%
        ggplot(aes(tgt, reorder(player_name, tgt), fill = adot)) +
        geom_col() +
        scale_x_continuous() +
        scale_fill_gradientn(colors = pal_hex) +
        labs(x = "Targets",
             y = "",
             title = paste0("Total Targets (Top 40 WR)"),
             caption = "Figure: @MambaMetrics | Data: @nflfastR",
             fill = "aDot") +
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        theme_dark()
      
    } else if (selected_position() == "TE") {
      
      hvo_te %>%
        filter(season == selected_season()) %>%
        arrange(desc(tgt)) %>%
        head(20) %>%
        ggplot(aes(tgt, reorder(player_name, tgt), fill = adot)) +
        geom_col() +
        scale_x_continuous() +
        scale_fill_gradientn(colors = pal_hex) +
        labs(x = "Targets",
             y = "",
             title = paste0("Total Targets (Top 20 TE)"),
             caption = "Figure: @MambaMetrics | Data: @nflfastR",
             fill = "aDot") +
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank()) +
        theme_dark()
      
    } else {
      # Handle the case when the player's position is not recognized
      cat("Selected player's position is not recognized.")
    }
    
  })
  
  # positional tiers
  output$plot4 <- render_gt({
    
    vorp_tiers_final %>%
      filter(position == selected_position() & season == selected_season()) %>%
      select(player_display_name, vorp, tier) %>%
      arrange(tier, desc(vorp)) %>%
      gt() %>%
      gt_theme_538() %>%
      tab_options(
        heading.align = "center",
      ) %>%
      tab_header(
        title = "Position Tiers",
        subtitle = "Tiers for Positive Value Players",
      ) %>%
      cols_align(
        "center"
      ) %>%
      cols_label(
        player_display_name = "Player",
        vorp = "VORP",
        tier = "Tier"
      ) %>%
      fmt_number(
        columns = vorp,
        decimals = 1
      ) %>%
      tab_style(
        style = cell_fill(color = "#7fbf7b"),
        locations = cells_body(
          rows = tier == "Tier 1")
      ) %>%
      tab_style(
        style = cell_fill(color = "#d9f0d3"),
        locations = cells_body(
          rows = tier == "Tier 2")
      ) %>%
      tab_style(
        style = cell_fill(color = "#f7f7f7"),
        locations = cells_body(
          rows = tier == "Tier 3")
      ) %>%
      tab_style(
        style = cell_fill(color = "#e7d4e8"),
        locations = cells_body(
          rows = tier == "Tier 4")
      ) %>%
      tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
      )
  })
  
  # stats tables
  output$plot5 <- renderDT({
    
    qb_tbl <- stats_yearly %>%
      filter(position == "QB" & season == selected_season()) %>%
      select(player_display_name, season, recent_team, position, games,
             total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
             vorp, adp, adp_pos_rank, performance_diff,
             completions, attempts, passing_yards, passing_tds, passing_air_yards,
             carries, rushing_yards, rushing_tds) %>%
      arrange(desc(total_points))
    
    rb_tbl <- stats_yearly %>%
      filter(position == "RB" & season == selected_season()) %>%
      select(player_display_name, season, recent_team, position, games,
             total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
             vorp, adp, adp_pos_rank, performance_diff,
             carries, rushing_yards, rushing_tds,
             receptions, receiving_yards, receiving_tds, targets, target_share,
             receiving_air_yards, receiving_yards_after_catch, wopr) %>%
      mutate(across(c(target_share,wopr), \(x) round(x, 3))) %>%
      arrange(desc(total_points))
    
    wr_tbl <- stats_yearly %>%
      filter(position == "WR" & season == selected_season()) %>%
      select(player_display_name, season, recent_team, position, games,
             total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
             vorp, adp, adp_pos_rank, performance_diff,
             receptions, receiving_yards, receiving_tds, targets, target_share,
             receiving_air_yards, receiving_yards_after_catch, wopr) %>%
      mutate(across(c(target_share,wopr), \(x) round(x, 3))) %>%
      arrange(desc(total_points))
    
    te_tbl <- stats_yearly %>%
      filter(position == "TE" & season == selected_season()) %>%
      select(player_display_name, season, recent_team, position, games,
             total_points, tot_pos_rank, average_points, avg_pos_rank, std_dev,
             vorp, adp, adp_pos_rank, performance_diff,
             receptions, receiving_yards, receiving_tds, targets, target_share,
             receiving_air_yards, receiving_yards_after_catch, wopr) %>%
      mutate(across(c(target_share,wopr), \(x) round(x, 3))) %>%
      arrange(desc(total_points))
    
    # opportunities by position
    if (selected_position() == "QB") {
      
      datatable(qb_tbl)
      
    } else if (selected_position() == "RB") {
      
      datatable(rb_tbl)
      
    } else if (selected_position() == "WR") {
      
      datatable(wr_tbl)
      
    } else if (selected_position() == "TE") {
      
      datatable(te_tbl)
      
    } else {
      # Handle the case when the player's position is not recognized
      cat("Selected player's position is not recognized.")
    }
    
  })
  
}


# UI ----
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("Fantasy Football Player Dashboard"),
  
  fluidRow(
    selectInput("position1", "Position",
                choices = c("QB", "RB", "WR", "TE"),
                selected = "QB"),
    selectInput("year1", "Season",
                choices = NULL,
                selected = max(stats_yearly$season))
  ),
  
  tabsetPanel(type = "tabs",
              tabPanel("Overview", fluid = T,
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("player1", "Select Player:",
                                       choices = unique(stats_yearly$player_display_name),
                                       selectize = TRUE),
                           gt_output("plot2"),
                           width = 4
                         ),
                         mainPanel(
                           fluidRow(
                             column(12, plotOutput("plot1", height = 500)),
                             h3(textOutput(""), align = "center")
                           )
                         )
                       )
              ),
              tabPanel("Opportunities", fluid = T,
                       sidebarLayout(
                         sidebarPanel(
                           gt_output("plot4"),
                           width = 4
                         ),
                         mainPanel(
                           fluidRow(
                             column(12, plotOutput("plot3", height = 750)),
                             h3(textOutput(""), align = "center")
                           )
                         )
                       )
              ),
              tabPanel("Stats", fluid = T,
                       mainPanel(
                         fluidRow(
                           column(12, DTOutput("plot5")),
                           h3(textOutput(""), align = "center")
                         )
                       )
              )
  )
)

# Run the Shiny app
shinyApp(ui, server)





