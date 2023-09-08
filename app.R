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
load("ff_player_app.RData")

# Fantasy Football Shiny App ----
stats_yearly <- stats_yearly %>% arrange(desc(season), player_display_name)
stats_weekly <- stats_weekly %>% arrange(desc(season), player_display_name)


# Server ----
server <- function(input, output, session) {
  
  # Position filter sync
  selected_position <- reactiveVal("QB")
  
  observeEvent(input$position1, {
    updateSelectInput(session, "position2", selected=input$position1)
    selected_position(input$position1)
  })
  observeEvent(input$position2, {
    updateSelectInput(session, "position1", selected=input$position2)
    selected_position(input$position2)
  })
  observeEvent(input$position2, {
    updateSelectInput(session, "position3", selected=input$position2)
    selected_position(input$position2)
  })
  
  
  # Player filter sync
  selected_player <- reactiveVal(stats_yearly$player_display_name[1])
  
  observeEvent(input$player1, {
    updateSelectInput(session, "player2", selected=input$player1)
    selected_player(input$player1)
  })
  observeEvent(input$player2, {
    updateSelectInput(session, "player1", selected=input$player2)
    selected_player(input$player2)
  })
  observeEvent(input$player2, {
    updateSelectInput(session, "player3", selected=input$player2)
    selected_player(input$player2)
  })
  
  # Update the available players based on the selected position
  observeEvent(selected_position(), {
    players <- stats_yearly %>%
      filter(position == selected_position()) %>%
      pull(player_display_name) %>%
      unique()
    updateSelectInput(session, "player1", choices = players, selected = players[1])
    updateSelectInput(session, "player2", choices = players, selected = players[1])
    updateSelectInput(session, "player3", choices = players, selected = players[1])
  })
  
  
  # Update the available seasons based on the selected player
  observeEvent(selected_player(), {
    seasons <- stats_yearly %>%
      filter(player_display_name == selected_player()) %>%
      pull(season) %>%
      unique()
    updateSelectInput(session, "year", choices = seasons, selected = max(seasons))
  })
  
  # Reactive player
  player <- reactive({
    stats_yearly %>%
      filter(player_display_name == selected_player() & season == input$year) %>%
      select(player_display_name, position, recent_team) %>%
      left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))
  })
  
  pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
               "#d9f0d3", "#7fbf7b", "#1b7837")
  
  finishes_pct_tbl <- reactive({
    stats_weekly %>%
      filter(position == selected_position()) %>%
      group_by(season, week, position) %>%
      mutate(week_rank = if_else(total_points == 0,
                                 NA, rank(-total_points, ties.method = "first"))) %>%
      ungroup() %>%
      group_by(season, recent_team, position, player_display_name) %>%
      summarise(top_6 = sum(if_else(week_rank <= 6, 1, 0), na.rm = TRUE),
                top_12 = sum(if_else(week_rank <= 12, 1, 0), na.rm = TRUE),
                top_18 = sum(if_else(week_rank <= 18, 1, 0), na.rm = TRUE),
                top_24 = sum(if_else(week_rank <= 24, 1, 0), na.rm = TRUE),
                games = n()) %>%
      ungroup() %>%
      mutate(across(top_6:top_24, na_if, 0))
  })
  
  stats_pct_tbl <- reactive({
    stats_yearly %>%
      select(season, position, vorp, std_dev,
             total_points, tot_pos_rank, average_points, avg_pos_rank,
             performance_diff) %>%
      filter(position == selected_position()) %>%
      mutate(across(vorp:performance_diff, na_if, 0))
  })
  
  # finishes_pct_tbl <- stats_weekly %>%
  #     filter(position == selected_position()) %>%
  #     group_by(season, week, position) %>%
  #     mutate(week_rank = if_else(total_points == 0,
  #                                NA, rank(-total_points, ties.method = "first"))) %>%
  #     ungroup() %>%
  #     group_by(season, recent_team, position, player_display_name) %>%
  #     summarise(top_6 = sum(if_else(week_rank <= 6, 1, 0), na.rm = TRUE),
  #               top_12 = sum(if_else(week_rank <= 12, 1, 0), na.rm = TRUE),
  #               top_18 = sum(if_else(week_rank <= 18, 1, 0), na.rm = TRUE),
  #               top_24 = sum(if_else(week_rank <= 24, 1, 0), na.rm = TRUE),
  #               games = n()) %>%
  #     ungroup() %>%
  #     mutate(across(top_6:top_24, na_if, 0))
  
  # stats_pct_tbl <- stats_yearly %>%
  #     select(season, position, vorp, std_dev,
  #            total_points, tot_pos_rank, average_points, avg_pos_rank,
  #            performance_diff) %>%
  #     filter(position == selected_position()) %>%
  #     mutate(across(vorp:performance_diff, na_if, 0))
  
  # week and average points by week
  output$plot1 <- renderPlot({
    all_combinations <- expand.grid(player_display_name = unique(stats_weekly$player_display_name),
                                    position = player()$position,
                                    week = 1:17)
    
    stats_weekly %>%
      filter(position == player()$position & season == input$year) %>%
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
  
  # fantasy performance by season
  output$plot2 <- render_gt({
    stats_yearly %>%
      filter(player_display_name == player()$player_display_name) %>%
      select(season, recent_team, games, vorp, std_dev,
             total_points, tot_pos_rank, average_points, avg_pos_rank,
             performance_diff) %>%
      arrange(season) %>%
      gt() %>%
      gt_theme_538() %>%
      tab_options(
        heading.align = "center",
      ) %>%
      tab_header(
        title = "Fantasy Performance by Season",
        subtitle = "Points and Position Rank"
      ) %>%
      cols_align(
        "center"
      ) %>%
      cols_label(
        season = "Season",
        recent_team = "Team",
        games = "Games",
        vorp = "VORP",
        std_dev = "Standard Deviation",
        total_points = "Total Points",
        tot_pos_rank = "Total Points Rank",
        average_points = "Average Points",
        avg_pos_rank = "Average Points Rank",
        performance_diff = "ADP +/-"
      ) %>%
      fmt_number(
        columns = c(total_points, average_points, std_dev),
        decimals = 1
      )  %>%
      cols_width(
        columns = c(season) ~ px(55),
        columns = c(recent_team, games, vorp) ~ px(52),
        columns = std_dev ~ px(75),
        columns = c(total_points, tot_pos_rank, average_points, avg_pos_rank) ~ px(62),
        columns = c(performance_diff) ~ px(55)
      ) %>%
      gt_color_rows(
        vorp, 
        palette = pal_hex, 
        domain = range(stats_pct_tbl() %>% pull(vorp), na.rm = T)
      ) %>%
      gt_color_rows(
        total_points, 
        palette = pal_hex, 
        domain = range(stats_pct_tbl() %>% pull(total_points), na.rm = T)
      ) %>%
      gt_color_rows(
        average_points, 
        palette = pal_hex, 
        domain = range(stats_pct_tbl() %>% pull(average_points), na.rm = T)
      ) %>%
      tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
      ) %>%
      tab_options(footnotes.multiline = TRUE)
  })
  
  # opportunities totals
  output$plot3 <- render_gt({
    
    # opportunities by position
    if (player()$position == "QB") {
      
      qb_pct_tbl <- hvo_qb %>%
        select(season, attempts, rush_attempts, hvo)
      
      # QB total HVOs
      hvo_qb %>%
        filter(hvo_type == "hvo_pct" & player_name == player()$player_display_name) %>%
        select(season, attempts, rush_attempts, hvo) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_options(
          heading.align = "center",
        ) %>%
        tab_header(
          title = "High Value Opportunities by Season",
          subtitle = ""
        ) %>%
        cols_align(
          "center"
        ) %>%
        cols_label(
          season = "Season",
          attempts = "Pass ATT",
          rush_attempts = "Rush ATT",
          hvo = "High Value Opps"
        ) %>%
        cols_width(
          columns = everything() ~ px(70)
        ) %>%
        gt_color_rows(
          attempts, 
          palette = pal_hex, 
          domain = range(qb_pct_tbl %>% pull(attempts), na.rm = T)
        ) %>%
        gt_color_rows(
          rush_attempts, 
          palette = pal_hex, 
          domain = range(qb_pct_tbl %>% pull(rush_attempts), na.rm = T)
        ) %>%
        gt_color_rows(
          hvo, 
          palette = pal_hex, 
          domain = range(qb_pct_tbl %>% pull(hvo), na.rm = T)
        ) %>%
        tab_footnote(
          "Figure: @MambaMetrics | Data: @nflfastR"
        )
      
      
    } else if (player()$position == "RB") {
      
      rb_pct_tbl <- hvo_rb %>%
        select(season, total_touches, hvo, tgt)
      
      # RB total HVOs
      hvo_rb %>%
        filter(hvo_type == "hvo_pct" & player_name == player()$player_display_name) %>%
        select(season, total_touches, hvo, tgt) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_options(
          heading.align = "center",
        ) %>%
        tab_header(
          title = "High Value Opportunities by Season",
          subtitle = ""
        ) %>%
        cols_align(
          "center"
        ) %>%
        cols_label(
          season = "Season",
          total_touches = "Total Touches",
          hvo = "High Value Opps",
          tgt = "Targets",
        ) %>%
        cols_width(
          columns = everything() ~ px(70)
        ) %>%
        gt_color_rows(
          total_touches, 
          palette = pal_hex, 
          domain = range(rb_pct_tbl %>% pull(total_touches), na.rm = T)
        ) %>%
        gt_color_rows(
          hvo, 
          palette = pal_hex, 
          domain = range(rb_pct_tbl %>% pull(hvo), na.rm = T)
        ) %>%
        gt_color_rows(
          tgt, 
          palette = pal_hex, 
          domain = range(rb_pct_tbl %>% pull(tgt), na.rm = T)
        ) %>%
        tab_footnote(
          "Figure: @MambaMetrics | Data: @nflfastR"
        )
      
    } else if (player()$position == "WR") {
      
      wr_pct_tbl <- hvo_wr %>%
        mutate(adot = round(adot, 1)) %>%
        select(season, adot, tgt, hvo_pot)
      
      # WR targets
      hvo_wr %>%
        filter(player_name == player()$player_display_name) %>%
        mutate(adot = round(adot, 1)) %>%
        select(season, adot, tgt, hvo_pot) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_options(
          heading.align = "center",
        ) %>%
        tab_header(
          title = "High Value Opportunities by Season",
          subtitle = ""
        ) %>%
        cols_align(
          "center"
        ) %>%
        cols_label(
          season = "Season",
          adot = "aDOT",
          tgt = "Targets",
          hvo_pot = "High Value Opps"
        ) %>%
        cols_width(
          columns = everything() ~ px(70)
        ) %>%
        gt_color_rows(
          adot, 
          palette = pal_hex, 
          domain = range(wr_pct_tbl %>% pull(adot), na.rm = T)
        ) %>%
        gt_color_rows(
          tgt, 
          palette = pal_hex, 
          domain = range(wr_pct_tbl %>% pull(tgt), na.rm = T)
        ) %>%
        gt_color_rows(
          hvo_pot, 
          palette = pal_hex, 
          domain = range(wr_pct_tbl %>% pull(hvo_pot), na.rm = T)
        ) %>%
        tab_footnote(
          "Figure: @MambaMetrics | Data: @nflfastR"
        )
      
    } else if (player()$position == "TE") {
      
      te_pct_tbl <- hvo_te %>%
        filter(tgt >= 25) %>%
        mutate(adot = round(adot, 1)) %>%
        select(season, adot, tgt, hvo_pot)
      
      # TE targets
      hvo_te %>%
        filter(player_name == player()$player_display_name) %>%
        mutate(adot = round(adot, 1)) %>%
        select(season, adot, tgt, hvo_pot) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_options(
          heading.align = "center",
        ) %>%
        tab_header(
          title = "High Value Opportunities by Season",
          subtitle = ""
        ) %>%
        cols_align(
          "center"
        ) %>%
        cols_label(
          season = "Season",
          adot = "aDOT",
          tgt = "Targets",
          hvo_pot = "High Value Opps"
        ) %>%
        cols_width(
          columns = everything() ~ px(70)
        ) %>%
        gt_color_rows(
          adot, 
          palette = pal_hex, 
          domain = range(te_pct_tbl %>% pull(adot), na.rm = T)
        ) %>%
        gt_color_rows(
          tgt, 
          palette = pal_hex, 
          domain = range(te_pct_tbl %>% pull(tgt), na.rm = T)
        ) %>%
        gt_color_rows(
          hvo_pot, 
          palette = pal_hex, 
          domain = range(te_pct_tbl %>% pull(hvo_pot), na.rm = T)
        ) %>%
        tab_footnote(
          "Figure: @MambaMetrics | Data: @nflfastR"
        )
      
    } else {
      # Handle the case when the player's position is not recognized
      cat("Selected player's position is not recognized.")
    }
    
  })
  
  # finishes by season
  output$plot4 <- render_gt({
    stats_weekly %>%
      group_by(season, week, position) %>%
      mutate(week_rank = if_else(total_points == 0,
                                 NA, rank(-total_points, ties.method = "first"))) %>%
      ungroup() %>%
      group_by(season, position, recent_team, player_display_name) %>%
      summarise(top_6 = sum(if_else(week_rank <= 6, 1, 0), na.rm = TRUE),
                top_12 = sum(if_else(week_rank <= 12, 1, 0), na.rm = TRUE),
                top_18 = sum(if_else(week_rank <= 18, 1, 0), na.rm = TRUE),
                top_24 = sum(if_else(week_rank <= 24, 1, 0), na.rm = TRUE),
                games = n()) %>%
      ungroup() %>%
      filter(player_display_name == player()$player_display_name) %>%
      select(season, recent_team, games, top_6, top_12, top_18, top_24) %>%
      gt() %>%
      gt_theme_538() %>%
      tab_options(
        heading.align = "center",
      ) %>%
      tab_header(
        title = "Fantasy Finishes by Season",
        subtitle = ""
      ) %>%
      cols_align(
        "center"
      ) %>%
      cols_label(
        season = "Season",
        recent_team = "Team",
        games = "Games",
        top_6 = "Top 6",
        top_12 = "Top 12",
        top_18 = "Top 18",
        top_24 = "Top 24"
      ) %>%
      cols_width(
        columns = season ~ px(62),
        columns = c(recent_team,games) ~ px(58),
        columns = c(top_6,top_12,top_18,top_24) ~ px(55)
      ) %>%
      gt_color_rows(
        top_6, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(top_6), na.rm = T)
      ) %>%
      gt_color_rows(
        top_12, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(top_12), na.rm = T)
      ) %>%
      gt_color_rows(
        top_18, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(top_18), na.rm = T)
      ) %>%
      gt_color_rows(
        top_24, 
        palette = pal_hex, 
        domain = range(finishes_pct_tbl() %>% pull(top_24), na.rm = T)
      ) %>%
      tab_footnote(
        "Figure: @MambaMetrics | Data: @nflfastR"
      )
    
  })
  
}


# UI ----
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("Fantasy Football Player Dashboard"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Overview", fluid = T,
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("position1", "Select Position:",
                                       choices = c("QB", "RB", "WR", "TE"),
                                       selected = "QB"),
                           selectInput("player1", "Select Player:",
                                       choices = unique(stats_yearly$player_display_name),
                                       selectize = TRUE),
                           selectInput("year", "Select Season:",
                                       choices = NULL,
                                       selected = max(stats_yearly$season)),
                           gt_output("plot4"),
                           width = 4
                         ),
                         mainPanel(
                           fluidRow(
                             column(12, plotOutput("plot1", height = 300)),
                             h3(textOutput(""), align = "center")
                           ),
                           fluidRow(
                             column(8, gt_output("plot2")),
                             column(4, gt_output("plot3"))
                           )
                         )
                       )
              )
  )
)

# Run the Shiny app
shinyApp(ui, server)


