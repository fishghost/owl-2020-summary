library(shiny)
library(shinyjs)
library(ggplot2)
library(ggimage)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(rmarkdown)

#### Read stuff ####

team_colours <- readr::read_csv("www/TeamLogos/TeamColours.csv", col_types = "cffccc")
owl_logos_path <- "www/TeamLogos"
team_colours <- team_colours %>%
  mutate(logo = paste(file.path(owl_logos_path, abb),".png", sep = ""))
owl_colours <- c("wins" = "#FA9C1D", "upset_wins" = "#FFC360", "favoured_loses" = "#868A8E", "loses" = "#4A4C4E")
map_score_color <- c("3-0" = "#e18c1a", "3-1" = "#fa9c1d", "3-2" = "#fcc377", 
                     "2-3" = "#929394", "1-3" = "#6e6f71", "0-3" = "#4a4c4e")
col_scale <- c("4"="#1A86FF", "3"="#1A85FF", "2"="#57A5FF", "1"="#9DC5F3", 
               "-1"="#E07CA1", "-2"="#D04F7E", "-3"="#D41159", "-4"="#D41259")
match_table_names <- c("date" = "Date", "opponent" = "Opponent",  "outcome" = "Outcome",
                 "score" = "Maps won", "to" = "Maps lost", 
                 "odds" = "Odds to win", "against" = "Odds to lose")
opponent_table_names <- c("date" = "Date", "opponent" = "Opponent", "outcome" = "Outcome",
                       "score" = "Maps won", "to" = "Maps lost", 
                       "odds" = "Odds to win", "against" = "Odds to lose")
map_score_order <- data.frame(map_score = c("3-0", "3-1", "3-2", "2-3", "1-3", "0-3"))
logo_dims <- "70px"

csv_files <- file.path("www/CSV",list.files("www/CSV"))
csv_path <- tail(csv_files,1)
cleaned_dat <- read.csv(csv_path)

rm(owl_logos_path, csv_files, csv_path)

#### Data wrangling ####
matches <- cleaned_dat %>%
  mutate(winner = ifelse(t1score>t2score,team1,team2), 
         opponent = ifelse(t1score<t2score,team1,team2)) %>%
  mutate(wodd = ifelse(t1score>t2score,odd2/(odd1+odd2),odd1/(odd1+odd2)), lodd = 1-wodd) %>%
  select(date,t1score,t2score,winner,wodd,opponent,lodd)

won <- matches %>%
  group_by(winner) %>%
  summarize(wins = n())

lost <- matches %>%
  group_by(opponent) %>%
  summarize(loses = n())

upset_wins <- matches %>%  filter(wodd < 0.5) %>% 
  group_by(winner) %>% 
  summarise(upset_wins = n()) %>% 
  arrange(desc(upset_wins))

favoured_loses <- matches %>% filter(lodd > 0.5) %>% 
  group_by(opponent) %>% 
  summarise(favoured_loses = n()) %>% 
  arrange(desc(favoured_loses)) %>%
  setNames(c("loser", "favoured_loses"))

# Team data
ft <- cleaned_dat %>%
  select(team2, odd2, odd1) %>%
  setNames(c("team", "odds", "opponent_odds"))

f <- cleaned_dat %>%
  select(team1, odd1, odd2) %>%
  setNames(c("team", "odds", "opponent_odds")) %>%
  bind_rows(ft) %>%
  mutate(favoured = ifelse(odds<opponent_odds, TRUE, FALSE))
rm(ft)

total_odds <- f %>%
  group_by(team) %>%
  summarise(total_played = n(), odd_sum = sum(odds), 
            odd_avg = odd_sum/total_played)

favoured <- f %>%
  group_by(team) %>%
  summarise(fav_to_win = sum(favoured))

summary_table <- total_odds %>%
  select(team, total_played, odd_sum, odd_avg) %>%
  left_join(won, by = c("team" = "winner")) %>% 
  left_join(lost, by = c("team" = "opponent")) %>% 
  left_join(favoured, by = "team") %>% 
  left_join(upset_wins, by = c("team" = "winner")) %>% 
  left_join(favoured_loses, by = c("team" = "loser")) %>%
  mutate_at(c("upset_wins","favoured_loses"), list(~replace(., which(is.na(.)), 0))) 
rm(won, lost, favoured, upset_wins, favoured_loses, f, total_odds)

team_appearances <- cleaned_dat %>%
  .[nrow(.):1,] %>% # <- inverts data frame, making first game of season at top
  mutate(t1o = team2, t2o = team1) %>% 
  mutate(match_num = row_number()) %>% 
  reshape2::melt(measure.vars = c("team1", "team2"), variable.name = "t1t2", value.name = "team") %>% 
  mutate(opponent = ifelse(t1t2=="team1", t1o, t2o), 
         score = ifelse(t1t2=="team1",t1score, t2score),
         to = ifelse(t1t2=="team1",t2score,t1score), 
         odds = ifelse(t1t2=="team1", odd1, odd2), 
         against = ifelse(t1t2=="team1",odd2,odd1)) %>% 
  arrange(match_num) %>% 
  select(date, (ncol(.)-7):ncol(.), -t1t2) %>%
  group_by(team) %>% 
  mutate(app = 1:n()) %>% # <- short for appearance
  arrange(team, app)

ms_dist <- team_appearances %>%
  mutate(map_score = paste(score,abs(to),sep="-")) %>%
  count(map_score) %>%
  ungroup() %>%
  # Pivot wider to add NAs and replace them with them with 0s
  pivot_wider(names_from = map_score, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  left_join(select(team_colours, c(team,logo)), by = "team") %>% 
  mutate(map_diff = 3*`3-0`+2*`3-1`+1*`3-2`-1*`2-3`-2*`1-3`-3*`0-3`) %>%
  mutate(team = reorder(team, map_diff)) %>% 
  mutate(team = reorder(team, summary_table$wins))

#### Shiny UI ####

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Odds Portal OWL 2020 Season Summary"),
  fluidRow(column(12, align = "center",
                  tags$p("Click a team logo for more detailed information."),                  
                  tabsetPanel(
                    tabPanel(
                      "Win-Loss Expectations",
                      # radioButtons(inputId = "summary_order", label = "Ordered by:", inline = TRUE, 
                      #              choices = c("Wins" = "wins", 
                      #                          "Upset wins" = "upsets", 
                      #                          "Favoured loses" = "loses"),
                      #              selected = "wins"),
                      plotOutput(outputId = "summary_plot", width = "850px", height = "600px",
                                 click = "clicked_team", dblclick = "clicked_team")
                    ),
                    tabPanel(
                      "Map Score Distribution",
                      plotOutput(outputId = "map_score_plot", width = "850px", height = "600px", 
                                 click = "clicked_team_ms", dblclick = "clicked_team_ms")                      
                    ),
                    tabPanel(
                      "Read Me",
                      tags$div(style="text-align:left;",includeMarkdown(file.path("www","readme.md")))
                    )
                    ))),
  wellPanel(id="anchor_team",
    # tags$div(id="anchor_team"),
    fluidRow(column(1, offset = 3, imageOutput("team_logo", width = logo_dims, height = logo_dims, inline = TRUE)), 
             column(8,tags$h2(textOutput("team_name")))),
    fluidRow(column(12, align = "center",
                    tags$h4(textOutput(outputId = "expected_outcomes")),
                    tableOutput(outputId = "map_table"))),
    ## Inputs:
    fluidRow(
      column(4, offset=2, 
             selectInput(inputId = "selected_team", label = "Team to Show:",
                         choices = unique(team_appearances$team))
      ),
      column(4, offset = 2,
             radioButtons(inputId = "plot_type", label = "Odds displayed as:", inline = TRUE,
                          choices = c("Percentage" = "clean",
                                      "Betting" = "weird"), 
                          selected = "clean"))
    ),
    ## Instructions and Match Table
    fluidRow(column(12, align = "center", 
                    tags$p(htmlOutput(outputId = "plot_instruction")),
                    tableOutput(outputId = "clicked_match"))),
    ## Main Plot
    fluidRow(column(12, align = "center",
                    plotOutput(outputId = "main_plot", width = "850px", height = "500px",
                               click = "clicked_match", dblclick = "dbl_main"))),
    tags$hr(),
    ## Opponent summary
    fluidRow(column(12, align = "center", 
                    tags$h3(textOutput("opponent_label")), 
                    tags$p(tags$b("Click a bar to see all matches."), "Double click to switch to that team."),
                    tableOutput(outputId = "clicked_opponent"))),
    fluidRow(column(12, align = "center",
                    plotOutput(outputId = "opponent_plot", width = "850px", height = "500px",
                               click = "clicked_opponent", dblclick = "dbl_opponent")))
    
  )
)

#### Shiny server ####

server <- function(input, output, session) {
  
  #### Summary Plot ####
  output$summary_plot <- renderPlot({
    plot_data <- summary_table %>% 
      left_join(team_colours, by = "team") %>%
      mutate_if(is.character, as.factor) %>%
      select(team, abb, logo, primary, wins, upset_wins, loses, favoured_loses) %>%
      # Adding 0.1s to declutter text labels when upsets/floses is 1 less than wins/loses 
      mutate(upset_wins = ifelse(upset_wins==wins-1, upset_wins+0.1, upset_wins), 
             favoured_loses = ifelse(favoured_loses==loses-1, favoured_loses+0.1, favoured_loses)) %>%
      # Negate loses for plot
      mutate(loses = -1*loses, favoured_loses = -1*favoured_loses)
    
    # Summary order as determined by user input (currently wonky)
    # if (input$summary_order == "wins") {
    #   plot_data <- plot_data %>%
    #     mutate(team = reorder(team, desc(wins)))
    # } else if (input$summary_order == "upsets") {
    #   plot_data <- plot_data %>%
    #     mutate(team = reorder(team, desc(upset_wins)))
    # } else if (input$summary_order == "loses") {
    #   plot_data <- plot_data %>%
    #     mutate(team = reorder(team, favoured_loses))
    # }
    
    plot_data <- plot_data %>%
      mutate(team = reorder(team, desc(wins)))

    ## Plot: 
    plot_data %>%
      pivot_longer(cols = -(1:4), names_to = "variable") %>%
      ggplot(aes(x = team, y = value)) +
      ## Extras: 
      # geom_text(x = 16, y = 18, label = "double click on a team logo \nfor more details", 
      #           size = 8, colour = owl_colours[[1]]) + 
      geom_hline(yintercept = 0, color = "black") + 
      # Main: 
      geom_bar(aes(fill = variable), stat="identity", position = "identity") +
      geom_image(aes(image = logo, y = ifelse(variable == "wins", value+3, NA)), size = 0.04, asp = 1.42, na.rm = TRUE) + 
      geom_text(aes(label = abs(value), color = variable,
                    y = case_when(value == 0 ~ NaN, 
                                  value%%1 != 0 ~ NaN,
                                  variable %in% c("wins","upset_wins") ~ value + 1,
                                  variable %in% c("loses","favoured_loses") ~ value - 1,
                                  TRUE ~ NaN)), fontface = "bold", na.rm = TRUE) + 
      ## Format: 
      scale_fill_manual(name = "", labels = c("Favoured loses", "Loses", "Upset wins", "Wins"), 
                        values=owl_colours) +
      scale_color_manual(values=owl_colours) +
      ylab("Loses/Wins") + 
      guides(color = FALSE, fill = guide_legend(reverse = TRUE)) + 
      theme_void() + 
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"), 
            legend.position = "top", 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.title.x = element_blank())
  }, bg="transparent")
  
  #### Map Score Plot ####
output$map_score_plot <- renderPlot({
  ms_dist %>%
    pivot_longer(cols = -c(team,logo,map_diff), names_to = "map_score", values_to = "n") %>% 
    mutate(map_score = factor(map_score, levels = rev(names(map_score_color)))) %>% 
    # Plot  
    ggplot(aes(y = team, x = n)) + 
    geom_bar(aes(fill = map_score), color = "white", stat = "identity", position = "stack") + 
    geom_image(aes(image = logo), x = -1, asp = 1.42, size = 0.04) +
    geom_text(aes(label = sprintf("%+d",map_diff)), x = 21.6, color = "#4a4c4e") + 
    # facet_grid(cols = vars(map_score)) + 
    # Format
    scale_fill_manual(values = map_score_color) + 
    scale_x_continuous(limits = c(-1, NaN)) + 
    theme(plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"), 
          legend.position = "top", 
          axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) + 
    guides(fill = guide_legend(title = NULL, reverse = TRUE, nrow = 1))
})  

  #### Main Plot ####
  team_data <- eventReactive(c(input$plot_type, input$selected_team), {
    if (input$plot_type == "weird") {
      team_data <- team_appearances %>%
        ungroup() %>%
        filter(grepl(input$selected_team, team)) %>%
        left_join(team_colours, by="team") %>%
        left_join(team_colours, by=c("opponent"="team"), suffix = c("_t","_o")) %>%
        mutate(logo_pos = case_when(abs(odds-against) > 0.12 ~ against,
                                    odds-against >= 0 ~ against - 0.5, 
                                    against-odds > 0 ~ against + 0.5))
    } else if (input$plot_type == "clean") {
      team_data <- team_appearances %>%
        ungroup() %>%
        filter(grepl(input$selected_team, team)) %>%
        mutate(pwin = (against)/(odds+against), ploss = (odds)/(odds+against)*-1) %>% 
        mutate(pwin = ifelse(pwin <= 0.5, 0, pwin-0.5), ploss = ifelse(ploss >= -0.5, 0, ploss+0.5)) %>%
        mutate(map_diff = score-to, to = -1*to, against = -1*against) %>%
        left_join(team_colours, by="team") %>%
        left_join(team_colours, by=c("opponent"="team"), suffix = c("_t","_o")) %>%
        mutate(logo_pos = map_diff)
    }
  })
  
  observeEvent(c(input$plot_type, input$selected_team), {
    output$clicked_match <- renderTable(read.csv(text=paste(match_table_names, collapse = ","),
                                                 check.names = FALSE))
    
    output$clicked_opponent <- renderTable(read.csv(text=paste(opponent_table_names, collapse = ","),
                                                 check.names = FALSE))
  })
  
  observeEvent(c(input$plot_type), {
    instruction_text <- ""
    plot_clickable <- ""
    if(input$plot_type == "weird") {
      instruction_text <- paste("Solid line connects aggregated betting odds for selected team to win match; arrows representing match outcome and map score.", 
                            "Team logos on dotted line represent betting odds for opponent to win.",
                            "<br>An arrow positioned above a team logo is an expected win.")
      plot_clickable <- "arrow"
    } else if(input$plot_type == "clean") {
      instruction_text <- paste("Each bar represents the expected chance to win (above) or lose (below) a match.", 
                            "Opponents shows as their team logos with position representing map score; above line for wins and below line for loses.")
      plot_clickable <- "bar"
    }
    output$plot_instruction <- renderText({
      paste("<b>Instructions:</b>",
        instruction_text, 
        "<br><b>Click a logo or ",
        plot_clickable,
        " below for match details</b>. Double click to switch to that team's season summary."
      )
    })
  })
  
  observeEvent(input$selected_team, { 
    output$team_logo <- renderImage({ list(src = file.path(filter(team_colours, grepl(input$selected_team,team))$logo),
                                          width = logo_dims, height = logo_dims) }, 
                                   deleteFile = FALSE)
    team_row <- filter(summary_table, grepl(input$selected_team,team))
    team_name_score <- paste0(input$selected_team," (",team_row$wins,"-",team_row$loses,")")
    output$team_name <- renderText(team_name_score)
    
    opponent_label <- paste0("Opponent Summary for ", input$selected_team)
    output$opponent_label <- renderText(opponent_label)
    
    # expected_wins <- team_row$wins-team_row$upset_Wins
    # expected_loses <- team_row$wins+team_row$loses-expected_wins
    expected_outcomes <- paste0("Expected: (", team_row$fav_to_win, "-", 
                               team_row$total_played-team_row$fav_to_win,")")
    output$expected_outcomes <- renderText(expected_outcomes)
    
    map_table <- team_data() %>%
      mutate(map_score = paste(score,abs(to),sep="-")) %>% 
      count(map_score) %>% 
      right_join(map_score_order, by = "map_score") %>%
      mutate(n = replace_na(n,0)) %>%
      column_to_rownames("map_score") %>%
      t()
    
    output$map_table <- renderTable(map_table, digits = 0)
    })
  
  observeEvent(input$clicked_match, {
    if (!is.null(team_data())) {
      clicked <- nearPoints(team_data(), input$clicked_match,
                            xvar = "app", yvar = "logo_pos",
                            threshold = 10)
      
      # Check to see if arrow or bar is clicked instead
      if(nrow(clicked)==0) {
        if (input$plot_type == "weird") {
          clicked <- nearPoints(team_data(), input$clicked_match,
                                xvar = "app", yvar = "odds",
                                threshold = 10)          
        } else if (input$plot_type == "clean") {
          if (is.null(input$clicked_match$x)) return()
          else {
            keeprows <- round(input$clicked_match$x) == as.numeric(team_data()$app)
            clicked <- team_data()[keeprows,]
          }
        }
      }
      
      # Only change selection if valid area is clicked
      if(nrow(clicked)>0) {
        clicked <- clicked %>%
          mutate(outcome = ifelse(score>abs(to),"Won","Lost")) %>%
          select(names(match_table_names)) %>%
          mutate_at(c("score","to"), funs(as.integer(abs(.)))) %>%
          mutate_at("against", abs) %>%
          mutate(date = format.Date(date,"%B %d")) %>%
          setNames(match_table_names)
        
        output$clicked_match <- renderTable(clicked[1,])
      }
    }

  })
  
  output$main_plot <- renderPlot({
    if (input$plot_type == "weird") {
      #### Weird Plot ####
      team_data() %>%
        ## plot
        ggplot(aes(x = app, y = odds)) +
        geom_line() + 
        geom_line(aes(y = against), linetype = 2) + 
        geom_point(aes(fill = as.character(score-to), shape = ifelse(score>to, 'win', 'loss'), stroke = case_when(
          abs(score-to)==4 ~ 3,
          abs(score-to)==3 ~ 3,
          abs(score-to)==2 ~ 2,
          abs(score-to)==1 ~ 1
        )), size = 6, alpha = 0.8) +
        # geom_point(aes(shape = ifelse(score>to, 'win', 'loss')), size = 8) +
        geom_text(aes(label = paste(score,to,sep=":")), color = "white", size = 3) +
        facet_grid(.~team, labeller = labeller(team_colours$abb)) +
        geom_image(aes(y = logo_pos,
                       image = logo_o), size = 0.04, asp = 1.7) + 
        # Would want to add the dates on x-axis instead of appearance
        scale_fill_manual(name = "Map Difference", values = col_scale) +
        scale_shape_manual(name = "Match Outcome", values = c(25, 24)) + 
        scale_y_continuous(trans = "reverse") + 
        scale_x_continuous(limits = c(1,21)) +
        xlab("Appearance in OWL") + ylab("OddsPortal betting odds") + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"), legend.position = "none")      
    } else if (input$plot_type == "clean") {
      #### Clean Plot ####
      team_data() %>%
        reshape2::melt(measure.vars = c("pwin", "ploss")) %>% 
        mutate(plabel = case_when(variable == "pwin" & value > 0 ~ value,
                                  variable == "ploss" & value < 0 ~ value, 
                                  TRUE ~ NaN)) %>%
        ## Plot: 
        ggplot(aes(x = app)) + 
        # geom_hline(yintercept = c(1.5, 0,  -1.5), alpha = 0.5) + 
        geom_hline(yintercept = c(3, 0,  -3), alpha = 0.3) + 
        geom_bar(aes(y = ifelse(variable %in% c("pwin", "ploss"),value*6, NaN), fill = variable), 
                 stat="identity", position = "stack", alpha = 0.8) + 
        geom_text(aes(label = paste0(round(abs(plabel+0.5*sign(plabel))*100),"%"),
                      y = 6*plabel-0.16*sign(plabel),
                      color = case_when(variable == "pwin" ~ "loses", variable == "ploss" ~ "wins")),
                  na.rm = TRUE) +
        geom_image(aes(y = logo_pos, image = logo_o), asp = 1.7) + 
        # geom_text(aes(y = map_diff+sign(map_diff)*0.5, label = paste(score, -to, sep = "-"))) + 
        ## Format
        scale_color_manual(values = owl_colours, guide = FALSE) +
        scale_fill_manual(name = "Expected chance to", values = c("pwin" = owl_colours[[1]],"ploss" = owl_colours[[4]]),
                          labels = c("pwin" = "win","ploss" = "lose")) +
        facet_grid(.~team, labeller = labeller(team_colours$abb)) +
        labs(x = "Appearance in OWL", y = "match score") + 
        scale_y_continuous(sec.axis = sec_axis(~.*33.33, name = "win expectation from OddsPortal (%)", 
                                               breaks = c(-100,0,100), labels = c("-100%", "50/50%", "100%")), 
                           limits = c(-4, 4), n.breaks = 9, 
                           labels = c("", "0-3", "1-3", "2-3", "", "3-2", "3-1", "3-0", "")) + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"),
              legend.position = "bottom", 
              axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
              axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
      
      # Alternate bar representation
      # geom_bar(aes(y = ifelse(variable %in% c("pwin", "ploss"),value*3, NaN), fill = variable), 
      # scale_y_continuous(sec.axis = sec_axis(~.*33.33, name = "win expectation from OddsPortal (%)", 
      #                                        breaks = c(-100,-50,50,100), labels = c("-100%", "-50","50%", "100%")), 
    }
  }, bg="transparent")
  
  #### Opponent Plot ####
  
  observeEvent(input$clicked_opponent, {
    if (!is.null(team_data())) {
      team_order <- team_data() %>%
        group_by(opponent) %>%
        summarize(faced = n()) %>% 
        left_join(select(team_colours,c(1,7)),by = c("opponent" = "team")) %>%
        arrange(faced, desc(opponent)) %>% 
        mutate(y = 1:n())
      
      clicked_opp <- team_order[round(input$clicked_opponent$y),]$opponent
      
      output$clicked_opponent <- renderTable({
      team_data() %>%
          filter(opponent == clicked_opp) %>%
          mutate(outcome = ifelse(score>abs(to),"Won","Lost")) %>%
          select(names(opponent_table_names)) %>%
          mutate_at(c("score","to"), funs(as.integer(abs(.)))) %>%
          mutate_at("against", abs) %>%
          mutate(date = format.Date(date,"%B %d")) %>%
          setNames(opponent_table_names)
      })
    }
  })
  
  output$opponent_plot <- renderPlot({
    team_data() %>%
      group_by(opponent) %>%
      summarize(faced = n(), wins = length(team[score>abs(to)])) %>% # abs because `to` is negative sometimes
      left_join(select(team_colours,c(1,7)),by = c("opponent" = "team")) %>%
      # arrange(desc(faced), opponent) %>%
      mutate(opponent = reorder(opponent, desc(opponent)), opponent = reorder(opponent, faced)) %>% 
      pivot_longer(cols = c("faced", "wins")) %>% 
      
      ggplot(aes(y=opponent)) + 
      geom_bar(aes(x = value, fill = name), stat = "identity", position = "nudge") +
      geom_text(aes(x = value+0.1, colour = name, label = value), fontface = "bold") + 
      geom_image(aes(image = logo), x = -0.3, asp = 1.7) + 
      
      # Format
      scale_x_continuous(limits = c(-0.35,NaN)) + 
      scale_fill_manual(name = "", values = c("faced" = owl_colours[[4]], "wins" = owl_colours[[1]]), 
                        labels = c("Times played", "Matches won")) + 
      scale_color_manual(name = "", values = c("faced" = owl_colours[[4]], "wins" = owl_colours[[1]]), guide = FALSE) +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"),
            axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
            legend.position = "bottom")
  }, bg="transparent")
  
  #### Double click ####
  observeEvent (input$clicked_team, {
    # Add different summary orders later (currently wonky): 
    # if (input$summary_order == "wins") team_order <- arrange(summary_table, desc(wins), team)
    # else if (input$summary_order == "upsets") team_order <- arrange(summary_table, desc(round(upset_wins)), team)
    # else if (input$summary_order == "loses") team_order <- arrange(summary_table, round(favoured_loses), team)
    
    team_order <- summary_table %>%
      arrange(desc(wins),team)

    clicked <- team_order[round(input$clicked_team$x),]$team
    
    # Only change selection if valid area is clicked
    if(!identical(clicked,character(0))) {
      updateSelectInput(session, "selected_team",
                        selected = clicked)
      
    }
    
    # scroll down 
    runjs('
      document.getElementById("anchor_team").scrollIntoView({behavior: "smooth", block: "start", inline: "nearest"});
    ')
  })
  
  observeEvent (input$clicked_team_ms, {
    team_order <- ms_dist %>%
      arrange(summary_table$wins,map_diff)
    
    clicked <- team_order[round(input$clicked_team_ms$y),]$team

    # Only change selection if valid area is clicked
    if(!identical(clicked,character(0))) {
      updateSelectInput(session, "selected_team",
                        selected = clicked)

    }
    
    # scroll down 
    runjs('
      document.getElementById("anchor_team").scrollIntoView({behavior: "smooth", block: "start", inline: "nearest"});
    ')
  })
  
  observeEvent(input$dbl_main, {
    team_order <- team_data()
    
    clicked <- team_data()[round(input$dbl_main$x),]$opponent
      
    # Only change selection if valid area is clicked
    if(!identical(clicked,character(0))) {
      updateSelectInput(session, "selected_team",
                        selected = clicked)
      
    }
  })
  
  observeEvent(input$dbl_opponent, {
    team_order <- team_data() %>%
      group_by(opponent) %>%
      summarize(faced = n()) %>% 
      left_join(select(team_colours,c(1,7)),by = c("opponent" = "team")) %>%
      arrange(faced, desc(opponent)) %>% 
      mutate(y = 1:n())
    
    clicked <- team_order[round(input$dbl_opponent$y),]$opponent
    
    # Only change selection if valid area is clicked
    if(!identical(clicked,character(0))) {
      updateSelectInput(session, "selected_team",
                        selected = clicked)
      
    }
  })
}

shinyApp(ui = ui, server = server)