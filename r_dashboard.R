# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(corrplot)
library(RColorBrewer)

# Load the datasets
fifa_summary <- read_excel("C:/Users/Vaibhavi Deo/Downloads/fifa_wcsummary.xlsx")
fifa_data <- read_excel("C:/Users/Vaibhavi Deo/Downloads/fifa_data.xlsx")

# Data Preparation for fifa_summary
# 1. World Cup Wins per Team
team_wins <- fifa_summary %>%
  group_by(CHAMPION) %>%
  summarise(total_wins = n())

# 2. Year vs Total Goals Scored
total_goals_per_year <- fifa_summary %>%
  group_by(YEAR) %>%
  summarise(total_goals = sum(`GOALS SCORED`, na.rm = TRUE))

# 3. Year vs Matches per Year
matches_per_year <- fifa_summary %>%
  group_by(YEAR) %>%
  summarise(total_matches = sum(MATCHES, na.rm = TRUE))

# 4. Host Country Wins
host_country_wins <- fifa_summary %>%
  mutate(host_win = CHAMPION == HOST) %>%
  group_by(host_win) %>%
  summarise(count = n())

# Data for Bubble Chart
bubble_data <- fifa_summary %>%
  select(YEAR, MATCHES, `GOALS SCORED`) %>%
  mutate(size = `GOALS SCORED` / max(`GOALS SCORED`, na.rm = TRUE) * 100)

# Data Preparation for Treemap
goals_by_country <- fifa_summary %>%
  group_by(CHAMPION) %>%
  summarise(total_goals = sum(`GOALS SCORED`, na.rm = TRUE)) %>%
  rename(country = CHAMPION)

# Data Preparation for fifa_data
# 1. World Cup Wins per Team
team_wins_fifa_data <- fifa_data %>%
  filter(Position == 1) %>%
  count(Team)

# 2. Year vs Total Goals Scored
total_goals <- fifa_data %>%
  group_by(Year) %>%
  summarise(total_goals = sum(`Goals For`))

# 3. Year vs Average Goals per Game
avg_goals_per_game <- fifa_data %>%
  group_by(Year) %>%
  summarise(avg_goals = sum(`Goals For`) / sum(`Games Played`))

# 4. Goals Scored vs Goals Conceded (Density Plot) for Winning Teams
winning_team_goals <- fifa_data %>%
  filter(Position == 1)

# 5. Team Performance Over Time (Line Chart)
team_performance <- fifa_data %>%
  group_by(Year, Team) %>%
  summarise(points = sum(Points, na.rm = TRUE))

# 6. Goals Scored per Year and Team (Heatmap)
goals_per_team_year <- fifa_data %>%
  group_by(Year, Team) %>%
  summarise(goals_scored = sum(`Goals For`))



# Define UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "FIFA World Cup Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "fifa_summary", icon = icon("chart-line")),
      menuItem("Country Insights", tabName = "team_insights", icon = icon("flag")),
      
      
      # Selectize input for selecting multiple countries with search and scroll functionality
      selectizeInput("selected_countries", "Select Countries:", 
                     choices = unique(fifa_data$Team), 
                     selected = NULL,  # No countries selected by default
                     multiple = TRUE, 
                     options = list(
                       placeholder = 'Select country(s)',
                       maxItems = 10,  # Maximum number of countries to select
                       scrollHeight = 200  # Height of the scrollable area
                     )),
      menuItem("Year-wise Insights", tabName = "year_insights", icon = icon("calendar")),
      selectInput("selected_year", "Select Year:", 
                  choices = c("Select Year" = "", unique(fifa_data$Year)), 
                  selected = NULL)  # Default to NULL, no year selected
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "fifa_summary",
        fluidRow(
          # Value boxes for summary statistics
          valueBoxOutput("numEditionsBox"),
          valueBoxOutput("numChampionsBox"),
          valueBoxOutput("totalGoalsBox")
        ),
        fluidRow(
          box(plotlyOutput("pieChartWins"), title = "World Cup Wins by Country", width = 6),
          box(plotlyOutput("bubbleChart"), title = "Goals vs. Matches per Year", width = 6)
        ),
        fluidRow(
          box(plotlyOutput("scatterGoals"), title = "Year vs Total Goals Scored", width = 4),
          box(plotlyOutput("barHostWins"), title = "Host Country Wins", width = 4),
          box(plotlyOutput("scatterMatches"), title = "Year vs Matches Per Year", width = 4)
        ),
        fluidRow(
          box(plotlyOutput("treemapGoals"), title = "Total Goals", width = 12)
        )
      ),
      
      tabItem(
        tabName = "team_insights",
        fluidRow(
          box(plotlyOutput("heatmapGoals"), title = "Goals Scored per Year and Team", width = 12),
          box(plotlyOutput("linePerformance"), title = "Country Performance Chart", width = 12),
          box(plotlyOutput("densityGoals"), title = "Goals Scored vs Goals Conceded of Champion Countries", width = 6),
          box(plotlyOutput("barPoints"), title = "Total Points by Country", width = 6)
        )
      ),
      tabItem(
        tabName = "year_insights",
        fluidRow(
          box(plotlyOutput("positionBarChart"), title = "Top 3 Teams by Points in Selected Year", width = 6),
          box(plotlyOutput("scatterAvgGoals"), title = "Year vs Avg Goals per Game", width = 6),
          # New graphs for top 3 teams with most goals scored and conceded
          box(plotlyOutput("topGoalsScored"), title = "Top 3 Countries with Most Goals Scored", width = 6),
          box(plotlyOutput("topGoalsConceded"), title = "Top 3 Countries with Most Goals Conceded", width = 6)
          
          
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Value box for number of editions
  output$numEditionsBox <- renderValueBox({
    num_editions <- n_distinct(fifa_summary$YEAR)
    valueBox(
      num_editions,
      subtitle = "Number of Editions",
      icon = icon("trophy"),
      color = "blue"
    )
  })
  
  # Value box for number of unique champions
  output$numChampionsBox <- renderValueBox({
    num_champions <- n_distinct(fifa_summary$CHAMPION)
    valueBox(
      num_champions,
      subtitle = "Champion Countries",
      icon = icon("flag"),
      color = "light-blue"
    )
  })
  
  # Value box for total goals scored (updated for fifa_wcsummary file)
  output$totalGoalsBox <- renderValueBox({
    total_goals <- sum(fifa_summary$`GOALS SCORED`, na.rm = TRUE)
    valueBox(
      total_goals,
      subtitle = "Total Goals Scored",
      icon = icon("futbol"),
      color = "aqua"
    )
  })
  
  # Treemap: Total Goals by Country
  output$treemapGoals <- renderPlotly({
    plot_ly(
      data = goals_by_country,
      type = "treemap",
      labels = ~country,
      parents = rep("Total Goals", nrow(goals_by_country)),
      values = ~total_goals,
      hoverinfo = "label+value+percent entry",
      textinfo = "label+percent entry",
      marker = list(colors = RColorBrewer::brewer.pal(9, "Blues"))
    )
  })
  
  # Pie chart: World Cup Wins Distribution (fifa_summary)
  output$pieChartWins <- renderPlotly({
    plot_ly(
      data = team_wins,
      labels = ~CHAMPION,
      values = ~total_wins,
      type = 'pie',
      textinfo = 'label+value',
      hovertemplate = paste(
        '<b>Country:</b> %{label}<br>',
        '<b>Total Wins:</b> %{value}<extra></extra>'
      ),
      marker = list(colors = RColorBrewer::brewer.pal(length(team_wins$CHAMPION), "PuBu"))
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  # Density Plot: Goals Scored vs Goals Conceded (Winning Teams)
  output$densityGoals <- renderPlotly({
    ggplot(winning_team_goals) +
      geom_density(aes(x = `Goals For`, fill = "Goals Scored"), color = "olivedrab2", alpha = 0.4) +
      geom_density(aes(x = `Goals Against`, fill = "Goals Conceded"), color = "darkolivegreen", alpha = 0.4) +
      labs(x = "Goals", y = "Probability Density", title = "Probability Density of Champions' Goal Stats", fill = "Legend") +
      scale_fill_manual(values = c("Goals Scored" = "olivedrab2", "Goals Conceded" = "darkolivegreen")) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.box = "horizontal", legend.key.size = unit(0.5, "cm"))
  })
  
  # Scatter Plot: Year vs Avg Goals per Game
  output$scatterAvgGoals <- renderPlotly({
    ggplot(avg_goals_per_game, aes(x = Year, y = avg_goals)) +
      geom_point(color = "burlywood3") +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      labs(x = "Year", y = "Avg Goals per Game") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.box = "horizontal", legend.key.size = unit(0.5, "cm"))
  })

  # Bar Graph: Total Points by Country
  output$barPoints <- renderPlotly({
    points_per_country <- fifa_data %>%
      group_by(Team) %>%
      summarise(total_points = sum(Points, na.rm = TRUE)) %>%
      arrange(desc(total_points))
    
    # Create custom hover text
    points_per_country$text <- paste("Country:", points_per_country$Team, 
                                     "<br>Total Points:", points_per_country$total_points)
    
    p <- ggplot(points_per_country, aes(x = reorder(Team, -total_points), y = total_points, text = text)) +
      geom_bar(stat = "identity", fill = "olivedrab4") +
      labs(x = "Country", y = "Total Points", title = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),  # Hide x-axis labels
        axis.ticks.x = element_blank()  # Hide x-axis ticks
      )
    
    # Convert ggplot to plotly and ensure custom hover text is displayed
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "azure2")  # Optional: Set hover label background color
      )
  })
  
  
  
  # Scatter plot: Year vs Total Goals Scored
  output$scatterGoals <- renderPlotly({
    p <- ggplot(total_goals_per_year, aes(x = YEAR, y = total_goals)) +
      geom_point(aes(text = paste("Year:", YEAR, "<br>Total Goals: ", total_goals)), color = "deepskyblue") +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      labs(x = "Year", y = "Total Goals Scored") +
      theme_minimal()
    
    # Convert ggplot to plotly and add hover text with customized background color
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "azure2")  # Set hover label background color to grey
      )
  })
  
  # Bar plot: Host Country Wins
  output$barHostWins <- renderPlotly({
    p <- ggplot(host_country_wins, aes(x = as.factor(host_win), y = count, fill = as.factor(host_win))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "darkblue")) +
      labs(
        x = "Host Win",
        y = "Number of Occurrences",
        fill = "Host Win"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  # Scatter plot: Year vs Matches Per Year
  output$scatterMatches <- renderPlotly({
    p <- ggplot(matches_per_year, aes(x = YEAR, y = total_matches)) +
      geom_point(aes(text = paste("Year:", YEAR, "<br>Total Matches:", total_matches)), color = "darkturquoise") +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      labs(x = "Year", y = "Matches Per Year") +
      theme_minimal()
    
    # Convert ggplot to plotly and add hover text with customized background color
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "azure2")  # Set hover label background color to grey
      )
  })
  
  # Bubble Chart: Goals vs Matches per Year
  output$bubbleChart <- renderPlotly({
    p <- ggplot(bubble_data, aes(x = YEAR, y = MATCHES, size = size, color = `GOALS SCORED`)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 15)) +
      scale_color_gradient(low = "darkslategray2", high = "blue3") + # Shades of red
      labs(x = "Year", y = "Number of Matches") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })

  
  # Line chart: Team Performance Over Time
  output$linePerformance <- renderPlotly({
    # If no countries selected, render a blank plot with placeholder axis labels
    if (length(input$selected_countries) == 0) {
      empty_data <- data.frame(Year = numeric(0), points = numeric(0), Team = character(0))
      p <- ggplot(empty_data, aes(x = Year, y = points, color = Team)) +
        geom_blank() + 
        labs(x = "Year", y = "Points", title = "No Country selected") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5, color = "gray"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()
        )
      return(ggplotly(p))
    }
    
    # Filter data for selected countries
    selected_countries_data <- team_performance %>%
      filter(Team %in% input$selected_countries)
    
    if (nrow(selected_countries_data) == 0) {
      return(NULL)  # If no data for selected countries, do not render the plot
    }
    
    # Create the plot with hover text customization
    p <- ggplot(selected_countries_data, aes(x = Year, y = points, color = Team, group = Team, 
                                             text = paste("Country: ", Team, "<br>Year: ", Year, "<br>Points Scored: ", points))) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Points", title = "Team Performance Over Time") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Convert ggplot to plotly and add custom hover text
    ggplotly(p, tooltip = "text")  # The `tooltip = "text"` ensures the custom hover text is shown
  })

  # Heatmap: Goals Scored per Year and Team for Selected Countries
  output$heatmapGoals <- renderPlotly({
    # If no countries selected, render a blank heatmap with placeholder axis labels
    if (length(input$selected_countries) == 0) {
      empty_data <- data.frame(Year = integer(0), Team = character(0), goals_scored = numeric(0))
      
      # Create a blank plot with placeholder axis labels
      p <- ggplot(empty_data, aes(x = Team, y = Year, fill = goals_scored)) +
        geom_blank() +  # Use geom_blank to create an empty plot without tiles
        scale_fill_gradient(low = "beige", high = "darkcyan", name = "Goals Scored") +
        labs(x = "Year", y = "Team", title = "No Country selected") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5, color = "gray"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()
        )
      
      return(ggplotly(p))
    }
    
    # Filter data for selected countries
    selected_goals_data <- goals_per_team_year %>%
      filter(Team %in% input$selected_countries)
    
    if (nrow(selected_goals_data) == 0) {
      return(NULL)  # If no data for selected countries, do not render the heatmap
    }
    
    # Create the heatmap with hover text customization
    p <- ggplot(selected_goals_data, aes(x = Year, y = Team, fill = goals_scored,
                                         text = paste("Team: ", Team, "<br>Year: ", Year, "<br>Goals Scored: ", goals_scored))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "beige", high = "darkcyan", name = "Goals Scored") +
      labs(x = "Year", y = "Team", title = "Goals Scored per Year and Team") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    # Convert ggplot to plotly and add custom hover text
    ggplotly(p, tooltip = "text")  # The `tooltip = "text"` ensures the custom hover text is shown
  })
  
  
  
  # Bar Graph: Top 3 Teams by Points for the Selected Year
  output$positionBarChart <- renderPlotly({
    # Check if a year is selected or if no data is available for the selected year
    if (is.null(input$selected_year) || input$selected_year == "" || 
        nrow(fifa_data %>% filter(Year == input$selected_year)) == 0) {
      # Return an empty plot if no year is selected or no data is available
      p <- ggplot() + 
        labs(x = "Points", y = "Team", title = "No year selected") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5, color = "gray"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
        )
      return(ggplotly(p))
    }
    
    # Filter and prepare the data for the selected year
    selected_year_data <- fifa_data %>%
      filter(Year == input$selected_year) %>%
      arrange(desc(Points)) %>%
      slice(1:3) %>%
      mutate(Position = 1:3)
    
    # Assign colors based on position
    selected_year_data$Color <- case_when(
      selected_year_data$Position == 1 ~ "goldenrod1",
      selected_year_data$Position == 2 ~ "silver",
      selected_year_data$Position == 3 ~ "chocolate4"
    )
    
    # Assign labels to the colors for the legend
    selected_year_data$PositionLabel <- case_when(
      selected_year_data$Position == 1 ~ "Champions",
      selected_year_data$Position == 2 ~ "Runner Up",
      selected_year_data$Position == 3 ~ "Third Place"
    ) 
    
    # Plot
    p <- ggplot(selected_year_data, aes(x = Points, y = reorder(Team, Points), fill = PositionLabel, text = paste("Team: ", Team, "<br>Points: ", Points))) +
      geom_bar(stat = "identity",width=0.8) +
      labs(x = "Points", y = "Team", title = "", fill = "Positions") +  # Set the legend title to "Positions"
      scale_fill_manual(values = c("Champions" = "goldenrod1", "Runner Up" = "gray87", "Third Place" = "chocolate4")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        legend.position = "bottom"  # Position the legend at the bottom
      )
    
    ggplotly(p, tooltip = "text")  # Set tooltip to show team name and points
  })
  
  
  # Top 3 Countries with Most Goals Scored in the Selected Year
  output$topGoalsScored <- renderPlotly({
    # Check if a year is selected or if no data is available for the selected year
    if (is.null(input$selected_year) || input$selected_year == "" || 
        nrow(fifa_data %>% filter(Year == input$selected_year)) == 0) {
      # Return an empty plot with a message if no year is selected or no data is available
      p1 <- ggplot() + 
        labs(x = "Team", y = "Goals Scored", title = "No Year Selected") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5, color = "gray"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
        )
      return(ggplotly(p1))
    }
    
    # Filter and arrange data to get top 3 teams with most goals scored for the selected year
    top_goals_scored <- fifa_data %>%
      filter(Year == input$selected_year) %>%
      arrange(desc(`Goals For`)) %>%
      slice(1:3)  # Get top 3 teams
    
    # Assign specific colors to the top 3 countries
    colors <- c("gold", "goldenrod4", "goldenrod")
    
    # Plot with custom hover text
    p1 <- ggplot(top_goals_scored, aes(x = reorder(Team, `Goals For`), y = `Goals For`, fill = Team, 
                                       text = paste("Country: ", Team, "<br>Goals Scored: ", `Goals For`))) +
      geom_bar(stat = "identity") +
      labs(x = "Team", y = "Goals Scored", title = "Top 3 Offensive Countries") +
      scale_fill_manual(values = colors) +  # Apply the gold and goldenrod colors
      theme_minimal() +
      theme(legend.position = "none")
    
    # Convert ggplot to plotly with custom hover text
    ggplotly(p1, tooltip = "text")  # Use 'text' to show the custom hover text
  })

  # Top 3 Countries with Most Goals Conceded in the Selected Year
  output$topGoalsConceded <- renderPlotly({
    # Check if a year is selected or if no data is available for the selected year
    if (is.null(input$selected_year) || input$selected_year == "" || 
        nrow(fifa_data %>% filter(Year == input$selected_year)) == 0) {
      # Return an empty plot with a message if no year is selected or no data is available
      p2 <- ggplot() + 
        labs(x = "Team", y = "Goals Conceded", title = "No Year Selected") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5, color = "gray"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
        )
      return(ggplotly(p2))
    }
    
    # Filter and arrange data to get top 3 teams with most goals conceded for the selected year
    top_goals_conceded <- fifa_data %>%
      filter(Year == input$selected_year) %>%
      arrange(desc(`Goals Against`)) %>%
      slice(1:3)  # Get top 3 teams
    
    # Define colors for the top 3 countries
    colors <- c("salmon2", "salmon3", "salmon4")
    
    # Plot with custom hover text
    p2 <- ggplot(top_goals_conceded, aes(x = reorder(Team, `Goals Against`), y = `Goals Against`, 
                                         fill = factor(Team),  # Use Team as a factor for colors
                                         text = paste("Country: ", Team, "<br>Goals Conceded: ", `Goals Against`))) +
      geom_bar(stat = "identity") +
      labs(x = "Team", y = "Goals Conceded", title = "Worst 3 Defensive Countries") +
      scale_fill_manual(values = colors) +  # Apply red, orange, and purple colors
      theme_minimal() +
      theme(legend.position = "none")
    
    # Convert ggplot to plotly with custom hover text
    ggplotly(p2, tooltip = "text")  # Use 'text' to show the custom hover text
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
