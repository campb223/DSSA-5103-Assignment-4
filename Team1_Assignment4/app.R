library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(DT)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)


#reading in our data (netflix)
netflix <- read.csv("netflix_cat.csv")
netflix_rec <- read.csv("netflix_cat.csv") %>%
  select(-show_id, -date_added)

# read the Netflix dataset
df <- read.csv('cat_count.csv')

#Dashboard header
header <- dashboardHeader(
  title = "Netflix"
)

# Define UI 
ui <- dashboardPage(
  title = 'Netflix Dashboard',
  header,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Categories", tabName = "categories", icon = icon("folder-open")),
      menuItem("All Movies / TV Shows", tabName = "recommendations", icon = icon("table")),
      tags$div(
        style = "position: fixed; bottom: 0;",
        
        tags$p(
          "Created by: ", HTML("<br>"), "Dale Campbell", HTML("<br>"), "Jenna Kobular", HTML("<br>"), "Emil Bobev", HTML("<br><br>"),
        ),
        
        tags$a(
          href="https://github.com/campb223/DSSA-5103-Assignment-4", target="_blank",
          icon("github"),
          "View Source on Github"
        )
      )
    )
  ),
  dashboardBody(
    tags$style(
      HTML(
        "
        .skin-red .main-sidebar {
          background-color: #000000;
        }
        .main-sidebar {
          min-height: 100%;
        }
       .tab-content {
         padding: 0px 0px 0px 0px;
       }
       .dashboard-body {
          overflow-x: auto;
          overflow-y: auto;
          padding: 10px 0px 0px 10px;
       }
       .my-fluid-row {
          display: flex;
          align-items: center;
          margin-bottom: 25px;
       }
        "
      )
    ),
    tabItems(
      tabItem(tabName = "summary", 
              fluidRow(
                column(8,
                       fluidRow(
                         column(6, valueBoxOutput("num_movies", width="100%")),
                         column(6, valueBoxOutput("num_tv_shows", width="100%"))
                       ),
                       fluidRow(class = "my-fluid-row",
                         column(6, plotOutput("movie_rating_plot", height = "300px")),
                         column(6, plotOutput("tv_rating_plot", height = "300px")),
                       ),
                       
                       fluidRow(
                         column(12, plotlyOutput("map_plot", height = "420px"))
                       ),
                       
                ),
                column(4,
                       fluidRow(class = "my-fluid-row",
                         column(12, plotOutput("top_directors_plot", height = "420px")),
                       ),
                       fluidRow(
                         column(12, plotOutput("year_plot", height = "420px"))
                       )
                )
              )
      ),
      tabItem(tabName = "categories",
              fluidRow(
                column(4, valueBoxOutput("num_movies2", width="100%")),
                column(4, valueBoxOutput("num_tv_shows2", width="100%")),
                column(4, valueBoxOutput("num_docs2", width="100%"))
              ),
              fluidRow(
                column(4,
                       plotOutput("movies_plot", height = "600px")
                ),
                column(4,
                       plotOutput("tv_shows_plot", height = "600px")
                ),
                column(4,
                       plotOutput("documentaries_plot", height = "600px")
                )
              )
      ),
      tabItem(tabName = "recommendations",
                fluidRow(class = "my-fluid-row",
                  column(
                    width = 3,
                    textInput("search_text", "Search"),
                    selectInput(
                      "type_filter", "Type",
                      choices = c("All", "Movie", "TV Show"), selected = "All"
                    ),
                    selectInput(
                      "rating_filter", "Rating",
                      choices = c(
                        "G", "TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "PG",
                        "TV-PG", "PG-13", "TV-14", "R", "NC-17", "TV-MA", "NR", "UR"
                      ),
                      selected = NULL, multiple = TRUE
                    ),
                    selectInput(
                      "year_filter", "Year",
                      choices = c("All",rev(sort(unique(netflix_rec$release_year)))),
                      selected = "All", multiple = TRUE
                    ),
                    selectInput(
                      "country_filter", "Country",
                      choices = c("All", sort(unique(netflix_rec$country))),
                      selected = "All", multiple = TRUE
                    ),
                    selectInput(
                      "cat1_filter", "Category 1",
                      choices = c("All", sort(unique(netflix_rec$cat1))),
                      selected = "All", multiple = TRUE
                    ),
                    selectInput(
                      "cat2_filter", "Category 2",
                      choices = c("All", sort(unique(netflix_rec$cat2))),
                      selected = "All", multiple = TRUE
                    ),
                    selectInput(
                      "cat3_filter", "Category 3",
                      choices = c("All", sort(unique(netflix_rec$cat3))),
                      selected = "All", multiple = TRUE
                    )
                  ),
                    fluidRow(
                      column(
                        width = 12,
                        div(
                          style = "overflow-y: scroll;",
                          dataTableOutput("filtered_table_data")
                        )
                      )
                    )
                  
                )
      )
    )
  ),
  skin='red'
)

server <- function(input, output, session) {
  
  
  ## SUMMARY SECTION ----------------------------------------------------------
  
  # Number of Movies
  output$num_movies <- renderValueBox({
    valueBox(paste0(nrow(filter(netflix, type == "Movie")), " "),
             subtitle = "Movies",
             icon = icon("film"),
             color = "red"
             
    )
  })
  
  # Number of TV Shows
  output$num_tv_shows <- renderValueBox({
    valueBox(paste0(nrow(filter(netflix, type == "TV Show")), " "),
             subtitle = "TV Shows",
             icon = icon("tv"),
             color = "red"
             
    )
  })
  
  # Map of Netflix titles by country
  output$map_plot <- renderPlotly({
    
    # Read the world countries geometries
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Summarize the Netflix dataset by country and count the number of occurrences of each country
    netflix_summary <- netflix %>%
      group_by(country) %>%
      summarise(n = n()) # %>%
    
    # Join the Netflix summary data with the world geometries on both the name and admin columns
    world_data <- world %>%
      st_make_valid() %>%
      left_join(netflix_summary, by = c("name" = "country")) %>%
      st_as_sf()
    
    # Replace NA values with 0
    world_data$n[is.na(world_data$n)] <- 0
    
    # Calculate the centroids of the country polygons
    world_data$centroid <- st_centroid(world_data)
    
    # Extract the latitude and longitude coordinates
    world_data$lon <- st_coordinates(world_data$centroid)[, 1]
    world_data$lat <- st_coordinates(world_data$centroid)[, 2]
    
    # Create a color palette function
    pal <- colorRampPalette(c("white", "red"))
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
      theme_bw() + theme(axis.text = element_blank(),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_blank(), 
        strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    world_map <- ggplot(world_data) +
      geom_sf(aes(fill = n, text = paste0("Total films: ", n)), color = "grey", size = 0.1) +
      scale_fill_gradientn(colours = pal(100), na.value = 'black') + 
      scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      labs(title = "Releases By Country", x = NULL, y = NULL, caption = "Releases By Country") +
      my_theme() 
    
    
    ggplotly(world_map)
    
  })
  
  # Define the levels in the desired order
  rating_levels <- c("G", "TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "PG", "TV-PG", "PG-13", "TV-14", "R", "NC-17", "TV-MA", "NR", "UR")
  
  movie_ratings <- c("G", "PG", "PG-13", "R", "NC-17", "NR", "UR")
  tv_ratings <- c("TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "TV-PG", "TV-14", "TV-MA")
  
  # Create a new factor variable with the ordered levels
  netflix$rating_ordered <- factor(netflix$rating, levels = rating_levels)
  
  # Histogram of Netflix ratings
  output$rating_plot <- renderPlot({
    ggplot(netflix, aes(x = rating_ordered)) +
      geom_bar(fill = "#e50914") +
      labs(x = "Rating", y = "Count") +
      ggtitle("Distribution of Netflix Ratings") +
      facet_wrap(~ type, ncol = 1)
  })
  
  # Histogram of Movie Ratings
  output$movie_rating_plot <- renderPlot({
    ggplot(filter(netflix, rating %in% movie_ratings), aes(x = rating_ordered)) +
      geom_bar(fill = "#000000") +
      labs(x = "Rating", y = "Count") +
      ggtitle("Distribution of Movie Ratings") 
  })
  
  # Histogram of TV Show Ratings
  output$tv_rating_plot <- renderPlot({
    ggplot(filter(netflix, rating %in% tv_ratings), aes(x = rating_ordered)) +
      geom_bar(fill = "#000000") +
      labs(x = "Rating", y = "Count") +
      ggtitle("Distribution of TV Show Ratings") 
  })
  
  output$year_plot <- renderPlot({
    
    #extracting the year out of the date_added date 
    netflix$date_added <- format(as.Date(netflix$date_added, format="%d/%m/%Y"), "%Y")
    
    netflix_summary <- netflix %>%
      group_by(release_year, type ) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    type <- netflix %>%
      group_by(type, release_year) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Create a bar plot of the count of movies and TV shows by release year
    ggplot(netflix_summary, aes(x = release_year, y = count, color = type)) +
      geom_line(alpha = 0.5, size = 2) +
      scale_fill_manual(values = c("#FF9933", "#3366CC")) +
      labs(title = "Number of Movies and TV Shows Released by Year", 
           x = "Release Year", y = "Count") +
      theme_classic() +
      theme(axis.ticks = element_blank()) +
      xlim(1975, 2022)
  })

  
  output$top_directors_plot <- renderPlot({
    netflix %>% filter(director != "Not Given") %>%
      group_by(director) %>%
      summarise(count = n()) %>%
      top_n(10, count) %>%
      ggplot(aes(x = reorder(director, count), y = count)) +
      geom_col(fill = "red") +
      coord_flip() +
      theme_classic()+
      labs(title = "Top 10 Directors by Count", x = NULL, y = "Count")
  })
  
  
  ## Categories SECTION ------------------------------------------------------
  
  # Number of Movies
  output$num_movies2 <- renderValueBox({
    valueBox(paste0("5122"),
             subtitle = "Movies",
             icon = icon("film"),
             color = "red"
             
    )
  })
  
  # Number of TV Shows
  output$num_tv_shows2 <- renderValueBox({
    valueBox(paste0("2222"),
             subtitle = "TV Shows",
             icon = icon("tv"),
             color = "red"
             
    )
  })
  
  # Number of Documentaries
  output$num_docs2 <- renderValueBox({
    valueBox(paste0("829"),
             subtitle = "Documentaries",
             icon = icon("far fa-file-video"),
             color = "red"
    )
  })
  
  output$movies_plot <- renderPlot({
    # Create a bar plot of subcategory vs. count
    ggplot(filter(df, category == "Movies"), aes(x = count, y = reorder(subcategory, -desc(count)))) +
      geom_bar(stat = "identity") +
      labs(x = "Count", y = element_blank(), title = "Count of Subcategories in Movies")
  })
  
  output$tv_shows_plot <- renderPlot({
    # Create a bar plot of subcategory vs. count
    ggplot(filter(df, category == "TV Shows"), aes(x = count, y = reorder(subcategory, -desc(count)))) +
      geom_bar(stat = "identity") +
      labs(x = "Count", y = element_blank(), title = "Count of Subcategories in TV Shows")
  })
  
  output$documentaries_plot <- renderPlot({
    # Create a bar plot of subcategory vs. count
    ggplot(filter(df, category == "Documentaries"), aes(x = count, y = reorder(subcategory, -desc(count)))) +
      geom_bar(stat = "identity") +
      labs(x = "Count", y = element_blank(), title = "Count of Subcategories in Documentaries")
  })
  
  
  ## DATATABLE SECTION ---------------------------------------------------------
  
  filtered_data <- reactive({
    if(input$type_filter == "All"){
      if(is.null(input$rating_filter) & input$country_filter == "All"
         & input$cat1_filter == "All" & input$cat2_filter == "All" & input$cat3_filter == "All"){
        netflix_rec
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, rating %in% input$rating_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else {
        filter(netflix_rec, rating %in% input$rating_filter, country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      }
    } else if(input$type_filter == "Movie"){
      if(is.null(input$rating_filter) & input$country_filter == "All"
         & input$cat1_filter == "All" & input$cat2_filter == "All" & input$cat3_filter == "All"){
        filter(netflix_rec, type == "Movie")
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, type == "Movie", country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, type == "Movie", rating %in% input$rating_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else {
        filter(netflix_rec, type == "Movie", rating %in% input$rating_filter, country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      }
    } else if(input$type_filter == "TV Show"){
      if(is.null(input$rating_filter) & input$country_filter == "All"
         & input$cat1_filter == "All" & input$cat2_filter == "All" & input$cat3_filter == "All"){
        filter(netflix_rec, type == "TV Show")
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, type == "TV Show", country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, type == "TV Show", rating %in% input$rating_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      } else {
        filter(netflix_rec, type == "TV Show", rating %in% input$rating_filter, country == input$country_filter,
               cat1 %in% input$cat1_filter, cat2 %in% input$cat2_filter, cat3 %in% input$cat3_filter)
      }
    }
  })
  
  # Define reactive expression to filter based on search term
  filtered_data_search <- reactive({
    if (input$search_text != "") {
      filter(netflix_rec, grepl(input$search_text, paste(netflix_rec$title, netflix_rec$director, netflix_rec$country, netflix_rec$rating, netflix_rec$duration, netflix_rec$cat1, netflix_rec$cat2, netflix_rec$cat3), ignore.case = TRUE))
    } else {
      filtered_data()
    }
  })
  
  output$filtered_table_data <- renderDataTable({
    filtered_data_search()
  }, options = list(
    pageLength = 10, 
    lengthMenu = c(5, 10, 15, 20, 25), 
    searching = FALSE,
    buttons = list(
      'colvis',
      list(
        background = "#e50914",
        textColor = "#000000"
      )
    )
  )
  )
  
}

# Run the app
shinyApp(ui, server)