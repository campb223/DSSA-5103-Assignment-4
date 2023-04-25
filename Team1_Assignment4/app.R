library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)
library(DT)

#reading in our data (netflix)
netflix <- read.csv("../data/netflix.csv")
netflix_rec <- read.csv("../data/netflix.csv") %>%
  select(-show_id, -date_added)

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
      menuItem("Pick Title For Me", tabName = "titlePicker", icon = icon("thumbs-up")),
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
       }
        "
      )
    ),
    tabItems(
      tabItem(tabName = "summary", 
              fluidRow(
                column(12,
                       fluidRow(
                         column(4, valueBoxOutput("num_movies", width="100%")),
                         column(4, valueBoxOutput("num_tv_shows", width="100%"))
                       ),
                ),
              ),
              fluidRow(
                column(12,
                       fluidRow(
                         column(4, plotOutput("movie_rating_plot", height = "302px", width="100%")),
                         column(4, plotOutput("tv_rating_plot", height = "302px", width="100%")),
                         column(4, plotOutput("top_directors_plot"))
                       ),
                ),
              ),
              fluidRow(
                column(8, plotlyOutput("map_plot", height = "450px")),
              ),
      ),
      tabItem(tabName = "titlePicker",
              fluidRow(
                class = "my-fluid-row",
                column(
                  width = 6,
                  uiOutput('prompts'),
                ),
              ),
              conditionalPanel(
                condition = "!$.isEmptyObject($('#rating_picker').val())",
                
                         
                           uiOutput('randTable')
                         
                
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
  
  title_type_picker = ""
  rating_picker = ""
  duration_picker = ""
  category_1 = ""
  category_2 = ""
  category_3 = ""
  
  output$prompts <- renderUI({
    tags$p(
      "Would you like to watch a movie or tv show?", HTML("<br>"),
      actionBttn("movie_pick_filter", label = "Movie"),
      actionBttn("tv_pick_filter", label = "TV Show"),
      
    )
  })
  
  observeEvent(input$movie_pick_filter, {
    title_type_picker = "Movie"
    output$prompts <- renderUI({
      tags$p(
        "Select a rating", HTML("<br>"),
        actionBttn("rating_picker1", label = "G"),
        actionBttn("rating_picker2", label = "PG"),
        actionBttn("rating_picker3", label = "PG-13"),
        actionBttn("rating_picker4", label = "R"),
        actionBttn("rating_picker5", label = "NC-17"),
        actionBttn("rating_picker6", label = "NR"),
        actionBttn("rating_picker7", label = "UR"),
      )
    })
  })
  
  observeEvent(input$tv_pick_filter, {
    title_type_picker = "TV Show"
    output$prompts <- renderUI({
      tags$p(
        "Select a rating", HTML("<br>"),
        actionBttn("rating_picker8", label = "TV-Y"),
        actionBttn("rating_picker9", label = "TV-Y7"),
        actionBttn("rating_picker10", label = "TV-Y7-FV"),
        actionBttn("rating_picker11", label = "TV-G"),
        actionBttn("rating_picker12", label = "TV-PG"),
        actionBttn("rating_picker13", label = "TV-14"),
        actionBttn("rating_picker14", label = "TV-MA"),
      )
    })
  })
  
  # observe the event when the user selects a rating
  observeEvent(input$rating_picker1, {
    rating_picker("G")
  })
  observeEvent(input$rating_picker2, {
    rating_picker("PG")
  })
  observeEvent(input$rating_picker3, {
    rating_picker("PG-13")
  })
  observeEvent(input$rating_picker4, {
    rating_picker("R")
  })
  observeEvent(input$rating_picker5, {
    rating_picker("TC-17")
  })
  observeEvent(input$rating_picker6, {
    rating_picker("NR")
  })
  observeEvent(input$rating_picker7, {
    rating_picker("UR")
  })
  observeEvent(input$rating_picker8, {
    rating_picker("TV-Y")
  })
  observeEvent(input$rating_picker9, {
    rating_picker("TV-Y7")
  })
  observeEvent(input$rating_picker10, {
    rating_picker("TV-Y7-FV")
  })
  observeEvent(input$rating_picker11, {
    rating_picker("TV-G")
  })
  observeEvent(input$rating_picker12, {
    rating_picker("TV-PG")
  })
  observeEvent(input$rating_picker13, {
    rating_picker("TV-14")
  })
  observeEvent(input$rating_picker14, {
    rating_picker("TV-MA")
  })
  
  # define a reactive expression to filter the data based on selected rating and title type
  filtered_data <- reactive({
    netflix_rec %>%
      filter(rating == input$rating_picker, title_type == input$title_type_picker)
  })
  
  # define a reactive expression to randomly select 10 rows from the filtered data
  random_data <- reactive({
    filtered_data() %>%
      sample_n(10)
  })
  
  # render the table with the randomly selected data
  output$randTable <- renderDT({
    dataTable(random_data())
  })
  
  #rating_picker_list = c(input$rating_picker1, input$rating_picker2, input$rating_picker3,input$rating_picker4,input$rating_picker5,input$rating_picker6,
  #                       input$rating_picker7,input$rating_picker8,input$rating_picker9,input$rating_picker10,input$rating_picker11,input$rating_picker12,
  #                       input$rating_picker13,input$rating_picker14)
  
  
  #https://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
  
  filtered_data <- reactive({
    if(input$type_filter == "All"){
      if(is.null(input$rating_filter) & input$country_filter == "All"){
        netflix_rec
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, country == input$country_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, rating %in% input$rating_filter)
      } else {
        filter(netflix_rec, rating %in% input$rating_filter, country == input$country_filter)
      }
    } else if(input$type_filter == "Movie"){
      if(is.null(input$rating_filter) & input$country_filter == "All"){
        filter(netflix_rec, type == "Movie")
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, type == "Movie", country == input$country_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, type == "Movie", rating %in% input$rating_filter)
      } else {
        filter(netflix_rec, type == "Movie", rating %in% input$rating_filter, country == input$country_filter)
      }
    } else if(input$type_filter == "TV Show"){
      if(is.null(input$rating_filter) & input$country_filter == "All"){
        filter(netflix_rec, type == "TV Show")
      } else if(is.null(input$rating_filter)){
        filter(netflix_rec, type == "TV Show", country == input$country_filter)
      } else if(input$country_filter == "All"){
        filter(netflix_rec, type == "TV Show", rating %in% input$rating_filter)
      } else {
        filter(netflix_rec, type == "TV Show", rating %in% input$rating_filter, country == input$country_filter)
      }
    }
  })
  
  # Define reactive expression to filter based on search term
  filtered_data_search <- reactive({
    if (input$search_text != "") {
      filter(netflix_rec, grepl(input$search_text, paste(netflix_rec$title, netflix_rec$director, netflix_rec$country, netflix_rec$rating, netflix_rec$duration, netflix_rec$listed_in), ignore.case = TRUE))
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
      theme_bw() + theme(#axis.text = element_blank(),
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
      labs(title = "capt", x = NULL, y = NULL, caption = "capt") +
      my_theme() 
    
    
    ggplotly(world_map)
    
  })
  
  
  #output$map_plot <- renderPlotly({
  #  netflix %>%
  #    group_by(country) %>%
  #    summarise(n = n()) %>%
  #    plot_ly(z = ~n, type = "choropleth", locations = ~country, locationmode = "country names",
  #            colors = "Reds", marker = list(line = list(color = "white", width = 2))) %>%
  #    colorbar(title = "Number of Titles") %>%
  #    layout(title = "Netflix Titles by Country")
  #})
  
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

}

# Run the app
shinyApp(ui, server)