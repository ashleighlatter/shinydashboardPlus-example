# shinydashboardPlus template

# Load packages
library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(DT)
library(shinydashboardPlus)
library(fresh)

# Update app colours
app_theme <- create_theme(
  adminlte_color(
    light_blue = "#52A1A5", # header bar/primary status
    red = "#E76A66", # danger status
    green = "#53A66A", # success status
    yellow = "#F9CB77", # warning status
  ),
  adminlte_sidebar(
    width = "270px", # sidebar width
    dark_bg = "#F8F3F7", # left and right sidebar colour
    dark_color = "#000000", # text colour
    dark_hover_color = "#000000" # hover text colour
  ),
  adminlte_global(
    content_bg = "#FBFBFD", # main body background
    box_bg = NULL, # box background
    info_box_bg = NULL # info box background
  )
)

plot_colour <- "#BAD9DB" # colour for plotly bar charts

# Load data
data <- read_csv("https://s3.ap-southeast-2.amazonaws.com/dmzweb.adelaidecitycouncil.com/OpenData/Street_Trees/Street_Trees.csv")

# Data wrangling
colnames(data) <- c("Roadname", "Asset_ID", "Circumference", "Vigour", "Structure", "Age", "Height", "Species_Name", "Common_Name", "Easting", "Northing", "Geometry")

data <- data %>% 
  # Grab only the relevant columns
  select(
    Roadname,
    Species_Name,
    Common_Name,
    Circumference,
    Vigour,
    Structure,
    Age,
    Height
  ) %>% 
  # Convert to factors
  mutate(
    Circumference = factor(Circumference, levels = c("0m -1m", "1m - 2m", "2m - 3m", ">3m")),
    Vigour = factor(Vigour, levels = c("Dying tree", "Low vigour", "Medium vigour", "High vigour")),
    Structure = factor(Structure, levels = c("Dead tree", "Poor Structure", "Fair Structure", "Good Structure")),
    Age = factor(Age, levels = c("Juvenile", "Semi mature", "Mature", "Scenescent"))
  ) %>% 
  # Remove rows with NAs
  filter(complete.cases(.))

# Values for info boxes
num_healthy <- data %>% filter(Vigour %in% c("Medium vigour", "High vigour")) %>% nrow()
num_unhealthy <- data %>% filter(Vigour == "Low vigour") %>% nrow()
num_dying <- data %>% filter(Vigour == "Dying tree") %>% nrow()


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  title = "shinydashboard Example",
  freshTheme = app_theme, # specify colour theme from above
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(
    title = tagList(
      tags$img(src = "https://rinterface.com/inst/images/shinydashboardPlus.svg", width = 20), # header icon
      "shinydashboardPlus" # header title text
    ),
    controlbarIcon = icon("circle-info") # update right hand sidebar icon
  ),
  
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart")),
      menuItem("Help", tabName = "help", icon = icon("circle-info"))
    )
    
  ),
  
  controlbar = dashboardControlbar(),
  
  body = dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        infoBox(
          width = 4,
          title = "Healthy trees",
          color = "green",
          value = num_healthy,
          icon = icon("face-smile")
        ),
        
        infoBox(
          width = 4,
          title = "Unhealthy trees",
          color = "yellow",
          value = num_unhealthy,
          icon = icon("face-meh")
        ),
        
        infoBox(
          width = 4,
          title = "Dying trees",
          color = "red",
          value = num_dying,
          icon = icon("face-frown")
        ),
        
        
        box(
          title = "Circumference and Height",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("plot_age_circum")
        ),
        
        box(
          title = "Histogram of Height",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          # box right hand sidebar
          sidebar = boxSidebar(
            id = "id_box_hist",
            background = "#F8F3F7", # sidebar background colour
            
            div(style = "color: #000000;", # sidebar font colour
                
                numericInput(
                  inputId = "num_bins",
                  label = "Number of bins",
                  min = 1,
                  max = 30,
                  value = 15,
                  step = 1,
                  width = "97%"
                ) 
                
            )
          ),
          plotlyOutput("plot_hist")
        ),
        
        box(
          title = "Data",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          closable = TRUE,
          DTOutput("table")
        )
      ),
      
      tabItem(
        tabName = "help"
      )
    )
    
  ),
  
  footer = dashboardFooter(
    left = NULL,
    right = "Template by Ashleigh Latter"
  )
  
)


# Server ------------------------------------------------------------------

server <- function(input, output) { 
  
  output$plot_age_circum <- renderPlotly({
    
    data %>% 
      filter(!is.na(Circumference)) %>% 
      group_by(Circumference) %>% 
      summarise(Avg_Height = mean(Height, na.rm = TRUE)) %>% 
      # Generate plot
      plot_ly(
        x = ~Circumference,
        y = ~Avg_Height,
        name = "Circumference vs Height",
        type = "bar",
        marker = list(color = plot_colour)
      ) %>% 
      layout(
        yaxis = list(title = "Average Height")
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$plot_hist <- renderPlotly({
    
    plot_ly(
      x = data$Height,
      type = "histogram",
      nbinsx = input$num_bins,
      marker = list(color = plot_colour)
    ) %>% 
      layout(
        xaxis = list(title = "Height"),
        yaxis = list(title = "Count")
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$table <- DT::renderDT({
    
    data %>% 
      select(
        Roadname,
        Species_Name,
        Common_Name,
        Circumference,
        Vigour,
        Structure,
        Age,
        Height
      ) 
    
  })
  
}  

shinyApp(ui, server)
