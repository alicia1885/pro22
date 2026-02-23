library(tidyverse)
library(shiny)

fish_raw <- read_csv(
  "https://gist.githubusercontent.com/5chang2/6923330a794da4f4e05c06599c592914/raw/fish.csv",
  show_col_types = FALSE
)
fish_df <- fish_raw %>%
  drop_na() %>%
  mutate(
    Species = factor(Species, levels = sort(unique(Species)))
  )
reset_dist <- function(x, click) {
  nearPoints(
    x, click, xvar = "Length2", yvar = "Height", allRows = TRUE,addDist = TRUE)$dist_
  }#nearPoints returns a data frame with a distance column (dist_).
# We extract dist_ so each point gets a distance to the click location.

reset_selection <- function(x, brush) {
  brushedPoints(
    x, brush, xvar = "Length2", yvar = "Height", allRows = TRUE)$selected_
}#We extract selected_ to track which rows are inside the brush.

scatter_fish <- function(x, selected_, dists) {
  x %>%
    mutate(
      selected_ = selected_, dist = dists
      ) %>%
    ggplot() +
    geom_point(aes(x = Length2, y = Height, color = Species,
      alpha = as.numeric(selected_), size = dist
    )) +
    scale_color_discrete(
      drop = FALSE,
      limits = levels(fish_df$Species)
    ) +
    scale_alpha(range = c(0.10, 1)) +
    scale_size(range = c(6, 1)) +
    labs(
      x = "Length2 (cm)", y = "Height (cm)", color = "Species"
    ) +
    guides(alpha = "none", size = "none") +
    theme_minimal()
}#Build the scatterplot.

ui <- fluidPage(
  h3("Fish Market"),
  p(
    "you can select fish species for clearer analysis; drag to select a batch of samples on the scatter plot; After clicking on a location, the point size will change according to the distance;The table below displays the current selection results, sorted by click distance."
  ),
  selectInput(
    "species_filter", "Choose species",
    choices = c("All", as.character(levels(fish_df$Species))), selected = "All"
  ),
  plotOutput(
    "plot",
    click = "plot_click", brush = "plot_brush"
  ),
  dataTableOutput("table")
)#Build user interface.

server <- function(input, output) {
  filtered <- reactive({
    if (input$species_filter == "All") {
      fish_df
    } else {
      fish_df %>% filter(Species == input$species_filter)
    }
  })
  
  dist <- reactiveVal(rep(1, nrow(fish_df)))
  selected <- reactiveVal(rep(TRUE, nrow(fish_df)))
  # dist: distance from each point to the most recent click
  # selected: whether each point is inside the current brush
  
  observeEvent(filtered(), {
    dist(rep(1, nrow(filtered())))
    selected(rep(TRUE, nrow(filtered())))
  })
  
  observeEvent(
    input$plot_click,
    dist(reset_dist(filtered(), input$plot_click))
  )# Update distance vector using nearPoints().
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(filtered(), input$plot_brush))
  )# Update selected rows using brushedPoints().
  
  output$plot <- renderPlot(
    scatter_fish(filtered(), selected(), dist())
  )# Plot uses filtered data and current click/brush states.
  
  output$table <- renderDataTable({
    filtered() %>%
      mutate(
        selected_ = selected(),
        dist = dist()
      ) %>%
      filter(selected_) %>%
      arrange(dist) %>%
      select(
        Species, Weight, Length1, Length2, Length3, Height, Width,dist
      )
  })# Show only brushed rows, then sort them by click distance.
}#filtered() creates the current dataset based on UI selection, reactiveVal() stores click/brush states, observeEvent() updates states when inputs change, plot and table render from the current reactive states.

shinyApp(ui, server)
