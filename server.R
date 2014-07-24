# server.R

library(ggvis)

#data reading and manipulations
milbases <- read.csv("data/milbases.csv")
reports <- read.csv("data/ufo_reports.csv")
mergeddata <- merge(reports, milbases, by="State")
mergeddata$id <- 1:nrow(mergeddata)

#function to connect a data point to its other variabless in data set for tooltip
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- mergeddata[mergeddata$id == x$id, ]
  paste0("State", ": ", row$State,"<br />", "UFO Reports", ": ", row$UFOReports,"<br />", "Total Bases", ": ", row$Total, "<br />", "Air Force Bases", ": ", row$AirForce,"<br />")
}

shinyServer(
  function(input, output, session) {
      
      #interactive controls
      input_size <- reactive(input$size)
      input_model <- reactive(input$model)
            
      #ggvis plot build
      mergeddata %>% 
        ggvis( x = ~Total, y = ~UFOReports) %>% 
        layer_points(fill := "blue", size := input_size, key := ~id) %>%
        add_tooltip(all_values, "hover") %>%
        layer_model_predictions(model = input_model, se = TRUE) %>%
        add_axis("x", title = "Number of Military Bases", orient = "bottom", title_offset = 50) %>%
        add_axis("y", title = "Num of UFO Reports", orient = "left", title_offset = 50) %>%
        bind_shiny("ggvis", "ggvis_ui")
  }
)
