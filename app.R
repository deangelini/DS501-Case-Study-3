
#library(shiny)
library(shinythemes)
library(cluster)
library(dplyr)
library(ggplot2)

#Housing data link: https://www.kaggle.com/datasets/camnugent/california-housing-prices
housing = "https://raw.githubusercontent.com/deangelini/DS501-Case-Study-3/main/housing.csv"
housing_csv = read.csv(housing, header = TRUE)
housing_data = as_tibble(housing_csv)

#Result of Data Cleaning
housing_clean = housing_data
br_median = median(housing_data$total_bedrooms, na.rm = TRUE)
housing_clean$total_bedrooms[is.na(housing_clean$total_bedrooms)] = br_median

#Define UI
ui = fluidPage(theme = shinytheme("sandstone"),
               titlePanel("California Housing Prices"),
               
               sidebarLayout(sidebarPanel(
                 textInput("xvar", "X Variable:", "longitude"),
                 textInput("yvar", "Y Variable:", "latitude"),
                 numericInput("k", label = "Number of Clusters:", min = 2, step = 1 ,max = 15, value = 3),
                 uiOutput("cvar_select")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary",
                            h2("Algorithm: K-Means Clustering"),
                            p("K-means clustering is a distance-based method for cluster analysis in data mining, and
                              it is used for knowledge discovery rather than prediction. The process behind k-means clustering
                              can be broken down into the following steps:"),
                            p("1) Select k points at random (centers)."),
                            p("2) For each data point, assign it to the closest center to form k clusters."),
                            p("3) For each cluster, re-compute the centers. In the case of 2D data points, this will look like
                              taking the average over all the x-axis points in the cluster, taking the average over all the y-axis
                              points in the cluster, and these averages becoming the new position of the center."),
                            p("4) If the new centers are different from the old centers in the previous iteration, repeat steps 2 and 3."),
                            
                            h2("Methodology"),
                            p("Discovery: The purpose of this application is to test how California housing data can be clustered. Based on the 
                              fields provided, variables like median income, median house value, and total bedrooms could have some link
                              to economic status of that clustered region. I will also be looking at population within a block to get an idea of how 
                              population is distributed throughout California."),
                            p("Data Cleaning: Data cleaning is done through looking at a summary of the data to resolve missing values. No data will be
                              ommitted from this exploration."),
                            p("Data Exploration: Data will be explored through looking at box plots and ocean proximity tables to quanitfy and understand the
                              relationship of the chosen variables and their location. This Shiny Application will help to visualize the clusters created for each
                              chosen variable."),
                            
                            h2("1990s California Housing Data Summary"),
                            p("Longitude: A measure of how far west a house is; a higher value is farther west."),
                            p("Latitude: A measure of how far north a house is; a higher value is farther north."),
                            p("Median House Value: Median house value for households within a block (measured in US Dollars)."),
                            p("Median Income: Median income for households within a block of houses (measured in tens of thousands of US Dollars)."),
                            p("Population: Total number of people residing within a block."),
                            p("Total Bedrooms: Total number of bedrooms within a block."),
                            p("Ocean Proximity: Location of the house w.r.t ocean/sea."),
                            
                            h2("Motivation"),
                            p("I am interested in looking into this because usually people associate high class or higher income populations 
                              to living near a coast. Higher income populations can be attested to variables like median house value and median income. Additionally,
                              houses with many bedrooms tend to impact the value of a house, so this variable will also be considered. Visuallizing this data through using the Shiny Application 
                              is also interesting since I get to consider location of the houses with respects to the chosen variable.")),
                   tabPanel("Plot", 
                            h2("Instructions:"),
                            p("Select a housing variable you are interested in clustering on."),
                            p("Choose the desired number of clusters."),
                            h2("Plot Output:"), plotOutput(outputId = "clustplot", width = "100%"))
                 )
               )
               ))

#Define server logic
server = function(input, output, session){
  
  output$cvar_select = renderUI({
    selectInput("cvar", label = "Choose what you want to cluster on:", 
                choices = c("total_bedrooms","population","median_income","median_house_value"))
  })
  
  compute_c = reactive({
    
    data = subset(housing_clean, select = c(input$xvar, input$yvar, input$cvar))
    colnames(data) = c("long", "lat", "cvar")
    
    if(input$k > nrow(unique(data))){
      updateNumericInput(session, "k", value = nrow(unique(data)))
    }
    if(input$k < 2){
      updateNumericInput(session, "k", value = 2)
    }
    
    k_clust = kmeans(data$cvar, input$k)
    data$cluster = k_clust$cluster
    housing_clust = as.data.frame(data)
    return(housing_clust)
    
  })
  
  output$clustplot = renderPlot({
    result = compute_c()
    ggplot(data = result, aes(x = long, y = lat, color = cluster)) + 
      geom_point(size = 3, data = result, aes(x = long, y = lat, color = factor(cluster)), pch = 16, size = 7) +
      ggtitle("California Housing Clustering Result") + xlab("Longitude") + ylab("Latitude") + xlim(-125,-114) + ylim(32,42) + coord_fixed(1.3)
    
  })
}

#Create Shiny app
shinyApp(ui = ui, server = server)
  
  
  