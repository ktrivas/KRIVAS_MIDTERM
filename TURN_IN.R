library(shiny)
library(fpp3)
library(ggeasy)
library(ggpubr)
library(shinydashboard)
library(shinycssloaders)
library(quantmod)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinyWidgets)
library(dashboardthemes)
library(regclass)
# Path where data is
file_path <- "/Users/keltonrivas/Documents/Classes Spring 2022/BAS 475/Midterm/multiTimeline.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Week", "ApexInterest", "FortniteInterest")
# Convert Month to date. Changing to week instead since data file is in weeks.
g_trends$Week <- yearweek(g_trends$Week)
# Convert to tsibble
g_trends <- tsibble(g_trends)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$ApexInterest <- as.numeric(
  ifelse(g_trends$ApexInterest == "<1", 0, g_trends$ApexInterest)
)
g_trends$FortniteInterest <- as.numeric(
  ifelse(g_trends$FortniteInterest == "<1", 0, g_trends$FortniteInterest)
)
g_trends %>% 
  gg_season(ApexInterest, labels = "both") +
  labs (y = "Search Interest Over Time",
        title = "Seasonal Search Interest  - Apex Legends") + easy_center_title() ->
  ApexSeasonality #storing into variable

#Fortnite Seasonality
g_trends %>% 
  gg_season(FortniteInterest, labels = "both") +
  labs (y = "Search Interest Over Time",
        title = "Seasonal Search Interest  - Fortnite") + easy_center_title() ->
  FortSeasonality #storing into variable

#Apex Autocorrelation
g_trends %>% 
  ACF(ApexInterest) %>% autoplot() +
  labs(title = "Apex Search Interest White Noise") + easy_center_title() ->#note white noise!  Trended b/c positive values initially
  #that slowly decrease.
  ApexAuto #storing graph into variable


#Fortnite Autocorrelation
g_trends %>% 
  ACF(FortniteInterest) %>% autoplot() +
  labs(title = "Fortnite Search Interest White Noise") + easy_center_title() ->#Both trended 
  #and seasonal?! NOT white noise!
  FortAuto #storing into variable


my_g_trends_data <- g_trends %>% 
  select(Week,ApexInterest,FortniteInterest)

long_g_trends_data <- my_g_trends_data %>% 
  pivot_longer(-Week)
head(long_g_trends_data)

long_g_trends_data %>% 
  model(classical_decomposition(value)) %>% 
  components() %>% 
  autoplot() +
  easy_center_title() ->
  DECOMP



ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Time Series Analysis"),
                    dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem("Full Time Series", tabName = "tab1"),
                                  menuItem("Seasonality - Apex",tabName = "tab2"),
                                  menuItem("Seasonality - Fortnite",tabName = "tab3"),
                                  menuItem("Autocorrelation - Apex", tabName = "tab4"),
                                  menuItem("Autocorrelation - Fortnite",tabName = "tab5"),
                                  menuItem("Apex & Fortnite Decomposition", tabName = "tab6"))
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("tab1",
                                plotlyOutput("plot1"),
                                textOutput("text1")),
                        tabItem("tab2",
                                plotlyOutput("plot2"),
                                textOutput("text2")),
                        tabItem("tab3",
                                plotlyOutput("plot3"),
                                textOutput("text3")),
                        tabItem("tab4",
                                plotOutput("plot4"),
                                textOutput("text4")),
                        tabItem("tab5",
                                plotOutput("plot5"),
                                textOutput("text5")),
                        tabItem("tab6",
                                plotlyOutput("plot6"),
                                textOutput("text6"))
                        
                      
                      )
                      
                    
                      )
                      
                    )


server <- function(input, output) { 
  
  output$text1 <- renderText(("This infographic is a time-series graph for both Fortnite and Apex Legends. Fortnite is in red and Apex Legends is in blue. Fortnite began with a strong trend upward upon release and stay consistent until the beginning of 2019. Then, it began to have a moderate decreasing trend combined with moderate seasonality as well. The dips in the data are associated with decreased interest in the game with each season; while the following peaks are associated with the release of new seasons and renewed search interest. "))
  output$plot1 <- renderPlotly({
    
    X <- ggplot() + 
      geom_line(data=g_trends, aes(x=Week, y = ApexInterest), color = "blue") +
      geom_line(data=g_trends, aes(x=Week, y = FortniteInterest), color = "red") +
      labs(y = "Search Interest", x = "Week", title = "Search Interest Apex Legends Vs. Fortnite") +
      theme_gray() +
      theme(legend.position = "right") +
      easy_center_title()
    ggplotly(X)
  })
  output$text2 <- renderText(("Apex Legends demonstrates little seasonality. There is evidence that renewed search interest happens every 2 or 3 months with the release of a new season, but the renewed interest has been steadily decreasing with time, even with the release of a new season."))
  output$plot2 <- renderPlotly({
    
   ggplotly(ApexSeasonality)
  })
  output$text3 <- renderText(("Fortnite has very clear evidence of seasonality. The search interest for almost every year is highest in October, and the lowest point for each year is roughly in November as well—possibly in anticipation of events at the end of the year, where search interest spikes again. "))
  output$plot3 <- renderPlotly({
    ggplotly(FortSeasonality)
  })
  output$text4 <- renderText(("Apex has clear signs of a strong downward trend that cannot be attributed to random chance alone. This is demonstrated from the ACF or autocorrelation plot. The 6 beginning points outside of the line are the closest points attributed to the games release and presumably when ad spending was the highest to help consumers become aware of the game. Apex lacks seasonal effects because there are no spikes or dips that are repeated every two lags, for example. "))
  output$plot4 <- renderPlot({
    plot(ApexAuto)
  })
  output$text5 <- renderText(("Fortnite has very clear signs of both trending and seasonality presented in the ACF plot. The slow decrease in the ACF as the lags increase is due to the trend, while the even, downward slop is due to the seasonality. Additionally, because every lag is outside of the blue bounds, this cannot be attributed to random chance alone. "))
  output$plot5 <- renderPlot({
    plot(FortAuto)
  })
  output$text6 <- renderText(("Apex Legends that shows the three components of forecasting: trend, seasonality and remainder. Apex is most affected by trend because trend has the largest scale.
                              Fortnite is most impacted by seasonality. The seasonal pattern at the beginning is different than the seasonal pattern at the end, demonstrating the decrease in search term interest as the game gets older. "))
  output$plot6 <- renderPlotly({
    ggplotly(DECOMP)
    
  })
  
  
  
}

shinyApp(ui, server)
