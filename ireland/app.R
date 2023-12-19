library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(readr) 
library(shinyWidgets)
library(dplyr)
library(plotly)
library(DT)
library(rsconnect)
# Load your data here
cities <- read_csv("ie.csv") 
life_exp <- read_csv("life exp.csv")
gdp <- read_csv("gdp.csv")
migration_data <- read_csv("migration.csv")
population <- read_csv("population.csv")

gdp_gb <- read_csv("ireland-gdp-gross-domestic-product.csv")
gdp_iom <- read_csv("isle-of-man-gdp-gross-domestic-product.csv")
gdp_ci <- read_csv("channel-islands-gdp-gross-domestic-product.csv")

standardize_gdp <- function(df) {
  df$Year <- as.integer(format(as.Date(df$date, format="%m/%d/%Y"), "%Y"))
  
  standardized_df <- df %>%
    dplyr::select(Year, GDP, `Per Capita (US $)`, `Annual % Change`)
  
  return(standardized_df)
}

gdp_gb <- standardize_gdp(gdp_gb)
gdp_iom <-  standardize_gdp(gdp_iom)
gdp_ci <-  standardize_gdp(gdp_ci)

economic_data <- read.csv(text = "
2010,2015,2021
GDP (million current US$),222071,291522,398590
GDP growth rate (annual % const. 2015 prices),1.8,25.2,5.6
GDP per capita (current US$),48760.5,62660.2,81636.6
Economy: Agriculture (% of Gross Value Added),1,1,1
Economy: Industry (% of Gross Value Added),25.6,41.3,37.6
Economy: Services and other activity (% of GVA),73.3,57.8,61.4
Employment in agriculture (% of employed),5.8,5.3,4.5
Employment in industry (% of employed),18.1,18.1,18.5
Employment in services (% employed),76.2,76.6,77
Unemployment (% of labour force),14.5,9.9,5.2
Labour force participation rate (female/male pop. %),55.6/70.5,55.1/69.5,55.8/67.9
CPI: Consumer Price Index (2010=100),100,105,106
Agricultural production index (2014-2016=100),88,101,113
International trade: exports (million current US$),120645,124731,184131
International trade: imports (million current US$),64601,77795,98389
International trade: balance (million current US$),56045,46935,85742
Balance of payments current account (million US$),2320,12912,-44934
")



islands_data <- data.frame(
  Island = c("Great Britain", "Isle of Man", "Channel Islands"),
  Lat = c(51.509865, 54.2361, 49.3728),
  Lon = c(-0.118092, -4.5481, -2.3648)
)

ireland_icon <- makeIcon(
  iconUrl = "Flag_of_Ireland copy.svg", # Update with the correct path to your icon
  iconWidth = 25, iconHeight = 25
)



ui <- dashboardPage(
  dashboardHeader(title = HTML("Ireland <img src='Flag_of_Ireland copy.svg' style='height:20px; width:auto;' />")),
  dashboardSidebar(
    sidebarMenu(sidebarMenu(
      menuItem("General Description", tabName = "general"),
      menuItem("Key Demographics", tabName = "demographics"),
      menuItem("Comparative Analysis", tabName = "comparison"),
      menuItem("SWOT Analysis", tabName = "swot"),
      menuItem("Bibliography", tabName = "bibliography",
               menuSubItem(tags$a("Wikipedia/Ireland", href = "https://en.wikipedia.org/wiki/Ireland", target = "_blank")),
               menuSubItem(tags$a("shinyapps.io for Publishing", href = "https://www.shinyapps.io/", target = "_blank")),
               menuSubItem(tags$a("Ireland Table", href = "https://data.un.org/en/iso/ie.html", target = "_blank")),
               menuSubItem(tags$a("Ireland Dataset", href = "https://data.un.org/Search.aspx?q=Ireland+", target = "_blank")),
               menuSubItem(tags$a("Channel Island Dataset", href = "https://www.macrotrends.net/countries/CHI/channel-islands/gdp-gross-domestic-product", target = "_blank")),
               menuSubItem(tags$a("Isle of Man GDP Dataset", href = "https://www.macrotrends.net/countries/IMN/isle-of-man/gdp-per-capita", target = "_blank"))
      )

    )
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "general",
              fluidRow(
                box(
                  title = "Interactive Map of Ireland",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  leafletOutput("map", height = "500px")
                ),
                box(
                  title = "Global Location",
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  leafletOutput("mapGlobal", height = "250px")
                ),
                box(
                  title = "Key Facts",
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  textOutput("keyFacts")
                ),
                box(
                  title = "Narrative Description",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  textOutput("narrative")
                )
              )
      ),
      tabItem(tabName = "demographics",
              fluidRow(
                column(4,
                       sliderInput("yearInput", "Select Year:", 
                                   min = 1950, max = 2021, value = 2021)
                ),
                column(4,
                       selectInput("plotType", "Select Plot Type:", 
                                   choices = c("Population", "Life Expectancy", "GDP", "Net migration (per 1,000 population)"))
                )
              ),
              fluidRow(
                box(plotOutput("dataPlot", height = "600px"), title = "Plot of Key Demographics",width = 12,solidHeader = TRUE,
                    status = "primary")  # Adjust heigh
              )
      ),
      tabItem(tabName = "comparison",
              fluidRow(
                column(4,
                       selectInput("selectedIsland", "Select Island:", 
                                   choices = c("Isle of Man", "Channel Islands"))
                       
                ),
                fluidRow(
                  box(
                    title = "Regional Map of Islands",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    leafletOutput("mapRegional", height = "400px")
                  ),
                  box(
                    title = "Comparison Plot",
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    plotlyOutput("comparisonPlot", height = "400px")),
                  
                  box(
                    title = "Comparison analysis",
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    uiOutput("Comparisonanalysis")
                  )
                )
              )),
      tabItem(tabName = "swot",
              tabsetPanel(
                type = "tabs",
                tabPanel("Strengths",
                         fluidRow(
                           box(
                             title = "Strength Table",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             DTOutput("strengthTable")
                           ),
                           box(
                             title = "Strength",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("strengths")
                           )
                         )
                ),
                tabPanel("Weaknesses",
                         fluidRow(
                           
                           box(
                             title = "Housing Crisis",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("house")),
                           box(
                             title = "Dependence on Multinational Corporations",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("multinationals")),
                           box(
                             title = "Healthcare System Strains",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("health")),
                           box(
                             title = "Rural-Urban Divide",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("rural"))
                         )
                ),
                tabPanel("Opportunities",
                         box(
                           title = "Technological Innovation",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           verbatimTextOutput("tech")),
                         box(
                           title = "Renewable Energy",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           verbatimTextOutput("energy")),
                         box(
                           title = "Tourism",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           verbatimTextOutput("tour")),
                         box(
                           title = "Brexit",
                           width = 12,
                           solidHeader = TRUE,
                           status = "primary",
                           verbatimTextOutput("bre")
                         )
                )
                ,
                tabPanel("Threats",
                         fluidRow(
                           box(
                             title = "Brexit Uncertainty",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("brexit")),
                           box(
                             title = "Global Economic Shifts",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("eco")),
                           box(
                             title = "Climate Change",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("climate")),
                           box(
                             title = "Political Instability",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             verbatimTextOutput("pol")
                           )
                         )
                )
              )))
  ))

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = cities) %>%
      addTiles() %>%
      setView(lng = -8.24389, lat = 53.41291, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat, 
        popup = ~city, 
        clusterOptions = markerClusterOptions()
      )
  })
  
  observeEvent(input$cities, {
    if (input$cities == "All cities") {
      leafletProxy("map", data = cities) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat, 
          popup = ~city, 
          clusterOptions = markerClusterOptions()
        )
    } else {
      filtered_cities <- cities %>% filter(city == input$cities)
      leafletProxy("map", data = filtered_cities) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat, 
          popup = ~city
        )
    }
  })
  
  output$mapGlobal <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -8.24389, lat = 53.41291, zoom = 3) %>%
      addMarkers(
        lng = -8.24389, lat = 53.41291, 
        popup = "Ireland",
        icon = ireland_icon
      )
  })
  
  output$keyFacts <- renderText({
   "Ireland, known as the Emerald Isle for its lush greenery, is an island in the North Atlantic. 
  It's the second-largest island of the British Isles and the third-largest in Europe. 
  The Republic of Ireland occupies most of the island, with Northern Ireland (part of the United Kingdom) in the northeast. 
  The capital of Ireland is Dublin. Known for its rich cultural heritage, particularly in literature, 
  Ireland is also famed for its rugged coastline and historic castles.

  Ireland has a diverse landscape that includes mountains, lakes, and rivers. The Cliffs of Moher, 
  located on the western coast, are a spectacular natural attraction. The country experiences a temperate maritime climate, 
  with mild winters and cool summers.

  In addition to its natural beauty, Ireland has made significant contributions to the world of arts and science. 
  The Irish people are known for their friendliness and hospitality. Traditional Irish music and dance are integral 
  parts of the cultural identity, and Irish pubs are famous for their lively atmosphere.

  The Irish economy has seen growth and development, and the country is home to global tech companies and multinational corporations. 
  Despite its modernization, Ireland has managed to preserve its traditions and folklore, making it a unique and captivating destination."

  })
  
  
  output$narrative <- renderText({
  "Ireland, often referred to as the land of saints and scholars, 
  boasts a rich tapestry of history, culture, and natural beauty. 
  Nestled in the North Atlantic, this island nation captivates visitors 
  with its enchanting landscapes, from the verdant countryside to the 
  dramatic cliffs along the rugged coastline.

  Steeped in history, Ireland has been a hub of creativity and intellectual 
  pursuits. The Irish people, known for their warmth and hospitality, have 
  contributed significantly to literature, music, and the arts. The literary 
  legacy of Ireland includes renowned figures such as James Joyce, W.B. Yeats, 
  and Samuel Beckett.

  Traditional Irish music, characterized by lively jigs and soulful ballads, 
  resonates in pubs and gatherings across the country. The vibrant cultural 
  scene is complemented by ancient castles, monastic ruins, and picturesque villages.

  Ireland's capital, Dublin, is a dynamic city where modernity meets tradition. 
  The friendly locals, often ready with a welcoming smile, add to the sense of community.

  As a destination that seamlessly blends the old and the new, Ireland invites 
  exploration of its historical sites, appreciation of its artistic endeavors, 
  and immersion in the warmth of its people. Whether enjoying a pint of Guinness 
  in a traditional pub or exploring the mystical landscapes of the countryside, 
  visitors find themselves enchanted by the unique charm of the Emerald Isle."
  })
  
  
  output$dataPlot <- renderPlot({
    req(input$yearInput)
    req(input$plotType)
    
    # Convert input$yearInput to numeric and ensure it is at most 2021
    selected_year <- min(as.numeric(input$yearInput), 2021)
    
    # Filter the data
    plot_data <- population %>%
      filter(`Year(s)` <= selected_year)
    
    # Define a vector of blue shades
    blue_palette <- c("deepskyblue4", "dodgerblue3", "deepskyblue2", "skyblue1")
    
    # Plot based on the selected plot type
    if(input$plotType == "Population") {
      p <- ggplot(plot_data, aes(x = `Year(s)`, y = Value, group = Variant)) +
        geom_line(aes(color = Variant), size = 1.5) +
        geom_point(aes(color = Variant), size = 2) +
        scale_color_manual(values = blue_palette) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "aliceblue"),  # A pale blue background
          plot.title = element_text(size = 25, color = "navy"),  # Dark blue title
          text = element_text(size = 20, color = "navy"),  # Dark blue text
          legend.position = "bottom"
        ) +
        labs(title = paste("Population over Years (Up to", selected_year, ")"), 
             x = "Year", y = "Population",
             color = "Variant") +
        expand_limits(y = 0)  # Make sure the y-axis starts at 0
      
      print(p)
    }
    
    plot_d <- life_exp %>%
      filter(`Year(s)` <= selected_year)
    
    if(input$plotType == "Life Expectancy") {
      plot <- ggplot(plot_d, aes(x = `Year(s)`, y = Value, group = Variant)) +
        geom_line(aes(color = Variant), size = 1.5) +
        geom_point(aes(color = Variant), size = 2) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "aliceblue"),  # A pale blue background
          plot.title = element_text(size = 25, color = "navy"),  # Dark blue title
          text = element_text(size = 20, color = "navy"),  # Dark blue text
          legend.position = "bottom"
        ) +
        labs(title = paste("Population over Years (Up to", selected_year, ")"), 
             x = "Year", y = "Population",
             color = "Variant") +
        expand_limits(y = 0)  # Make sure the y-axis starts at 0
      
      print(plot)
    }
    
    plot <- gdp %>%
      filter(`Year` <= selected_year)
    
    # Assuming you have more than one item in your GDP plot
    # Assuming you have more than one item in your GDP plot
    if (input$plotType == "GDP") {
      pop <- ggplot(plot, aes(x = `Year`, y = Value, group = Item)) +
        geom_line(aes(color = Item, linetype = Item), size = 1.5) +
        geom_point(aes(color = Item), size = 2) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "aliceblue"),  # A pale blue background
          plot.title = element_text(size = 25, color = "navy"),  # Dark blue title
          text = element_text(size = 15, color = "navy"),  # Dark blue text
          legend.position = "bottom"
        ) +
        labs(title = paste("GDP over Years (Up to", selected_year, ")"), 
             x = "Year", y = "GDP",
             color = "Item", linetype = "Item") +
        expand_limits(y = 0)  # Make sure the y-axis starts at 0
      
      print(pop)
    }
    

    
    data <- migration_data %>%
      filter(`Year(s)` <= selected_year)
    
    if(input$plotType == "Net migration (per 1,000 population)") {
      mig <- ggplot(data, aes(x = `Year(s)`, y = Value, group = Variant)) +
        geom_line(aes(color = Variant), size = 1.5) +
        geom_point(aes(color = Variant), size = 2) +
        scale_color_manual(values = blue_palette) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "aliceblue"),  # A pale blue background
          plot.title = element_text(size = 25, color = "navy"),  # Dark blue title
          text = element_text(size = 20, color = "navy"),  # Dark blue text
          legend.position = "bottom"
        ) +
        labs(title = paste("Population over Years (Up to", selected_year, ")"), 
             x = "Year", y = "Population",
             color = "Variant") +
        expand_limits(y = 0)  # Make sure the y-axis starts at 0
      
      print(mig)
    }
  })
  
  output$mapRegional <- renderLeaflet({
    selected_island <- input$selectedIsland
    selected_island_data <- islands_data[islands_data$Island == selected_island, ]
    
    leaflet(data = selected_island_data) %>%
      addTiles() %>%
      setView(lng = selected_island_data$Lon, lat = selected_island_data$Lat, zoom = 6) %>%
      addMarkers(
        lng = ~Lon, lat = ~Lat,
        popup = ~Island,
        clusterOptions = markerClusterOptions() 
      )
  })
  
  output$comparisonPlot <- renderPlotly({
    # Combine data for all regions with an additional 'Country' column
    gdp_combined <- bind_rows(
      mutate(gdp_gb, Country = "Ireland"),
      mutate(gdp_iom, Country = "Isle of Man"),
      mutate(gdp_ci, Country = "Channel Islands")
    )
    # Filter based on user input
    if(input$plotType == "GDP") {
      # Filter out Channel Islands data for this specific case
      gdp_filtered <- gdp_combined %>% filter(Country != "Channel Islands")
    } else {
      gdp_filtered <- gdp_combined
    }
    
    # Generate the plot
    p <- ggplot(gdp_filtered, aes(x = Year, y = GDP, color = Country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "GDP Comparison",
           x = "Year", y = "GDP", color = "Country") +
      scale_y_continuous(limits = c(NA, 500)) # Cap the Y-axis at 100
    
    ggplotly(p)
    
    # Convert ggplot object to a plotly object
  })
  
  
  
  output$Comparisonanalysis <- renderUI({
    HTML("
      <p>Geography and Location:
Ireland is an island nation located in the North Atlantic Ocean, known for its verdant landscape and dubbed the Emerald Isle. It is the second-largest island of the British Isles and the third-largest in Europe. The Republic of Ireland occupies the majority of the island, with Northern Ireland (part of the United Kingdom) in the northeast. In contrast, the Isle of Man and the Channel Islands, though geographically close, are not part of the United Kingdom or European Union; they are self-governing dependencies of the British Crown with their own unique landscapes and cultural identities.</p>
      <p>Economy:
As evident from the GDP graph, Ireland has experienced significant economic growth, especially since the late 20th century. The country has become a hub for technology and pharmaceutical industries, contributing to its high GDP. The Isle of Man has a diversified economy with sectors like finance, manufacturing, and tourism driving growth. The Channel Islands, with their stable finance sectors and tourism, also show a robust economic structure but on a smaller scale compared to Ireland's dynamic market.</p>
      <p>Cultural Background:
Ireland's rich cultural heritage is world-renowned, with contributions to literature, music, and the arts. The country is famous for its rugged coastline, historic castles, and significant literary figures. The Isle of Man and Channel Islands also boast their own distinct cultural heritage, with the former known for the Manx language and the latter for a mix of British and French influences.</p>
      <p>
While Ireland has transformed into a modern, high-tech economy, it still retains its cultural roots, making it a unique blend of tradition and modernity. The Isle of Man and Channel Islands, while smaller in economic size, offer a blend of traditional charm and financial services, each maintaining its own distinct place within the broader British Isles<p>
         ")
  })


  
  
  output$strengthTable <- renderDT({
    datatable(
      economic_data,
      options = list(
        lengthMenu = c(5, 10, 15),
        pageLength = 10)
    )
  })
  
  output$strengths <- renderText({
    "Economic Growth: Ireland has shown robust economic growth over the years, with increases in GDP and GDP per capita.
High Labour Force Participation: The labour force participation rate is high, including a relatively balanced gender participation.
Diverse Economy: The economy is well-diversified across agriculture, industry, and services, with a particular strength in the services sector.
Education System: Ireland has a well-regarded education system that produces a skilled workforce.
Foreign Investment: Ireland is a favorable destination for foreign direct investment, especially in technology and pharmaceuticals.
Trade Relationships: Strong international trade, with significant exports and imports."
    
  })
  
  output$house <- renderText({
    "Ireland faces a significant housing crisis, with shortages leading to high housing costs."
    
  })
  
  output$multinationals <- renderText({
    "The economy's reliance on multinational corporations makes it vulnerable to global economic changes"
    
  })
 
  output$health <- renderText({
    "The healthcare system has room for improvement in terms of accessibility and quality."
    
  })
  output$rural <- renderText({
    " Economic growth is often concentrated in urban areas, leading to a divide in services and opportunities"
    
  })
  
  output$tech <- renderText({
    "Ireland can capitalize on its strong tech sector to drive innovation and growth.
"
    
  })
  
  output$energy<- renderText({
    "With its geographic position, Ireland has significant potential for developing renewable energy, particularly wind energy.
"
    
  })
  
  output$tour <- renderText({
    "Ireland's rich culture, history, and landscapes are a persistent draw for tourists.
"
    
  })
  output$bre <- renderText({
    " Post-Brexit, Ireland can attract businesses looking to retain an EU base, especially in financial services."
    
    
    
  })
  
  output$brexit <- renderText({
    "While there are opportunities, Brexit also presents significant economic uncertainties, particularly for trade.

"
    
  })
  
  output$eco<- renderText({
    "Ireland's open economy makes it susceptible to global economic trends and crises
"
    
  })
  
  output$climate <- renderText({
    "As an island nation, Ireland faces particular threats from climate change, including sea-level rise and extreme weather events
"
    
  })
  output$pol <- renderText({
    " The potential for political instability in Northern Ireland post-Brexit poses a risk."
})
}

shinyApp(ui, server)
