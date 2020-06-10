# install libraries
library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)

#read in data
brick <- read.csv("data/brick_la.csv")
pop <- read.csv("data/pop_la.csv")

# change lat/lon to numeric for brick stores
brick$lon <- as.numeric(brick$lon)
brick$lat <- as.numeric(brick$lat)



#add content of pop ups to brick stores
brick_present <- brick%>%
    mutate(Content = paste(sep = "",
                           "<div class = \"pop-up\">",
                           "<strong><span class = \"restaurant\">",
                           Restaurant,
                           "</span></strong>",
                           "</br>",
                           Location,
                           "</br>",
                           Number,
                           "</br>",
                           "<a href=",
                           Instagram,
                           " target='_blank'>Instagram</a>",
                           "</br>",
                           "<a href=",
                           Website,
                           " target='_blank'>Website</a>",
                           "</div>"))

brick_present <- brick_present%>%
    mutate(Content = str_replace(Content, "<a href=         --- target='_blank'>Instagram</a>", "         ---"))%>%
    mutate(Content = str_replace(Content, "<a href=         --- target='_blank'>Website</a>", "         ---"))


#filter for delivery
brick_d <- brick_present%>%
    filter(Delivery == "Yes")

#filter for delivery
brick_nod <- brick_present%>%
    filter(Delivery == "No")

#filter for vegan
brick_veg <- brick_present%>%
    filter(Vegan == "Yes")

#filter for non-vegan
brick_noveg <- brick_present%>%
    filter(Vegan == "No")

#prepare pop ups for presentation in DT
pop_present <- pop%>%
    mutate(Instagram = paste0("<a href='", Instagram,"' target='_blank'>", str_replace(Instagram, "https://www.instagram.com/","@"),"</a>"))%>%
    mutate(Instagram = str_replace(Instagram, "<a href='---' target='_blank'>---</a>", "         ---"))%>%
    mutate(Website = paste0("<a href='", Website,"' target='_blank'>", str_replace(Website, "https://www.|http://www.|https://|http://",""),"</a>"))%>%
    mutate(Website = str_replace(Website, "<a href='---' target='_blank'>---</a>", "         ---"))



ui <- shinyUI(
    fluidPage(
        
        tags$head(includeHTML(("www/google-analytics.html"))),
        
        includeCSS("www/styles.css"),
        
        title = "LA: BlackOwned",
        
        tags$style(
            HTML(
                ".tabbable > .nav > li > a                {background-color: black;  color:white}
                 .tabbable > .nav > li[class=active]  > a {background-color: white; color:black}"
            )),
        
        fluidRow(
            column(2,
            tags$img(src = "images/fist-white.png", class = "fist-black")
            ),
        
            column(10,
            tags$h1(class = "title_blm", "SUPPORT BLACK OWNED BUSINESS IN LA")
            )
        ),
        
        fluidRow(column(12, (
            tabsetPanel(
                tabPanel("Brick & Mortar", leafletOutput("mymap")),
                tabPanel("Pop-Up", dataTableOutput("mytable")))
            )
        )),
        fluidRow(
            column(6,
                        tags$div(
                            class = "footer",
                            h3("Many Thanks"),
                            hr(),
                            p("Inspiration from:"),
                            a(href ="https://www.theinfatuation.com/contributor/kat-hong", "Kat Hong's"),
                            a(href ="https://www.theinfatuation.com/features/support-black-owned-restaurants-in-la", "comprehensive list"),
                            hr(),
                            p("Restaurants from:"),
                            a(href="https://www.supportblackowned.com/", "supportblackowned.com"),
                            p("and"),
                            a(ref="https://www.latimes.com/food/story/2020-05-31/black-owned-restaurants-in-los-angeles?fbclid=IwAR3vts0x03E_HYbOYRDUqN6hDQ_akgMxK2H83_cvD-Zzvq7CJSkGvMcR2oc", "LA Times"),
                            align = "center"
                            
                        )),
            column(6,
                        tags$div(
                            class = "footer",
                            hr(),
                            p("Made Using:"),
                            a(href="https://shiny.rstudio.com/", "Shiny"),
                            p("and"),
                            a(href="https://rstudio.github.io/leaflet/", "Leaflet"),
                            hr(),
                            HTML("<div>Icons originally from <a href=\"https://www.flaticon.com/authors/freepik\" title=\"Freepik\">Freepik</a> at <a href=\"https://www.flaticon.com/\" title=\"Flaticon\">www.flaticon.com</a></div>"),
                            hr(),
                            p("Please contact me at alecsharpie@gmail.com if there are any inaccuracies."),
                            align = "center"
                            
                        )))
        
    ))

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    Icons <- iconList(
        No = makeIcon("images/mark-black.png", 
                         iconWidth = 24, 
                         iconHeight = 24,
                         iconAnchorX = 0, 
                         iconAnchorY = 0,
                         popupAnchorX = 0,
                         popupAnchorY = 0),
        Yes = makeIcon("images/mark-green.png", 
                         iconWidth = 24, 
                         iconHeight = 24,
                         iconAnchorX = 0, 
                         iconAnchorY = 0,
                         popupAnchorX = 0,
                         popupAnchorY = 0)
    )

    output$mymap <- renderLeaflet({
        leaflet(data = brick_present,
                options = leafletOptions(minZoom = 10, maxZoom = 17)) %>%
            setView(lng = mean(brick$lon),
                    lat = mean(brick$lat),
                    zoom = 10) %>%
            setMaxBounds( lng1 = (min(brick$lon) - 0.4),
                          lat1 = (min(brick$lat) - 0.4),
                          lng2 = (max(brick$lon) + 0.4),
                          lat2 = (max(brick$lat) + 0.4))%>%
            
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(group="Show All",
                       lng = ~lon, 
                       lat = ~lat, 
                       icon = ~Icons[Vegan],
                       popup = ~Content)%>%
            addMarkers(data = brick_veg,
                       group = "Only show vegan places",
                       lng = ~lon, 
                       lat = ~lat, 
                       icon = ~Icons[Vegan],
                       popup = ~Content)%>%
            addMarkers(data = brick_d,
                       group = "Only show places with delivery",
                       lng = ~lon, 
                       lat = ~lat, 
                       icon = ~Icons[Vegan],
                       popup = ~Content)%>%
            addLayersControl(
                baseGroups = c("Show All", "Only show places with delivery", "Only show vegan places"),
                position = "bottomleft",
                options = layersControlOptions(collapsed = FALSE)
            )%>%
            addLegend(title = "Vegan?", 
                      labels = c("Yes", "No"), 
                      colors = c("#6ea424","#000000"),
                      position = "bottomleft")
            
    })
    
    output$mytable = renderDataTable({
        datatable(pop_present, 
                  escape = FALSE)
    })
    
}


shinyApp(ui, server)
