library(xts)
library(shiny)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(splitstackshape)

#setwd('C:/Users/wen/Downloads/gunviolence')


all <- read.csv("Data/gun-violence.csv")
typeof(all$Date)
all$Date<-as.Date(all$Date, format ="%m/%d/%Y")
all$year <- year(all$Date)

all$month <- month(all$Date, label=TRUE)
all$day <- day(all$Date)

all$Mass_Shooting <- ifelse(all$Killed >= 4, "Yes", "No")
all$lat[is.na(all$lat)] <- 28
all$lon[is.na(all$lon)] <- -128


type = cSplit(all,c("participant_age_group"),sep="||",direction="long",drop=TRUE)
type$participant_age_group=gsub(".*:","",type$participant_age_group)
head(type$participant_age_group)


ui <- navbarPage("Gun Violence", id="nav",
                 
                 tabPanel("Interactive Map",
                          div(class="outer",
                              tags$head(
                                includeCSS("Assets/styles.css")
                              ),
                              leafletOutput("map", width="100%", height="100%"), 
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("US Gun Violence"),
                                            h4("2013-2018"),
                                          
                                            selectInput("Year", "Year", c(2013,2014,2015,2016,2017,2018)
                                                      ),
                                            selectInput("incidentweight", "Incident Factor",
                                                        c("Killed", "Injured"),selected = "Killed")   
                                            
                                        
                              ),
                              tags$div(id="cite",
                                       'Data source:  The Gun Violence Archive, 2013-2018.'
                              )
                          )
                 ),  
                 
    
      tabPanel("Analysis",
      sidebarLayout(
        sidebarPanel( 
          
      h4("Choose either one: "),
          checkboxInput("Killed", "Killed", value=FALSE),
          checkboxInput("Injured", "Injured", value = FALSE),
          sliderInput("years", "Years",
                      min(type$year), max(type$year),
                      value = c(2013,2018)),
          selectizeInput("states", "State (only 3 allowed)",
                      choices = c("All",levels(type$state)),
                      selected = "All",
                      multiple = TRUE,
                      options = list(maxItems = 3,
                                     placeholder = 'Select a name')
                      
                     ),
      selectInput("x", "X-axis:", choices = c("year", "state","participant_age_group")),
      selectInput("y", "y-axis:", choices = c("count")),
      
      img(src="gun_pic.jpg", height="90%", width="90%")
          
        ),
        mainPanel(
           plotOutput("scatterplot", height="800px",width="900px")
          
        ))
      ), 
      
      tabPanel("Dangerous Dates",
               sidebarLayout(
                 sidebarPanel(
                   # Text instructions
                  # HTML(paste("Enter a value between 1 and 30")),
                   
                   # Numeric input for sample size
                   numericInput(inputId = "n",
                                label = "Top N Most Dangerous Date",
                                min = 1,
                                max = 365,
                                value = 10,
                                step = 1),
                   
                   checkboxInput("Mass_shooting", "Mass Shooting", value = FALSE)
                   
                 ),
                 
                 mainPanel(
                   htmlOutput(outputId = "SpecialDate")
                 )
                 
               )
               
      ),
      
             

        tabPanel("About",
            fluidRow(
                column(12,
                    h4("Introduction"),
                    p("Gun ownership in the United States is higher per capita than any other nation in the world. 
                      Futhermore, it is protected by the 2nd Ammendment of the United States Constitution: 
                      A well regulated militia, being necessary to the security of a free state, 
                      the right of the people to keep and bear arms, shall not be infringed.
                      Each year over 30,000 people die from gun related deaths within the United States and the number is only increasing. 
                      At what cost are Americans willing to pay to keep their rights as gun owners?"),
                    h4("Data Source"),
                    div(HTML("<a href='https://www.kaggle.com/jameslko/gun-violence-data'>The Gun Violence Archive</a>")),
                   
                    h4("References"),
                    div(HTML("<a href='https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example'>Shiny-R SuperZip Example</a>")),
                    div(HTML("<a href='https://github.com/wwells/shiny-apps/tree/master/gunviolence'>https://github.com/wwells/shiny-apps/tree/master/gunviolence</a>"))
                    
                     ))   
        )
        
)



server <- function(input, output, session) {
  
  output$SpecialDate <- renderText({
    all <- all %>% mutate(date2=paste(month, day))
   
    if(input$Mass_shooting == 'TRUE'){
      kable(all 
            %>% filter(year %in%c(2014:2017) , Mass_Shooting=='Yes') 
            %>% count(date2) 
            %>% top_n(input$n)  
            %>% arrange(desc(n)) 
            %>% rename("Date"=date2, "Total Number of Incidents"=n)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover")) }
    
    else {
      
      kable(all 
            %>% filter(year %in%c(2014:2017)) 
            %>% count(date2) 
            %>% top_n(input$n)  
            %>% arrange(desc(n)) 
            %>% rename("Date"=date2, "Total Number of Incidents"=n)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"))}
    
    
  })
  
    output$scatterplot <-renderPlot({
      options(scipen = 5)
      
      if (length(input$states)==0) print("Please select at least one State")
    else{
      data <- subset(all,
                       year >= input$years[1] & year <= input$years[2] 
                       )

      
      if (input$states != "All") {
        data <- subset(
          data,
          state == input$states
        )
      } else
           data
        
      
      if(input$x == "participant_age_group"){
        dataset <- subset(type,
                          year >= input$years[1] & year <= input$years[2] 
        )
       
       # change scientic notation on plot to number: 5e+5...
        
        if (input$states != "All") {
          dataset <- subset(
            dataset,
            state == input$states)
          
            g<-dataset %>% group_by(participant_age_group,state)%>% summarise(count=n())
            
            ggplot(g, aes_string(input$x, input$y,fill='state'))+
              geom_bar(stat = "identity",width=0.6,position = position_dodge()) +
              theme(text = element_text(size=20))
          
        } else{
          dataset
      
      g <- dataset %>% group_by(participant_age_group) %>% summarise(count=n())
      
      ggplot(data=g,aes_string(x=input$x,input$y)) +
      geom_bar(stat = "identity",width=0.6,fill='steelblue') +
      geom_text(aes(label=count), vjust=-0.3, size=7)+
         theme(text = element_text(size=20))}}
      
      else if (input$x == 'year'){
        
         if(input$states != 'All'){
              if(input$Injured == 'TRUE'){
                
                g<- data%>% group_by(year,state)%>%summarise(count=sum(Injured)) %>%
                  filter(state %in% input$states)
                
                ggplot(data=g,aes_string('year', 'count' ,color='state'))+
                  geom_line()+
                  theme(text = element_text(size=20))+
                  labs(x="", y='People injured')
                
              } else if(input$Killed =='TRUE'){
                g<- data%>% group_by(year,state)%>%summarise(count=sum(Killed)) %>%
                  filter(state %in% input$states)
                
                ggplot(data=g,aes_string('year', 'count' ,color='state'))+
                  geom_line()+
                  theme(text = element_text(size=20))+
                  labs(x="", y='People Killed')
                
              } else {
                g<- data%>% group_by(year,state)%>%summarise(count=sum(Killed+Injured)) %>%
                  filter(state %in% input$states)
              
                ggplot(data=g,aes_string('year', 'count' ,color='state'))+
                  geom_line()+
                  theme(text = element_text(size=20))+
                  labs(x="", y='Total victims')}
         }
        else{
          
           if(input$Killed =='TRUE'){
            g<-data%>% group_by(year) %>%summarise(count = sum(Killed))
            ggplot(data=g, aes_string(x=input$x, input$y))+
              geom_bar(stat='identity', fill = 'steelblue')+
              geom_text(aes(label=count), vjust=-0.3, size=7)+
              theme(text = element_text(size=20))+
              labs(x="", y='People Killed')}
            else if (input$Injured == 'TRUE'){
              g<-data %>% group_by(year)%>%summarise(count = sum(Injured))
              ggplot(data=g, aes_string(x=input$x, input$y))+
                geom_bar(stat='identity', fill = 'steelblue')+
                geom_text(aes(label=count), vjust=-0.3, size=7)+
                theme(text = element_text(size=20))+
                labs(x="", y='People Injured')}
              
            else {
              g<-data %>% group_by(year)%>% summarise(count = n())
              ggplot(data=g, aes_string(x=input$x, input$y))+
                geom_bar(stat='identity', fill = 'steelblue')+
                geom_text(aes(label=count), vjust=-0.3, size=7)+
                theme(text = element_text(size=20))+
                labs(x="", y='Total incidents')
            }
            
            }
        
        
  }
      
      else if(input$x=="state" ){
          
         g<- data %>% group_by(state) %>% summarise(count=n())
        
          ggplot(data=g,aes_string(x=input$x, input$y))+
          geom_bar(stat='identity', fill='steelblue', width = 0.3) + 
          labs(x='', y='number of incidents')+
          theme(axis.text.x = element_text(angle = 90,size=13),
                axis.title=element_text(size=25))
          
          
      }
    
       }})
    history <- reactive({
      all %>%
        filter(year <= input$Year)
    }) 
    
    color <- reactive({
        if (input$incidentweight == "Killed") {
            col = "RdBu"
        } else
            col = "BrBG"
        
    })
    
   sc <- reactiveVal(15000)
    
    observeEvent(input$incidentweight, {
        if (input$incidentweight == "Killed") {
            newValue <- 15000
            sc(newValue)
        } else {
            newValue <- 10000
            sc(newValue)
        }
  })
    
    name <- reactive({
        if (input$incidentweight == "Killed") {
            nam = "Killed"
        } else {
            nam = "Injured"
        }
    })
    
    colorpal <- reactive({
        colorNumeric(color(), all[[input$incidentweight]])
    })
    
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        
            addTiles(
             urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
              attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
              addLegend(position = "bottomright",
                      pal = colorpal(), values = all[[input$incidentweight]],
                      title = name()) %>%
            setView(lng = -87.85, lat = 37.45, zoom = 4)
    })
    
    observe({
        pal <- colorpal()
        proxy <- leafletProxy("map",
                               data = history()) 
        proxy %>%
            clearShapes() %>%
            addCircles(lng = ~lon,
                       lat = ~lat,
                      radius = ~history()[[input$incidentweight]] * sc(),
                       weight = 1,
                       color = "#6754D8",
                       fillColor = ~pal(history()[[input$incidentweight]]),
                       stroke = F, 
                       fillOpacity = 0.7,
                       data = history()
            ) 
    })
    
}

shinyApp(ui, server)