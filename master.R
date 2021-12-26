library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library(shinydashboard)
library(dashboardthemes)
library(devtools)
library(DT)
library(Hmisc)
library(leaflet)

Datafinal=  read_excel("D:/R shiny/final_t1rain.xlsx")
df=as.data.frame(Datafinal)
names(df)=c("Profile","Experience","Place","Ratings","Fees","Latitude","Longitude","AvgExp","AvgRatings","AvgFees")
df$Place=as.factor(df$Place)
df$Profile=as.factor(df$Profile)
attach(df)

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "Doctor's Fee"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset",tabName = "DocData",icon=icon("table")),
      menuItem("Data Distribution",tabName = "Histograms",icon=icon("bar-chart")),
      menuItem("Relationship b/w Variables",tabName = "Scatterplotss",icon=icon("link")),
      menuItem("Predicting Doc's Fee",tabName = "Prediction",icon=icon("thermometer"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme="grey_dark"),
    tabItems(
      #first tab,displaying the doctor's dataset
      tabItem(tabName = "DocData",
              h1("The Doctor's Dataset is Displayed Below:"),
              fluidRow(
                column(4,
                       selectInput("place",
                                   "Place:",
                                   c("All",
                                     unique(as.character(df$Place))))
                ),
                column(4,
                       selectInput("profile",
                                   "Profile:",
                                   c("All",
                                     unique(as.character(df$Profile))))
                       
                )
              ),
              # Create a new row for the table.
              DT::dataTableOutput("table")
              
              #The below code displays the distribution of data according to each variable
              
      ),
      tabItem(tabName = "Histograms",
              fluidRow(
                shinyDashboardThemes(
                  theme="grey_dark"),
                box(
                  selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                              selected = "Blue",multiple = F),
                  
                  radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                  
                  selectInput(inputId="variables",label="Choose a variable",choices = c("Experience","Fees","Ratings","Place","Profile"),
                              selected = "Fees",multiple = F),
                  
                  sliderInput(inputId = "bins1xz",
                              label = "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 30)),
                fluidRow(
                  box(title="Distribution Of Variable:",background = "purple",solidHeader = TRUE,plotOutput("plothist",height=600),width=12)))
      ),
      #The below code is used to construct an interactive Scatterplot to display the relationship between two variables and the strength of their relationship
      tabItem(tabName = "Scatterplotss",
              fluidRow(
                shinyDashboardThemes(
                  theme="grey_dark"),
                box(
                  selectInput("X",
                              "Select x-axis:",
                              choices=c("Experience"="Experience","Fees"="Fees","Ratings"="Ratings","Place","Profile")),
                  
                  selectInput("Y",
                              "select y-axis:",
                              choices=c("Experience"="Experience","Fees"="Fees","Ratings"="Ratings","Place","Profile")),
                  valueBoxOutput("EVF",width=6),
                  valueBoxOutput("RVF",width=6),
                  width=6,
                  h2("The Above Value boxes Display The Pearson's Correlation Coefficients of Respective
                       Variables Mentioned.")),
                
                fluidRow(
                  box(title="Relationship Between Two Variables:",background = "maroon",solidHeader = TRUE,plotOutput("plot1",height=500)),width=16))
      ),
      #The below code is to predict the doc fees given the ratings and the place the doctor is located at
      tabItem(tabName = "Prediction",
              fluidRow(
                box(leafletOutput("Docmap",height=600),background = "yellow",width="fill"),
                h2("   Select the Place and Ratings to Get The Doctor's Fee:"),
                fluidRow(
                  box(
                    selectInput("city",
                                "Select Place:",
                                choices=c("Chennai","Bangalore","Ernakulam","Delhi","Thiruvananthapuram","Hyderabad","Coimbatore","Mumbai"),
                                selected = "Chennai"),
                    sliderInput(inputId = "ratings",
                                label = "Select ratings:",
                                min = 5,
                                max = 100,
                                value = 30),
                    textOutput("txtoutput"))
                  
                )
              )
      )
    )
  ))
server<-function(input,output){
  #output of dataset to view data in the first tab
  output$table <- DT::renderDataTable(DT::datatable({
    data<-df
    if (input$place != "All") {
      data <-  data[data$Place == input$place,]
    }
    if (input$profile != "All") {
      data  <-  data[ data$Profile == input$profile,]
    }
    data
    
  }))
  #Output for Distribution of data second tab
  output$plothist <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    p2 <- df %>% ggplot()
    if(input$variables == "Experience"){
      p2 <- p2 + geom_histogram(aes(x=Experience),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$variables == "Ratings"){
      p2 <- p2 + geom_histogram(aes(x=Ratings),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$variables == "Profile"){
      p2 <- p2 + geom_bar(aes(x=Profile),col=input$border1,fill=sColor)
    }else if(input$variables == "Place"){
      p2 <- p2 + geom_bar(aes(x=Place),col=input$border1,fill=sColor)
    }else if(input$variables == "Fees"){
      p2 <- p2 + geom_histogram(aes(x=Fees),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    print(p2)
    
  })
  #Output for depicting the relationship between varaibales
  output$plot1 <- renderPlot({
    ggplot(df, aes_string(x = input$X, y = input$Y))  +
      geom_point(color = "purple",shape=1,aes(size=5)) +
      geom_smooth(method="lm",se=F,color="green")
  })
  output$EVF<-renderValueBox({
    valueBox(
      "-0.0046", "Experience And Fees", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$RVF<-renderValueBox({
    valueBox(
      "-0.0631", "Ratings And Fees", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  #Output of prediction of doc's fees and the location hotspots
  output$txtoutput<-renderText({
    fit<-lm(Fees~0+Ratings+factor(Place),data=Datafinal)
    prd<-predict(fit,newdata=data.frame(Ratings=input$ratings,Place=input$city))
    print(paste("Doctor's Fee is:",round(prd,2)))
    
  })
  output$Docmap<-renderLeaflet({
    
    df<-df%>%mutate(popinfo=paste(Place,"<br/>","Average Fees",AvgFees,"<br/>","Average Ratings",AvgRatings,"<br/>","Average Experience",AvgExp))
    leaflet()%>%addTiles()
    
    colors<-c("Yellow","Red")
    pal<-colorFactor(colors,df$Avg_Fees)
    leaflet()%>%addTiles()%>%addCircleMarkers(data=Datafinal,lat=~Latitude,lng=~Longitude,radius = ~10,popup = df$popinfo,color = ~pal(Avg_Fees))
  })
  
  
}
shinyApp(ui = ui, server = server)



