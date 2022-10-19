#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  
  titlePanel("House Price Prediction System Using R."),
  
  sidebarPanel(
    
    selectInput("area" , "Choose the Area" , list("Built Up Area ===> 1",
                                                  "Carpet  Area ===> 2",
                                                  "Plot  Area ===> 3",
                                                  "super Built Up Area ===> 4")),
    
    
    textInput("area_id","Enter the area id which you can see in above part :- ",""),
    
    textInput("location_id","Enter the location pin-code :- ",""),
    
    textInput("bhk","How many bhk flat you want :-",""),
    
    textInput("sqft","Enter the total sqft :- ",""),
    
    textInput("bath","How many bath you want :- ",""),
    
    textInput("balcony","How many balcony you want :- ",""),
    
    actionButton('go',"Predict")
  ),
  
  mainPanel(
    sidebarPanel( width = 30,
                  
                  headerPanel("The Cost of the House"),
                  
                  textOutput("value")
    )
  )
  
)




server <- function(input, output) {
  
  
  
  data2 = reactiveValues()
  observeEvent(input$go,{
    
    data = read.csv("C:/Users/Aakash/Desktop/College BE Project/R/new house data 2.csv")
    #View(data)
    summary(data)
    str(data)
    
    is.factor(data$area_type)
    is.factor(data$location)
    
    data$area_type = as.factor(data$area_type)
    data$location = as.factor(data$location)
    
    str(data$area_type)
    str(data$location)
    str(data)
    
    Use_Data = data[,c("area_type","location","size","total_sqft","bath","balcony","price")]
    head(Use_Data)
    summary(Use_Data)
    
    na_clean_data = na.omit(Use_Data)
    summary(na_clean_data)
    str(na_clean_data)
    #View(na_clean_data)
    
    #sapply takes a list,vector or data frame as input and gives output in the form of an array or matrix object
    area.type = sapply(na_clean_data$area_type, as.numeric)
    #View(area.type)
    
    data.location = sapply(na_clean_data$location, as.numeric)
    #View(data.location)
    
    # cbind is used for merging two data frames together given that the number of rows in both data frames are equal
    second_final = cbind(na_clean_data, area_id = area.type)
    #View(second_final)
    
    second_main_dataset = cbind(second_final, location_id = data.location)
    #View(second_main_dataset)
    
    # c() function is used to combine the arguments passed to it
    Main_data_set = second_main_dataset[,c("area_type","area_id","location","location_id","size","total_sqft","bath","balcony","price")]
    #View(Main_data_set)
    
    inputdata = Main_data_set[,c("area_id","location_id","size","total_sqft","bath","balcony","price")]
    
    #View(inputdata)
    
    
    data2$myarea_id <- as.numeric(input$area_id)
    data2$myloaction_id <- as.numeric(input$location_id)
    data2$mybhk <- as.numeric(input$bhk)
    data2$mysqft <- as.numeric(input$sqft)
    data2$mybath <- as.numeric(input$bath)
    data2$mybalcony <- as.numeric(input$balcony)
    
    newPredict = data.frame(area_id = data2$myarea_id, location_id = data2$myloaction_id,
                            size = data2$mybhk, total_sqft = data2$mysqft,
                            bath = data2$mybath, balcony = data2$mybalcony)
    
    #lm function is used to fit linear models to data frames
    model = lm(price ~ area_id + location_id + size+total_sqft + bath + balcony,
               data = inputdata, weights = 1/inputdata$price ^ 3.8  )
    
    data2$op = predict(model, newPredict)
  })
  
  output$value <- renderPrint({data2$op})
 # tags$h3("output$value")
}

shinyApp(ui, server)