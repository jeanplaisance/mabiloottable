#library(shiny, lib.loc = "E:\\Program Files\\R\\R-3.3.1\\library")
library(shiny)
library(jsonlite)
library(XML)
library(stringr)
library(rsconnect)

source("Global.R", local=FALSE)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "type",
                   label="I'm looking for:",
                   choiceNames=c("Monsters", "Items", "Enchants"),
                   choiceValues=c("monsters", "items", "enchants")),
      
      #p("Make sure to set the multiplier to 1 when looking at shadow mission rewards."),
      sliderInput("mult", label = h3("Drop Rate Multiplier"), min = 1, 
                  max = 8, value = 2, width='300px'),
      
      conditionalPanel(
        condition = "input.type =='monsters'",
        selectInput("name","Monsters", 
                       lootTable$Name)),
      
      conditionalPanel(
        condition = "input.type == 'items'",
        selectInput("item", "Item",
                    itemTable)),
      
      conditionalPanel(
        condition = "input.type == 'enchants'",
        selectInput("enchant", "Enchant",
                    optionSet$name)),
      
      p("Do be mindful that some translation work is still left to do so not all names are currently meaningful. For instance, you won't find Shock, but you will find Spark pages."),
      p("Dungeons and Shadow Missions are not yet included."),
      p("Item manuals not yet loaded into the table.")
        
    ),
      
    mainPanel(
      tableOutput("table")
    ),
    position="left"
  )
)

server <- function(input, output){
  output$table <- renderTable( 
    
    if(input$type=="monsters"){
      parseMonsters(input$name,input$mult)
    }
    
    else if(input$type=="items"){
      parseItems(input$item,input$mult)
    }
    
    else if(input$type=="enchants"){
      parseEnchants(input$enchant,input$mult)
    }
  )
}

shinyApp(ui=ui, server=server)

#library(rsconnect, lib.loc = "E:\\Program Files\\R\\R-3.3.1\\library")
#rsconnect::deployApp(setwd("E:\\Studying\\R\\Mabinogi Loot Table\\"))