# define the user interface

my.data <- read.table("Eurostat_Table.txt", header = TRUE, sep="\t", dec=",")
shinyUI(
  # define type of page layout
  #pageWithSidebar(
  fluidPage( theme = "superman.css",
             # define content of page header ####
             headerPanel("European Monetary Union interest rate"),
             
             # define content of left side of the page ####
             sidebarPanel(
               conditionalPanel( condition = "input.conditionedPanels==1",
                                 tags$div(class="header", checked=NA,
                                          list("In the first two tabs you can choose 3 countries and compare their interest rates throughout years.",
                                               "The outcome can be represented as a plot or as a table.")),
                                 hr(),
                                 conditionalPanel( condition = "input.conditionedPanels==1", 
                                                   selectInput(inputId = "Country",
                                                               label = "Choose a country",
                                                               choices = my.data$geo),
                                                   hr(),
                                                   selectInput(inputId = "Country2",
                                                               label = "Choose a second country",
                                                               choices = my.data$geo,
                                                               selected= "EU (27 countries)"),
                                                   hr(),
                                                   selectInput(inputId = "Country3",
                                                               label = "Choose a third country",
                                                               choices = my.data$geo,
                                                               selected="Euro area"))),
               
               conditionalPanel( condition = "input.conditionedPanels==3", 
                                 tags$div(class="header", checked=NA,
                                          "Annual analysis is a graphical analysis of all countries in one year.
                                          The horizontal line will show you the value for choosen country."),
                                 hr(),
                                 sliderInput(inputId = "Year",
                                             label = "Choose a year",
                                             min = 2005, max = 2016, value = 2010, step = 1, animate = TRUE, sep = ""),
                                 hr(),
                                 selectInput(inputId = "AnnualCountry",
                                             label = "Choose a country for horizontal line",
                                             choices = my.data$geo)),
               
               conditionalPanel( condition = "input.conditionedPanels==4",
                                 tags$div(class="header", checked=NA,
                                          "Below is the extended analysis for the selected country."),
                                 hr(),
                                 selectInput(inputId = "CountryAnalysis",
                                             label = "Choose a country",
                                             choices = my.data$geo),
                                 checkboxGroupInput(inputId = "Choices", 
                                                    label = "Choose features", 
                                                    choices = c("Mean" = "Mean", 
                                                                "Maximum"= "Max",
                                                                "Minimum" = "Min"),
                                                    selected = NULL)
               )
               ),
             
             # define content of the main part of the page ####   
             mainPanel(
               tabsetPanel(
                 tabPanel(title="Plot",      
                          plotOutput("plots"), value =1
                 ),
                 tabPanel(title="Table",
                          tableOutput("table1"), value = 1
                 ),
                 tabPanel(title="Annual analysis", plotOutput("hist"),value = 3
                 ), 
                 tabPanel(title="Country analysis", tableOutput("countryTable"), plotOutput("countryPlot"), value = 4
                          
                 ),
                 id = "conditionedPanels"
               )
             )
  )
  )