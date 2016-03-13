library(plotly)
#download.file("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/sleepstudy.csv","sleepstudy.csv")
sleep <-read.csv("sleepstudy.csv")


shinyUI(
  pageWithSidebar( 
    # Application title 
    headerPanel("Effect of Sleep Deprivation on Reaction Times"),
  sidebarPanel( 
    selectInput("select", label = h3("Select Test Particant"), 
                choices = c("all",unique(sleep$Subject)),selected = "none"),
    h3("Help Documentation"),
    htmlOutput("helpText")#,
  #submitButton('Submit') 
  ), 
  mainPanel( h3('Results of Selection'), 
             h4('Your selected participant'), 
             verbatimTextOutput("inputValue"), 
             h4('Results '), 
             plotlyOutput("plot"),
             DT::dataTableOutput("newTable")
             )
  )
)
