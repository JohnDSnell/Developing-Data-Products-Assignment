library(dplyr)
library(DT)
library(plotly)


#load dataset
#download.file("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/sleepstudy.csv","sleepstudy.csv")
sleep <-read.csv("sleepstudy.csv")
#extract list of participants
subjects <- unique(sleep$Subject)
rawText <- paste(readLines("Documentation.txt"), collapse="\n")
#a function to isolate a specific subset of data for a specified participant
# parameter: subject - the code for the specified participant or 'all' if all participants required
# return a dataframe which is a subset of the  original sleep dataframes
getsubject <- function(subject) {
  if (subject == "all")
    subjectset = sleep
  else
    subjectset <- subset(sleep,Subject == subject)
}

#a function that builds a dataframe for display in a table on the client window 
# parameter: subject - the code for the specified participant or 'all' if all participants required
# returns: a daatframes containing participant ids, the intercept and slope of the regression line, plus the P value and R Squared value for that regression
#          for all participants identified by the parameter: subject
buildTableOfCoefficients <- function(subject) {
  #if subject is "all" extract data for all subjects,
  #otherwise only the one we're interested in
  if (subject == "all")
  {
    mod <- lm(Reaction - min(Reaction) ~ Days,data = sleep)
    sm <- summary(mod)
    #initiate dataframe with the characteristics of the regression performed across all participants 
    coeffs = data.frame(
      Subject = -1,
     # SubjectText="all",
      Intercept = mod$coefficients[1],
      Slope = mod$coefficients[2],
      "P Value" = sm$coefficients[1,4],
      "R Squared" = sm$r.squared
    )
    #now add the regression charactisitcs for each of the individual particants
    for (thissubject in subjects)
    {
      subjectset <- subset(sleep,Subject == thissubject)
      mod <- lm(Reaction - min(Reaction) ~ Days,data = subjectset)
      sm <- summary(mod)
      
      coeffs <-
        rbind(
          coeffs,c(
            thissubject,
           # as.character(thissubject),
            mod$coefficients[1],
            mod$coefficients[2],
            sm$coefficients[1,4],
            sm$r.squared
          )
        )
      
    }
    
  }
  else{
    subjectset <- getsubject(subject)
    mod <- lm(Reaction - min(Reaction) ~ Days,data = subjectset)
    sm <- summary(mod)
    coeffs = data.frame(
      Subject = subject,
      #SubjectText = as.character(subject),
      Intercept = mod$coefficients[1],
      Slope = mod$coefficients[2],
      "P Value" = sm$coefficients[1,4],
      "R Squared" = sm$r.squared
    )
  }
  #return the coefficients dataframe
  coeffs
}


shinyServer(function(input, output) {
  
  #return the selected value chosen by the user
  output$inputValue <- renderPrint({
    input$select
  })
  output$helpText<-renderPrint({
    rawText
  })
  #build and return a plot based on the users selection
  output$plot <- renderPlotly({
    selected <- input$select
    requiredset <- getsubject(selected)
    subjects <- unique(requiredset$Subject)
    mod1 <- lm(Reaction - min(Reaction) ~ Days,data = requiredset)
    #set plot fonts
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "lightgrey"
    )
    f2 <- list(
      family = "Old Standard TT, serif",
      size = 14,
      color = "black"
    )
    #build x and y labels
    a <- list(
      title = "Days of Sleep Deprivation",
      titlefont = f1,
      showticklabels = TRUE,
      #tickangle = 45,
      tickfont = f2,
      exponentformat = "e",
      showexponent = "All"
    )
    b <- list(
      title = "Reaction Time offset against the fastest time",
      titlefont = f1,
      showticklabels = TRUE,
      #tickangle = 45,
      tickfont = f2,
      exponentformat = "e",
      showexponent = "All"
    )
    
    #perform plot
    p1 <- plot_ly(
      requiredset,
      y = Reaction - min(Reaction),
      x = Days,
      showlegend = TRUE,
      mode = "markers",
      name = "Subject data",
      color = as.factor(Subject)
      #size=cls/100
      #
    )
    
    p1 <- add_trace(
      p1,
      y = mod1$fitted.values,
      name = "Regression line",
      #color="red",
      showlegend = TRUE,
      
      mode = "lines"
    )
    layout(
      p1,xaxis = a,
      yaxis = b,
      showlegend = TRUE,
      title = paste(
        "Normalised Reaction Times Vs Days of Sleep Deprivation for Subject ",selected,sep = " "
      )
    )
  })
  
  #buid and return a datframe of regression characteristics basd on the user's selection
  output$newTable <-
    DT::renderDataTable(buildTableOfCoefficients(input$select))
  
  
})
