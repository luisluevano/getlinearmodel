library(shiny)
shinyUI(pageWithSidebar (
  headerPanel("Load your data to get your predictive formula:"),
  sidebarPanel(
    h4("This app gives you a linear regression model based on any data you want to predict for. Just follow the steps."),
    h5("Step 1: Enter the sample values from the variable you want to predict separated by comma."),
    textInput('iDepVar', 'Dependent variable data: ', value = ''),
    h5("Step 2: Select how many predicted variables you want to use (up to 5):"),
    numericInput('iNoVar', 'Number of predictive variables: ', 1, min=1, max=5, step=1),
    h5("Step 3: Enter the values for each predictive variable separated by comma."),    
    textInput('iVar1', 'Variable 1 data: ', value = ''),
    textInput('iVar2', 'Variable 2 data: ', value = ''),
    textInput('iVar3', 'Variable 3 data: ', value = ''),
    textInput('iVar4', 'Variable 4 data: ', value = ''),
    textInput('iVar5', 'Variable 5 data: ', value = ''),
    h5("Step 4: Hit the Get Formula button and look at the right panel. "),        
    actionButton('getButton','Get Formula')
  ),
  mainPanel(
    h3('Here is your predictive formula!'),
    verbatimTextOutput("oformula")
  )
))