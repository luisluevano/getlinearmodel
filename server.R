library(shiny)
library(stats)

getLMFormula <- function(pDepVar, pNoVar, pVar1, pVar2, pVar3, pVar4, pVar5) {
  # Validate dependant variable
  if(nchar(pDepVar) < 1) # Has to be length greater than 0
    return('Input Data in Dependent variable is required.')
  # Get the individual values
  DepVar <- as.numeric(unlist(strsplit(pDepVar, split = ',')))
  # Get the number of rows the predict values should have to validate same length
  noRows <- length(DepVar)
  
  # Start creating the predictid variables
  NoVar <- pNoVar # Number of variables selected by user  
  
  # Validate the predictive varables
  i <- 1
  while(i <= NoVar) {
    inputlength <- nchar(get(sprintf("pVar%i",i)))
    if(inputlength < 1)
      return(paste0('Input Data in Variable ',i,' is required.'))    
    
    vartmp <- as.numeric(unlist(strsplit(get(sprintf("pVar%i",i)), split = ',')))
    varlength <- length(vartmp)        
    if(varlength != noRows)
      return(paste0('Input Data in Variable ',i,' must have the same number of elements as the Dependent variable.'))  
    
    assign(paste0("Var",i),vartmp)
    i <- i + 1
  }  

 # Based on number of input variables, get the linear model and sent return the coefficients.
    
  if(NoVar == 1) {
    fit <- lm(DepVar ~ Var1 - 1)
    return(paste0('DepVar = ', 'Var1*', round(fit$coeff[[1]], 2)))
  }
  else if(NoVar == 2) {
    fit <- lm(DepVar ~ Var1 + Var2 - 1)
    return(paste0('DepVar = ', 'Var1*', round(fit$coeff[[1]], 2),' + ','Var2*', round(fit$coeff[[2]], 2)))
  }
  else if(NoVar == 3) {
    fit <- lm(DepVar ~ Var1 + Var2 + Var3 - 1)
    return(paste0('DepVar = ', 'Var1*', round(fit$coeff[[1]], 2),' + ','Var2*', round(fit$coeff[[2]], 2),' + ','Var3*', round(fit$coeff[[3]], 2)))
  }
  else if(NoVar == 4) {
    fit <- lm(DepVar ~ Var1 + Var2 + Var3 + Var4 - 1)
    return(paste0('DepVar = ', 'Var1*', round(fit$coeff[[1]], 2),' + ','Var2*', round(fit$coeff[[2]], 2),' + ','Var3*', round(fit$coeff[[3]], 2),' + ','Var4*', round(fit$coeff[[4]], 2)))
  } else {
    fit <- lm(DepVar ~ Var1 + Var2 + Var3 + Var4 + Var5 - 1)
    return(paste0('DepVar = ', 'Var1*', round(fit$coeff[[1]], 2),' + ','Var2*', round(fit$coeff[[2]], 2),' + ','Var3*', round(fit$coeff[[3]], 2),' + ','Var4*', round(fit$coeff[[4]], 2),' + ','Var5*', round(fit$coeff[[5]], 2)))
  }
}

shinyServer(
  function(input, output){     
    output$oformula <- renderText({
      if (input$getButton == 0) ''
      else {
       input$getButton
       isolate(
         getLMFormula(input$iDepVar
                            , input$iNoVar
                            , input$iVar1, input$iVar2, input$iVar3, input$iVar4, input$iVar5)
         )
      }
    })
  }
)