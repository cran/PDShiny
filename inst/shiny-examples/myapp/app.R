#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(

   # Application title
   titlePanel("Probability distribution"),
   h6("Probability Distribution  Shiny  Authored by:", tags$img(src ="K.JPG", height=100, width=100)),

   verbatimTextOutput("preface"),
   tags$img(src = "T.png"),
   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput("Distribution", label = "Select the  distribution ",
                    choices = c("Binomial","Poisson","Normal","Exponential"), selected = "Binomial"),
        conditionalPanel(condition = "input.Distribution =='Binomial'",

        selectInput("signbinom", label = "Select the sign for the binomial distribution ",
                    choices = c("<=",">=","="), selected = "="),
        sliderInput("samplesize", label = "Enter the sample size:",
                    min = 1, max = 200, value = 5, step = 1),
        sliderInput("limitingvalue", label = "Enter the limitingvalue",
                    min = 0, max = 200, value = 2, step = 1),
        sliderInput("prob", label = "Enter the probability of success",
                    min = 0, max = 1, value = 0.5, step = 0.01)
        ),
        conditionalPanel(condition = "input.Distribution =='Poisson'",

                         selectInput("signpoisson", label = "Select the sign for the Poisson distribution ",
                                     choices = c("<=",">=","="), selected = "="),
                         sliderInput("rate", label = "Enter the rate:",
                                     min = 0, max = 20, value = 5, step = 0.01),
                         sliderInput("limitingvaluep", label = "Enter the limitingvalue",
                                     min = 0,max = 20, value = 2, step = 1)


        ),
        conditionalPanel(condition = "input.Distribution =='Normal'",

                         selectInput("signnormal", label = "Select the sign for the Normal distribution ",
                                     choices = c("<=",">="), selected = ">="),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         textInput("mean", label = "Enter the mean:",30),
                         textInput("sd", label = "Enter the standard deviation:",1),
                         textInput("limitingvaluen", label = "Enter the limitingvalue",28),

                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         
                         
                         helpText("For the area in between"),

                         textInput("limitingvaluenl", label = "Enter the lower limit",28),
                         textInput("limitingvaluenu", label = "Enter the upper limit",30)
        ),





        conditionalPanel(condition = "input.Distribution =='Exponential'",

                         selectInput("signexp", label = "Select the sign for the Exponential distribution ",
                                     choices = c("<=",">="), selected = ">="),
                         
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         textInput("meantime", label = "Enter the mean time:",300),
                         textInput("limitingvaluee", label = "Enter the limitingvalue",280),

                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         helpText("For the area in between"),
                         
                         textInput("limitingvalueel", label = "Enter the lower limit",280),
                         textInput("limitingvalueeu", label = "Enter the upper limit",290)
        )


      ),

      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("printpaper1"),
        plotOutput("output1"),
        
        plotOutput("output2"),
        verbatimTextOutput("printpaper2")
      )
   )
)


server <- function(input, output) {

  output$printpaper1 <-renderPrint({

    if(input$Distribution == "Binomial")
    {
    cat(sprintf("\nBinomial Probability Distribution\n"))
    cat(sprintf("\nLet X be the Random variable which follows Binomial Distribution\n"))
    cat(sprintf("\nThe parameters of the Binomial Distribution are as follows\n"))
    cat(sprintf("\nThe number of trials  is %g",input$samplesize))
    cat(sprintf("\nThe probability of success is %g", input$prob))
    expectednumber  = input$samplesize * input$prob
    standarddeviation = sqrt(expectednumber*(1-input$prob))
    cat(sprintf("\nThe expected number of the distribution is %f",expectednumber))
    cat(sprintf("\nThe standard deviation of the distribution  is %f", standarddeviation))
    cat(sprintf("\nProbability(X %s %g)",input$signbinom,input$limitingvalue))

    if(input$signbinom == "=")
      probvalue = dbinom(x = input$limitingvalue,size = input$samplesize, prob = input$prob)
    if(input$signbinom == "<=")
      probvalue = pbinom(q = input$limitingvalue,size = input$samplesize, prob = input$prob)
    if(input$signbinom == ">=")
      probvalue = 1-pbinom(q = (input$limitingvalue - 1),size = input$samplesize, prob = input$prob)
      cat(sprintf(" : %f",probvalue))
    }
    if(input$Distribution == "Poisson")
    {
      cat(sprintf("\nPoisson Probability Distribution\n"))
      cat(sprintf("\nLet X be the Random variable which follows Poisson Distribution\n"))
      cat(sprintf("\nThe parameters of the Poisson Distribution are as follows\n"))
      cat(sprintf("\nThe rate(lambda) is %f",input$rate))
      expectednumber  = input$rate
      standarddeviation = sqrt(input$rate)
      cat(sprintf("\nThe expected number of the distribution is %f",expectednumber))
      cat(sprintf("\nThe standard deviation of the distribution  is %f", standarddeviation))
      cat(sprintf("\nProbability(X %s %g)",input$signpoisson,input$limitingvaluep))

      if(input$signpoisson == "=")
        probvalue = dpois(x = input$limitingvaluep, lambda= input$rate)
      if(input$signpoisson == "<=")
        probvalue = ppois(q = input$limitingvaluep,lambda= input$rate)
      if(input$signpoisson == ">=")
        probvalue = 1-ppois(q = (input$limitingvaluep - 1),lambda= input$rate)
      cat(sprintf(" : %f",probvalue))
    }
    if(input$Distribution == "Normal")
    {
      cat(sprintf("\nNormal Probability Distribution\n"))
      cat(sprintf("\nLet X be the Random variable which follows Normal Distribution\n"))
      cat(sprintf("\nThe parameters of the Normal Distribution are as follows\n"))
      expectednumber  = as.numeric(input$mean)
      standarddeviation = as.numeric(input$sd)
     limitingvaluen = as.numeric(input$limitingvaluen)
     
      cat(sprintf("\nThe mean of the distribution is %f",expectednumber))
      cat(sprintf("\nThe standard deviation of the distribution  is %f", standarddeviation))
      cat(sprintf("\nProbability(X %s %f)",input$signnormal,limitingvaluen))

      if(input$signnormal == "<=")
        probvalue = pnorm(q = limitingvaluen, mean = expectednumber,sd = standarddeviation)

      if(input$signnormal == ">=")
        probvalue = 1-pnorm(q = limitingvaluen,mean = expectednumber,sd = standarddeviation)

      cat(sprintf(" : %f",probvalue))
      


    }

    if(input$Distribution == "Exponential")
    {
      cat(sprintf("\nExponential Probability Distribution\n"))
      cat(sprintf("\nLet X be the Random variable which follows Exponential Distribution\n"))
      cat(sprintf("\nThe parameters of the Exponential Distribution are as follows\n"))
      expectednumber  = as.numeric(input$meantime)
      standarddeviation = as.numeric(input$meantime)
      limitingvaluee = as.numeric(input$limitingvaluee)
      cat(sprintf("\nThe mean of the distribution is %f",expectednumber))
      cat(sprintf("\nThe standard deviation of the distribution  is %f", standarddeviation))
      cat(sprintf("\nProbability(X %s %f)",input$signexp,limitingvaluee))

      if(input$signexp == "<=")
        probvalue = pexp(q = limitingvaluee, rate = 1/expectednumber)

      if(input$signexp == ">=")
        probvalue = 1-pexp(q = limitingvaluee,rate = 1/expectednumber)
      cat(sprintf(" : %f",probvalue))
    }

  })
  output$output1<-renderPlot({

    if(input$Distribution == "Binomial")
    {
    k =  seq(0,input$samplesize, by = 1)
    #curve(dbinom(x, input$samplesize,input$prob),xlim=c(0,input$samplesize),main='Binomial')
   plot(k,dbinom(k,input$samplesize,input$prob), xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Binomial Probability Distribution" )
   lines(k, dbinom(k,input$samplesize,input$prob), col = "red", lwd = 2)
   k1 =  seq(0,input$limitingvalue,by = 1)
   lines(k1, dbinom(k1,input$samplesize,input$prob), col = "blue", lwd = 2)
   #polygon(k1, dbinom(k1,input$samplesize,input$prob), col = "blue")
    }

    if(input$Distribution == "Poisson")
    {
      k =  seq(0,20, by = 1)
      #curve(dbinom(x, input$samplesize,input$prob),xlim=c(0,input$samplesize),main='Binomial')
      plot(k,dpois(k,input$rate), xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Poisson Probability Distribution" )
      lines(k, dpois(k,input$rate), col = "red", lwd = 2)
      k1 =  seq(0,input$limitingvaluep,by = 1)
      lines(k1, dpois(k1,input$rate), col = "blue", lwd = 2)
     # polygon(k1, dpois(k1,input$rate), col = "blue")
    }

    if(input$Distribution == "Normal")
    {
      mean = as.numeric(input$mean)
      sd = as.numeric(input$sd)
      limitingvaluen = as.numeric(input$limitingvaluen)
      x <- seq(-3.5,3.5,length=100)*sd + mean
      y <- dnorm(x,mean,sd)
    #  plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Normal Probability Distribution",col = "red")
      if(input$signnormal == "<=")
    {
        plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Normal Probability Distribution",col = "red")
      polygon(c( x[x<=limitingvaluen], limitingvaluen ),  c(y[x<=limitingvaluen],0 ), col="blue")
     }
if(input$signnormal == ">=")
    {
       plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Normal Probability Distribution")
        polygon(c( x[x>=limitingvaluen], limitingvaluen ),  c(y[x>=limitingvaluen],0 ), col="red")
      }

     }
    if(input$Distribution == "Exponential")
    {
      limitingvaluee = as.numeric(input$limitingvaluee)
      expectednumber  = as.numeric(input$meantime)
      x <- seq(0,expectednumber*2,0.01)
      y <- dexp(seq(0,expectednumber*2,0.01),rate= 1/ expectednumber)
      plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Exponential Probability Distribution",col = "red")
      if(input$signexp == "<=")
      polygon(c(0, x[x<=limitingvaluee], limitingvaluee ),  c(0,y[x<=limitingvaluee],0 ), col="blue")
      if(input$signexp == ">=")
      polygon(c( x[x>=limitingvaluee], limitingvaluee ),  c(y[x>=limitingvaluee],0), col="red")
       }
  })


  output$output2<-renderPlot({
    if(input$Distribution == "Normal")
    {
      mean = as.numeric(input$mean)
      sd = as.numeric(input$sd)
      limitingvaluenl = as.numeric(input$limitingvaluenl)
      limitingvaluenu = as.numeric(input$limitingvaluenu)
      x <- seq(-3.5,3.5,length=100)*sd + mean
      y <- dnorm(x,mean,sd)
      plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Normal Probability Distribution Between Upper and Lower Limits",col = "red")
      polygon(c(limitingvaluenl,x[x<=limitingvaluenu & x>=limitingvaluenl], limitingvaluenu ),  c(0,y[x<=limitingvaluenu & x>=limitingvaluenl],0 ), col="red")


    }
    if(input$Distribution == "Exponential")
    {
      limitingvalueel = as.numeric(input$limitingvalueel)
      limitingvalueeu = as.numeric(input$limitingvalueeu)
      expectednumber  = as.numeric(input$meantime)
      x <- seq(0,expectednumber*2,0.01)
      y <- dexp(seq(0,expectednumber*2,0.01),rate= 1/ expectednumber)
      plot(x, y, type="l",xlab = "Values taken by Random Variable X", ylab = " Discrete Probability Value",main = "Exponential Probability Distribution Between Upper and Lower Limits",col = "red")
      polygon(c(limitingvalueel,x[x<=limitingvalueeu & x>=limitingvalueel], limitingvalueeu ),  c(0,y[x<=limitingvalueeu & x>=limitingvalueel],0 ), col="red")
    }

  })
  output$printpaper2 <-renderPrint({
    
    if(input$Distribution == "Normal")
    {
      expectednumber  = as.numeric(input$mean)
      standarddeviation = as.numeric(input$sd)
      limitingvaluenl = as.numeric(input$limitingvaluenl)
      limitingvaluenu = as.numeric(input$limitingvaluenu)
      cat(sprintf("\nProbability(%f <= X <= %f)",limitingvaluenl,limitingvaluenu))
      probvaluebetween = pnorm(q = limitingvaluenu, mean = expectednumber,sd = standarddeviation)-pnorm(q = limitingvaluenl, mean = expectednumber,sd = standarddeviation)
      cat(sprintf(" : %f",probvaluebetween))
    }
    if(input$Distribution == "Exponential")
      
    { expectednumber  = as.numeric(input$meantime)
      limitingvalueel = as.numeric(input$limitingvalueel)
      limitingvalueeu = as.numeric(input$limitingvalueeu)
      cat(sprintf("\nProbability(%f <= X <= %f)",limitingvalueel,limitingvalueeu))
      probvaluebetween = pexp(q = limitingvalueeu,rate = 1/expectednumber )-pexp(q = limitingvalueel, rate = 1/expectednumber)
      cat(sprintf(" : %f",probvaluebetween))
      }
  })
  output$preface <-renderPrint({

    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))

  })






}
# Run the application
shinyApp(ui = ui, server = server)

