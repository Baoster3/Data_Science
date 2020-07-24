#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)

iris <- read.csv("../RTut/Iris.csv")

summary(iris)

#The columns were put in specific variables for short hand
SepalW <- (iris$Sepal.Width)
SepalL <- (iris$Sepal.Length)

PetalW <- (iris$Petal.Width)
PetalL <- (iris$Petal.Length)

# Different data frames were created to make sure the line graphed
# Will display on a specific intended plot
SepalWSepalL.df <- data.frame("Sepal Width"=SepalW,
                              "Sepal Length"=SepalL)

PetalWPetalL.df <- data.frame("Petal Width"=PetalW,
                              "Petal Length"=PetalL)

SepalWPetalW.df <- data.frame("Sepal Width"=SepalW,
                              "Petal Width"=PetalW)

SepalLPetalL.df <- data.frame("Sepal Length"=SepalL,
                              "Petal Length"=PetalL)

SepalWPetalL.df <- data.frame("Sepal Width"=SepalW,
                              "Petal Length"=PetalL)

SepalLPetalW.df <- data.frame("Sepal Length"=SepalL,
                              "Petal Width"=PetalW)

#Model and Linear Model #1
lm_1 = lm(SepalL~SepalW, data=SepalWSepalL.df)
plot(SepalWSepalL.df, pch=16, col="blue",
     main="Sepal Width vs. Sepal Length #1",
     xlab="Sepal Width",
     ylab="Sepal Length",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_1, col='red')#Plots the regression line

#Model and Linear Model #2
lm_2 = lm(PetalL~PetalW, data=PetalWPetalL.df)
plot(PetalWPetalL.df, pch=16, col="blue",
     main="Petal Width vs. Petal Length #2",
     xlab="Petal Width",
     ylab="Petal Length",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_2, col='red')#Plots the regression line

#Model and Linear Model #3
lm_3 = lm(PetalW~SepalW, data=SepalWPetalW.df)
plot(SepalWPetalW.df, pch=16, col="blue",
     main="Sepal Width vs. Petal Width #3",
     xlab="Sepal Width",
     ylab="Petal Width",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_3, col='red')#Plots the regression line

#Model and Linear Model #4
lm_4 = lm(PetalL~SepalL, data=SepalLPetalL.df)
plot(SepalLPetalL.df, pch=16, col="blue",
     main="Sepal Length vs. Petal Length #4",
     xlab="Sepal Length",
     ylab="Petal Length",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_4, col='red')#Plots the regression line

#Model and Linear Model #5
lm_5 = lm(PetalL~SepalW, data=SepalWPetalL.df)
plot(SepalWPetalL.df, pch=16, col="blue",
     main="Sepal Width vs. Petal Length #5",
     xlab="Sepal Width",
     ylab="Petal Length",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_5, col='red')#Plots the regression line

#Model and Linear Model #6
lm_6 = lm(PetalW~SepalL, data=SepalLPetalW.df)
plot(SepalLPetalW.df, pch=16, col="blue",
     main="Sepal Length vs. Petal Width #6",
     xlab="Sepal Length",
     ylab="Petal Width",
     xlim=c(0,8),
     ylim=c(0,8))
abline(lm_6, col='red')#Plots the regression line
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sepal vs. Petal Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of observations:",
                        min = 0,
                        max = 150,
                        value = 25,
                        step = .5)
        ),

        # Show a plot of the generated distribution
        mainPanel("Sepal Width vs. Sepal Length",
           plotOutput("lm_1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$lm_1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- SepalWSepalL.df
        n <- seq(min(x), max(x), length.out = input$n + 1)

        # draw the scatter plot with the specified number of bins
        plot(x, col = 'blue',
             xlab="Sepal Width", ylab="Sepal Length", xlim=c(0,10),
             ylim=c(0,10))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
