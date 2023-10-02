library(shiny)

fluidPage(

    # Application title
    titlePanel("Predictive Text Model"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
                helpText("Type a string or phrase of any length, and click predict to view the next word prediction."),
                textInput("user_input", label = h5("User Input Box"), value = ""),
                submitButton("Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
                h5("Predicted Next Word: "),
                textOutput("predicted_next_word")
        )
    )
)
