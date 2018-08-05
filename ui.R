library(shiny)


shinyUI(
	fluidPage(
		theme = "bootstrap.css",
		title = "Words predictor by Andrea Alberto",
		HTML(" <font><b> Word Predictor implemenented by Andrea Alberto based on Katz Back-Off Model</b></font> "),
		HTML("<hr>"),
		br(),
		fluidRow(
			column( "Instructions", width = 4, wellPanel(
				HTML(
					"Please, enter the sentence in the <font color=\"#00BBF\"><b>Input Sentence</b></font> area.<br>",
					"Use a space after the last words to get the word predicted. <br>",
					"The word predicted with the highest probability is displayed in <font color=\"#228B22\"><b> green </b></font> after the original sentence you have entered.<br>",
					"Alternative words, if found, are displayed in <font color=\"#006400\"><b>dark green</b></font> in the <font color=\"#00BBF\"><b> \'Other Possible Predictions\'</b></font> area.<br> "
					)
				)
			),
			column("App Area", width = 4, wellPanel(
			   textInput("sentence", "Input Sentence", value = "", width = '400px', placeholder = "enter here your sentence"),
			   HTML("<hr> <font color=\"#00BBF\"><b>Word predicted</b></font>"),
			   br(),
			   htmlOutput("value"),  
			   HTML("<font color=\"#00BBF\"><b>Other Possible Predictions</b></font>"), 
			   htmlOutput("alternatives"), 
			   HTML("<hr>"),
			   br()                            
				)
			)			
		)
	)
        
)