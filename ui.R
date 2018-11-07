library(shiny)
library(gmapsdistance)
library(shinyTime)
library(lubridate)
fluidPage(
  titlePanel("Travel Time Estimator To Virginia Port Authority"),
  
  # this is magic that grabs current location if allowed.
  # see https://github.com/AugustT/shiny_geolocation
  tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
			  
  column(4, wellPanel(
	  radioButtons("origin", "Trip Origin", c("Current Location"="cur_pos", "Enter Trip Origin (fill in below):"="entered_pos")),
	  textInput("entered_pos_text", "Street Address City State separated by spaces. No punctuation.", "1030 University Blvd Suffolk VA"),
	  tags$br(),
	  tags$br(),
    dateInput('date2',
      label = paste('Trip Date in',
        'dd/mm/yy format:'),
      value = as.character(Sys.Date()),
      min = Sys.Date(), max = Sys.Date() + 365,
      format = "dd/mm/yy",
      startview = 'year'),
    tags$br(),
	radioButtons("time", "Trip Start Time", c("Current Time"="cur_time", "Enter Trip Start Time (fill in below H/M/S format):"="entered_time")),
	# from shinyTime pacakage
	timeInput("entered_time_input", "", value = Sys.time()),
	tags$br(),
	tags$br(),
    sliderInput("quant", "Desired likelihood of arriving at or before estimated time:",
      min = 0, max = 100, value = 50
    ),
	tags$br(),
	tags$br(),
    tags$br(),
	# this is the button that initiates a simulation run
  actionButton("get_estimates", "Simulate Trip & Get Estimated Travel Time")
 )),
 # output elements
  column(8,
	verbatimTextOutput("from"),
    verbatimTextOutput("dateText2"),
	verbatimTextOutput("time_output"),
	plotOutput(outputId = "distPlot"),
	verbatimTextOutput("message")
  )
  
)