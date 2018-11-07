library(shiny)
library(gmapsdistance)
library(shinyTime)
library(lubridate)
library(ggplot2)
function(input, output, session) {

  distribution = c() # global variable to store monte carlo results
  set.api.key("YOUR API CODE GOES HERE") # get one at https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en
  

  # this is a useful document for api calls - https://cran.r-project.org/web/packages/gmapsdistance/gmapsdistance.pdf
  output$from <- renderText({ paste("Origin of trip:", 
  			switch(input$origin,
            	cur_pos = paste(input$lat, input$long),
            	entered_pos = input$entered_pos_text,
            	paste(input$lat, input$long)))
                            })
  
				
  output$dateText2 <- renderText({
    paste("Trip Date", as.character(input$date2))
  })
  output$time_output <- renderText(paste("Trip Time", 
         switch(input$time,
                   cur_time = strftime(Sys.time(), "%T"),
                   entered_time = strftime(input$entered_time_input, "%T"),
                   strftime(Sys.time(), "%T"))))
  

  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  # run a new monte carlo simulation
  observeEvent(input$get_estimates, {
	  port_for_call = as.character({ switch(input$origin,
	            	cur_pos = "36.912019,-76.3258407",
	            	entered_pos = "Virginia+Port+Authority+Norfolk+VA",
	            	paste(input$lat, input$long)) })
  
	  from_for_call = as.character({ switch(input$origin,
	            	cur_pos = paste0(input$lat, "+", input$long),
	            	entered_pos = gsub('[[:punct:] ]+',' ',input$entered_pos_text),
	            	paste(input$lat, input$long)) })
				
	  time_for_call <- as.POSIXct({ switch(input$time,
	                   cur_time = strftime(Sys.time()+60),
	                   entered_time = strftime(input$entered_time_input, "%T"),
	                   strftime(Sys.time()+60, "%T")) })
	  start_spot = gsub(" ", "+", from_for_call)
	  start_time = as.POSIXct(time_for_call, format="%m/%d/%Y %H:%M:%S")
	  start_date = as.POSIXct(paste(input$date2, format(start_time, format="%H:%M:%S")))
	  attr(start_date, "tzone") <- "UTC"
	  attr(start_time, "tzone") <- "UTC"
	  pes_results = gmapsdistance(origin = start_spot, destination =port_for_call, mode = "driving", traffic_model="pessimistic", dep_date = as.character(start_date), dep_time = format(start_time, format="%H:%M:%S"))
	  bg_results = gmapsdistance(origin = start_spot, destination =port_for_call, mode = "driving", traffic_model="best_guess", dep_date = as.character(start_date), dep_time = format(start_time, format="%H:%M:%S"))
	  opt_results = gmapsdistance(origin = start_spot, destination =port_for_call, mode = "driving", traffic_model="optimistic", dep_date = as.character(start_date), dep_time = format(start_time, format="%H:%M:%S"))
  
	  left_side_samples = rnorm(1000, mean = bg_results$Time, sd = (pes_results$Time - bg_results$Time)/3)
	  left_side_samples = left_side_samples[left_side_samples<=bg_results$Time]
	  right_side_samples = rnorm(1000, mean = bg_results$Time, sd = (bg_results$Time - opt_results$Time)/3)
	  right_side_samples = right_side_samples[right_side_samples>=bg_results$Time]
	  distribution = c(left_side_samples,right_side_samples)
	  output$distPlot <- renderPlot({
		  print("executed")
		  if (length(distribution)==0) return(NULL)
		  quantile_value = input$quant/100
		  value_at_quantile = quantile(distribution, probs=c(quantile_value))
		  output$message <- renderPrint({
   paste0("To have a ", input$quant,"%", " chance of arriving at or before your reservation, you should plan on your trip taking ", seconds_to_period(floor(value_at_quantile)), ".")
		  })
		  df <- data.frame(matrix(unlist(distribution), nrow=length(distribution), byrow=T))
		  df$Within.Time = TRUE
		  colnames(df) = c("Time.Estimate", "Within.Time")
		  df$Within.Time = as.logical(df$Time.Estimate <= value_at_quantile)
		  max_time = floor(max(distribution))
		  min_time = floor(min(distribution))
		  mean_time = floor(min_time + ((max_time - min_time)/2))
		  higher_than_mean = floor(mean_time + ((max_time - min_time)/4))
		  lower_than_mean = floor(mean_time - ((max_time - min_time)/4))
		  labels_for_plot = c(as.character(seconds_to_period(min_time)), as.character(seconds_to_period(lower_than_mean)), as.character(seconds_to_period(mean_time)), as.character(seconds_to_period(higher_than_mean)), as.character(seconds_to_period(max_time)))
	     ggplot(df, aes(Time.Estimate, fill=Within.Time)) + geom_histogram(bins=20, color="black") + scale_x_continuous(labels = labels_for_plot, breaks=c(min_time, lower_than_mean, mean_time, higher_than_mean, max_time))+ggtitle("Distribution of Simulated Travel Times")+xlab("Travel Times")+ylab("Higher Probability")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),axis.text.y=element_blank(),plot.title = element_text(size=16, face="bold"))+ guides(fill=FALSE)
		 
	  })
  })
 
  # if someone updates the probability then update the plot coloring and message
  output$distPlot <- renderPlot({
	  print("executed")
	  if (length(distribution)==0) return(NULL)
	  quantile_value = input$quant/100
	  value_at_quantile = quantile(distribution, probs=c(quantile_value))
	  df <- data.frame(matrix(unlist(distribution), nrow=length(distribution), byrow=T))
	  df$Within.Time = TRUE
	  colnames(df) = c("Time.Estimate", "Within.Time")
	  df$Within.Time = as.logical(df$Time.Estimate <= value_at_quantile)
	  output$message <- renderPrint({
paste0("To have a ", input$quant,"%", " chance of arriving at or before your reservation, you should plan on your trip taking ", seconds_to_period(floor(value_at_quantile)), ".")
	  })
	  max_time = floor(max(distribution))
	  min_time = floor(min(distribution))
	  mean_time = floor(min_time + ((max_time - min_time)/2))
	  higher_than_mean = floor(mean_time + ((max_time - min_time)/4))
	  lower_than_mean = floor(mean_time - ((max_time - min_time)/4))
	  labels_for_plot = c(as.character(seconds_to_period(min_time)), as.character(seconds_to_period(lower_than_mean)), as.character(seconds_to_period(mean_time)), as.character(seconds_to_period(higher_than_mean)), as.character(seconds_to_period(max_time)))
     ggplot(df, aes(Time.Estimate, fill=Within.Time)) + geom_histogram(bins=20, color="black") + scale_x_continuous(labels = labels_for_plot, breaks=c(min_time, lower_than_mean, mean_time, higher_than_mean, max_time))+ggtitle("Distribution of Simulated Travel Times")+xlab("Travel Times")+ylab("Higher Probability")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),axis.text.y=element_blank(),plot.title = element_text(size=16, face="bold"))+ guides(fill=FALSE)
  })
  

}