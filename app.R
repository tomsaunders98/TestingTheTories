library(shiny)
library(tidyverse)
library(Cairo)
options(shiny.usecairo=T)

testnumbers <- read_csv(url("https://github.com/tomsaunders98/CovidTests/raw/master/Testnumbers.csv"))
countries <- list( "UK")

for(i in 1:nrow(testnumbers)){
  val = toString(testnumbers$DataType[i])
  if (val == "People"){
    temp = "Number of People Tested"
  }
  if (val == "Unclear"){
    temp = "Measurement Not Specified"
  }
  if (val == "Samples"){
    temp = "Number of Laboratory Samples Analysed"
  }
  if (val == "Cases"){
    temp = "Number of Cases Tested"
  }
  testnumbers$DataType[i] = temp
}

#6 of April when total test start to be counted
## Code for Test to 100,000
df0 <- testnumbers[7:(ncol(testnumbers)-11)]
testnumbers1 <- cbind(testnumbers[1], df0[-length(df0)] - df0[-1])
testnumbers1[testnumbers1 < 0] <- NA


linedata <- testnumbers1 %>%
    filter(Country %in% countries) %>%
    pivot_longer(2:ncol(testnumbers1), names_to="dates", values_to="Total") %>%
    drop_na(Total) %>%
    mutate(dates = as.Date(dates, "%d/%m/%Y")) %>%
    arrange(desc(dates)) 



## Code for all tests
testnumbers$datemax <- colnames(testnumbers[7:ncol(testnumbers)])[apply(testnumbers[7:ncol(testnumbers)],1,which.max)]
testnumbers$max <- apply(testnumbers[7:(ncol(testnumbers)-1)], 1, max,na.rm=TRUE)

testnumbersBar <- testnumbers %>% 
  #filter(Country %in% countries) %>%
  mutate(
    TestsPerCap = as.double(max)/as.double(Population),
    TestsPerMil = (as.double(max)/as.double(Population))*1000
  ) %>%
  arrange(desc(max)) %>%
  head(n=20)


# Develop Line Graph
maxnum = max(testnumbersBar$TestsPerMil, na.rm=TRUE) + 7
maxvalue = max(linedata$Total)
maxvalue1 = max(linedata$Total) + 10000
maxvalue2 = sort(linedata$dates,T)[2]

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head( tags$meta(name = "viewport", content = "width=1600")), 
    # Application title
    fluidRow(
        column(12,
            titlePanel("Test the Tories: 100,000 Tests a Day by May 1st")
        ),
        
        column(12,
               actionButton("SmallGraph", "Just UK"),
               actionButton("BigGraph", "Compare to Other Countries")
        ),
        column(12,
            plotOutput("distPlot",width = "100%")
        ),
        column(12,
               h3("What is this?"),
               p('On the 2nd of April Matt Hancock, the health secretary, said:',
                 tags$i('“I’m now setting the goal of 100,000 tests per day by the end of this month. That is the goal and I’m determined we’ll get there.”'),
               ),
               p('This website will track this claim right up until May 1st.'),
               h4("More Details"),
               p("The data on the number of tests for the UK and other countries is taken from my",
                 a("Tests Tracker", href="https://github.com/tomsaunders98/CovidTests"),
                 " and is updated daily."),
               p("If you have any questions please ask me on twitter",
                 a("@tomandthenews", href="https://twitter.com/tomandthenews"),".")
        )

    # Sidebar with a slider input for number of bins 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    BigC <- reactiveValues(plot = ggplot(data = linedata, aes(x = dates, y = Total), cex.lab=1.5, cex.axis=1.5 ) +
                               geom_line(col="#003f5c") +
                               geom_point(col="#003f5c") +
                               scale_y_continuous(limits=c(0,120000)) +
                               scale_x_date(limits = as.Date(c("2020-04-07","2020-05-01")))+
                               xlab("Dates")+
                               ylab("Tests Per Day") +
                               guides(color=FALSE) +
                               theme_minimal(base_size = 20) +
                               theme(legend.position="none") +
                               annotate("segment", x = min(linedata$dates), y = 100000, xend =as.Date("2020-05-01"), yend = 100000, colour = "#333333", alpha = .8, size=1) +
                               annotate("text", x = median(linedata$dates), y = 105000, label = "100,000 Tests Per Day", colour = "#333333", size=6) +
                               annotate("segment", x = max(linedata$dates), linetype = "dashed", y = max(linedata$Total), xend =as.Date("2020-05-01"), yend = 100000, colour = "#bc5090", size=1) +
                               annotate("text", x = maxvalue2, y = maxvalue1, label = paste("Current Tests: ", maxvalue), colour = "#003f5c", size=5) +
                               annotate("rect", xmin = max(linedata$dates), xmax = as.Date("2020-05-01"), ymin = 0, ymax = 100000,alpha = .1))
    observeEvent(input$BigGraph, {
        BigC$plot <- ggplot(data = testnumbersBar, aes(x = reorder(CountryNames, TestsPerMil), y = TestsPerMil, fill = DataType)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label=datemax, colour = DataType),  vjust=0.5, hjust=0, show.legend = FALSE) +
          scale_y_continuous(limits=c(0,maxnum)) +
          xlab("Countries ")+
          ylab("Tests Per Thousand") +
          ggtitle("COVID-19 Tests Completed Per Thousand") +
          labs(fill = "Data Collected As:") +
          coord_flip() +
          theme_minimal(base_size = 20)
    })
    
    observeEvent(input$SmallGraph, {
        BigC$plot <- ggplot(data = linedata, aes(x = dates, y = Total, color=Country), cex.lab=1.5, cex.axis=1.5 ) +
                        geom_line(col="#003f5c") +
                        geom_point(col="#003f5c") +
                        scale_y_continuous(limits=c(0,120000)) +
                        scale_x_date(limits = as.Date(c("2020-04-07","2020-05-01")))+
                        xlab("Dates")+
                        ylab("Tests Per Day") +
                        guides(color=FALSE) +
                        theme_minimal(base_size = 20) +
                        theme(legend.position="none") +
                        annotate("segment", x = min(linedata$dates), y = 100000, xend =as.Date("2020-05-01"), yend = 100000, colour = "#333333", alpha = .8, size=1) +
                        annotate("text", x = median(linedata$dates), y = 105000, label = "100,000 Tests Per Day", colour = "#333333", size=6) +
                        annotate("segment", x = max(linedata$dates), linetype = "dashed", y = max(linedata$Total), xend =as.Date("2020-05-01"), yend = 100000, colour = "#bc5090", size=1) +
                        annotate("text", x = maxvalue2, y = maxvalue1, label = paste("Current Tests: ", maxvalue), colour = "#003f5c", size=5) +
                        annotate("rect", xmin = max(linedata$dates), xmax = as.Date("2020-05-01"), ymin = 0, ymax = 100000,alpha = .1)
        BigC$height = "auto"
    })  

    output$distPlot <- renderPlot({
        BigC$plot
     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
