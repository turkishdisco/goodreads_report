#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# libraries
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(ggpubr)


# setwd
setwd("/Users/chrisdixon/G Drive/R/R script files/goodreads_report/")

# read in data
reads = read_csv("goodreads_library_export_curated.csv", na = c("", "NA")) %>% 
  clean_names() %>%
  select(-c(book_id))

# wrangling
reads = reads %>% filter(exclusive_shelf == "read")

# change 'mass market paperback' and 'perfect paperback' to paperback
reads = reads %>%
  mutate(binding = recode(binding,
                          "Mass Market Paperback" = "Paperback",
                          "Perfect Paperback" = "Paperback"))

# derive binding2 variable to denote 'digital' vs 'paper' categories
reads = reads %>%
  mutate(binding2 = case_when(
    binding=="ebook" | binding=="Kindle Edition" ~ "Digital",
    binding=="Hardcover" | binding=="Paperback" ~ "Paper"))

# truncate titles
reads$title_trunc = str_trunc(reads$title, 50, side=c("right"))
reads$title_trunc_25 = str_trunc(reads$title, 25, side=c("right"))

# Change dates to proper format
reads$date_read = as.Date(reads$date_read, format = "%d/%m/%Y")
reads$date_added = as.Date(reads$date_added, format = "%d/%m/%Y")

reads$year_read = year(reads$date_read)
reads$month_read = month(reads$date_read, label=T)

# Calculate time taken to read (displays as eg '26 Days')
reads$time_taken = reads$date_read - reads$date_added

# Split up Bookshelves
reads = reads %>% 
  separate(bookshelves, into=c("Fiction", "Subcat1", "Subcat2"), sep=",")

# create objects for referencing
date = Sys.Date()
this_year = lubridate::year(date)

this_year_pretty = format(date, format="%d %B %Y")

books = n_distinct(reads$title)
authors = n_distinct(reads$author)

fiction = reads %>% filter(Fiction=="fiction") %>% summarise(books = n()) %>% as.numeric()
fiction.pc = paste(round( (fiction / books)*100,0), "%", sep="")
nonfiction = reads %>% filter(Fiction=="non-fiction") %>% summarise(books = n()) %>% as.numeric()
nonfiction.pc = paste(round( (nonfiction / books)*100,0), "%", sep="")

digital = reads %>% filter(binding2=="Digital") %>% summarise(books = n()) %>% as.numeric()
digital.pc = paste(round( (digital / books)*100,0), "%", sep="")

paper = reads %>% filter(binding2=="Paper") %>% summarise(books = n()) %>% as.numeric()
paper.pc = paste(round( (paper / books)*100,0), "%", sep="")

last.title = as.character(reads[1,1])
last.author = as.character(reads[1,2])
last.date = reads[1,14] %>% pull()
last.date.pretty = format(last.date, format="%d %B %Y")
# last.date = format(last.date,"%d %B %Y")

reads$title_trunc_60 = str_trunc(reads$title, 60, side=c("right"))


# UI
ui <- fluidPage(
  titlePanel("Goodreads Summary"),
  
  # text
  HTML(paste0("<p> As of ", this_year_pretty, " I have read <b>", books, "</b> books by ", authors, " authors. </p>")),
  HTML(paste0("<p> The last book I read was <b>", last.title, "</b> by ", last.author, " on ", last.date.pretty, "</p>")),

  plotOutput("all_years_plot"),
  br(),
  HTML(paste0("<p> This includes <b>", fiction, "</b> fiction books (", fiction.pc, "), and <b>", nonfiction, "</b> non-fiction books (", nonfiction.pc, ").</p>")),
  HTML(paste0("<p>Of these, <b>", paper, "</b> were paper (", paper.pc, ") and <b>", digital, "</b> were digital (", digital.pc, ").</p>")),
  
  plotOutput("paper_vs_digital"),
  uiOutput("year_selector"),
  textOutput("book_count"),
  plotOutput("book_length"),
  tableOutput("book_list")
    
)





server <- function(input, output, session) {
  
  # all years graph
  output$all_years_plot <- renderPlot({
    reads %>% group_by(year_read) %>% 
      summarise(books_read = n()) %>% 
      na.omit() %>%
      ggplot(aes(year_read, books_read, label = books_read)) + theme_classic() +
      geom_bar(stat="identity", fill="#90bcd4") +
      geom_text(vjust=-0.3, size=5) +
      theme(panel.grid.major.y = element_line(colour = "grey80"),
            legend.title = element_blank(),
            legend.position = "top",
            plot.title = element_text(hjust=0.5),
            plot.subtitle = element_text(hjust=0.5),
            axis.text = element_text(size=10)) +
      ylab("Books Read") + theme(axis.title.x = element_blank())  +
      ggtitle("Books read by year") +
      scale_y_continuous(breaks=seq(0,50,10)) 
  })
  
  # paper vs digital graph
  output$paper_vs_digital = renderPlot({
    a = reads %>% 
      filter(!is.na(year_read)) %>%
      group_by(year_read, Fiction) %>%
      summarise(total = n()) %>%
      mutate(perc = total/sum(total)*100) %>%
      filter(Fiction == "fiction") %>%
      ggplot(aes(year_read, perc)) + theme_classic() +
      geom_hline(yintercept=50, colour="grey", linetype="dashed") +
      stat_summary(fun=mean, geom="line", linewidth=0.7, colour='#1F78B4') +
      ylab("Books Read (%)") + theme(axis.title.x = element_blank()) +
      ggtitle("Percent Fiction") +
      theme(panel.grid.major.y = element_line(colour="grey90"),
            plot.title = element_text(hjust=0.5),
            axis.text = element_text(size=10)) +
      expand_limits(y = c(0,100))
    
    # % Paper books over time
    b = reads %>% 
      filter(!is.na(year_read)) %>%
      group_by(year_read, binding2) %>%
      summarise(total = n()) %>%
      mutate(perc = total/sum(total)*100) %>%
      filter(binding2 == "Paper") %>%
      ggplot(aes(year_read, perc)) + theme_classic() +
      geom_hline(yintercept=50, colour="grey", linetype="dashed") +
      stat_summary(fun=mean, geom="line", linewidth=0.7, colour='#33A02C') +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      ggtitle("Percent Paper vs Kindle") +
      theme(panel.grid.major.y = element_line(colour="grey90"),
            plot.title = element_text(hjust=0.5),
            axis.text = element_text(size=10)) +
      expand_limits(y = c(0,100))
    
    # together
    ggarrange(a, b, ncol=2)
  })
  
  
  # dynamic year selector
  output$year_selector <- renderUI({
    years <- sort(unique(reads$year_read))
    selectInput("year", "Choose a year:", choices = years)
  })
  
  # Count books in the selected year
  output$book_count <- renderText({
    req(input$year)
    n <- reads %>% filter(year_read == input$year) %>% nrow()
    paste("Books read in", input$year, ":", n)
  })
  
  # time to read plot
  output$book_length = renderPlot({
    req(input$year)
    reads %>%
      filter(!is.na(number_of_pages)) %>%
      filter(year_read == input$year) %>%
      ggplot(aes(x=year_read, y=number_of_pages)) + 
      geom_boxplot(aes(group=year_read), alpha=0.7) +
      theme_classic()
  })
  
  # list of books read in a given year
  output$book_list <- renderTable({
    req(input$year)
    # Filter books for the selected year
    reads %>% 
      filter(year_read == input$year) %>% 
      mutate(my_rating = na_if(my_rating, 0),
             my_rating = as.character(my_rating),  
             my_rating = replace_na(my_rating, "")) %>%
      select(Title = title_trunc_60, Rating = my_rating) 
  })
}

shinyApp(ui = ui, server = server)
