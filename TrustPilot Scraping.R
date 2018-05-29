library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(xts)
library(infer) #for hypothesis testing
library(ggplot2)

url1 <- "http://www.trustpilot.com/review/www.amazon.com"
url2 <- "https://www.trustpilot.com/review/www.airbnb.com"

#Create a function to extract the last review page
get_last_page <- function(link){
  pages_data <- link %>%
                  html_nodes(".pagination-page") %>% #extract all nodes with class pagination-page
                  html_text() #extract the values as a list
  pages_data[length(pages_data)-1] %>% unname() %>% as.numeric()  #take the second last value and convert it to a number              
  
}



#Get review content
get_review <- function(link){
  link %>%
    html_nodes(".review-info__body__text") %>%
    html_text() %>%
    str_trim() %>% #remove unnecessary white spaces
    unlist()
    
}

#Get reviewer's names
get_reviewer_name <- function(link){
  link %>%
    html_nodes(".consumer-info__details__name") %>%
    html_text() %>%
    str_trim() %>% #remove unnecessary white spaces
    unlist()  
}

#Get review dates
get_review_dates <- function(link){
  status <- link %>% 
            html_nodes('time') %>% #the status info is a tag attribute this time
            html_attrs() %>%
            map(2)%>% #extract the second element
            unlist()
  dates <- link %>%
          html_nodes("time") %>%
          html_attrs() %>%
          map(1) %>% #extract the first element
          ymd_hms() %>% #parse the string into a POSIX date vector
          unlist()
  #combine the status and the date info ti filter one via the other
  return_dates <- tibble(status=status, dates = dates) %>%
                  filter(status == "ndate") %>% #only filter the rows with status = ndate
                  pull(dates) %>% #Select and convert to vector
                  as.POSIXct(origin = "1970-01-01 00:00:00")
  
  length_review <- length(get_review(link))
  
  return_review <- if (length(return_dates) > length_review){
    return_dates[1:length_review]
  }else{
    return_dates
  }
  return_review
  }
  
#Get star rating
get_star_rating <- function(link){
  pattern <- 'count-'%R%capture(DIGIT) #look for the first digit after 'count-'
  ratings <- link %>%
              html_nodes(".star-rating") %>%
              html_attrs()%>%
              str_match(pattern = pattern) %>%
              unlist()
  ratings <- ratings[-1, 2]
  
  #Test if any comment is reported and thus the rating is missing
  temporary_review <- get_review(link) #load the reviews temporarily
  for (i in 1:length(temporary_review)){
    if (temporary_review[i] == ""){
      ratings <- append(ratings, 0, after = i - 1)
    }
  }
  ratings
}

#Combine all data into a tibble
get_data_table <- function(link, company_name){
  #Extract the basic data from the HTML
  reviews <- get_review(link)
  reviewer_names <- get_reviewer_name(link)
  dates <- get_review_dates(link)
  ratings <- get_star_rating(link)
  
  
  
  #Combine everything into a tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates,
                          rating = ratings,
                          review = reviews)
  
  #Tag the individual data with the company name
  combined_data %>% mutate(company = company_name) %>% select(company, reviewer, date, rating, review)
}

#Add the read HTML to the get_data_table funciton to make it more efficient in procesing URL
get_data_from_URL <- function(link, company_name){
  html <- read_html(link)
  get_data_table(html, company_name)
}

#Apply the function over a list of URLs
scrape_write_table <- function(url, company_name){
  first_page <- read_html(url)
  latest_page_number <- get_last_page(first_page)
  list_of_pages <- str_c(url,"?page=", 1:latest_page_number) #generate the target URLs
  #Apply the extraction and bind the individual results back into one table, which is then written as a tsv file into the working directory
  list_of_pages %>%
    map(get_data_from_URL, company_name) %>% #apply to all URLs
    bind_rows() %>% #combine all the tibbles into one tiblle
    write_tsv(str_c(company_name,".tsv")) #write a tab-separated file

}

par(mfrow = c(2,2))
plot(amazon_monthly_avg)
plot(amazon_month_count)
plot(airbnb_monthly_avg)
plot(airbnb_month_count)



