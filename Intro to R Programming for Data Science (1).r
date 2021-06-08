
require("httr")
require("rvest")

install.packages("httr")
install.packages("rvest")
library(dplyr)
library(magrittr)
library(httr)
library(rvest)


get_wiki_covid19_page <- 'https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country'

response <- GET(get_wiki_covid19_page)
  

# Call the get_wiki_covid19_page function and print the response
response

# Get the root html node from the http response in task 1 
root_node <- read_html(get_wiki_covid19_page)
root_node

# Get the table node from the root html node
table_node <- html_node(root_node, "table")
table_node

# Read the table node and convert it into a data frame, and print the data frame for review
data_frame <- html_table(table_node)
data_frame


# Print the summary of the data frame
summary(data_frame)

preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units[b]"] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}



# call `preprocess_covid_data_frame` function and assign it to a new data frame
new_data_frame <- preprocess_covid_data_frame(data_frame)

# Print the summary of the processed data frame again
summary(new_data_frame)

# Export the data frame to a csv file
write.csv(new_data_frame, "covid.csv")

# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
# File path
print(file_path)
file.exists(file_path)

## Download a sample csv file
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Read covid_data_frame_csv from the csv file
new_data_frame
# Get the 5th to 10th rows, with two "country" "confirmed" columns
new_data_frame[5:10, c(1,4)] 

# Get the total confirmed cases worldwide
(total_confirmed_cases <- colSums(new_data_frame[4]))

# Get the total tested cases worldwide
(total_tested_cases <- colSums(new_data_frame[3]))
# Get the positive ratio (confirmed / tested)
(positive_ratio <- total_confirmed_cases / total_tested_cases)


# Get the `country` column
country <- new_data_frame['country']
country <- as.factor(country)
# Check its class (should be Factor)
class(country)
# Conver the country column into character so that you can easily sort them
country_character <- as.character(country)
# Sort the countries AtoZ
new_data_frame[order(new_data_frame$country, na.last = FALSE),]
# Sort the countries ZtoA
sorted_country <- new_data_frame[order(new_data_frame$country, na.last = TRUE),]
# Print the sorted ZtoA list
print(sorted_country)

# Use a regular expression `United.+` to find matches
(united <- grep("United.+", new_data_frame$country, value = TRUE))
# Print the matched country names
united

# Select a subset (should be only one row) of data frame based on a selected country name and columns
(country1 <- new_data_frame[3,c(1,4,7)])

# Select a subset (should be only one row) of data frame based on a selected country name and columns
(country2 <- new_data_frame[7,c(1,4,7)])

Algeria_ratio <- new_data_frame[3,7]
Argentina_ratio <- new_data_frame[7,7]

if (Algeria_ratio > Argentina_ratio) {
    return("Algeria has a larger ratio of confirmed cases to population")
} else {
    return("Argentina has a larger ratio of confirmed cases to population")
}


# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
confirmed_population_ratio <- new_data_frame[,c(1,7)]

low_risk_countries <- new_data_frame %>%
                       select(country, confirmed.population.ratio) %>%
                          filter(confirmed.population.ratio < 0.01)

head(low_risk_countries,10)


