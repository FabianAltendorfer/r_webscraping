# install.packages("rvest")
# install.packages("readxl")
# install.packages("RODBC")

library(rvest)
library(readxl)
library(stringr)
library(RODBC)

conn <- odbcDriverConnect(paste0("DRIVER={FINCOM2};Server=SQLEXPRESS;integrated security=true;Database=fincom_realestate"))


file_path <- "C:/Users/P42518/OneDrive - FH OOe/Dokumente/Privat/Bachelorarbeit/Webscraping/R_WebScraping/Oberösterreich_Bezirke.xlsx"
bezirke <- read_excel(file_path, col_names = FALSE)


# We have to clean bezirke because we need hyphen instead of whitespaces for the URL and convert Umlaute to UTF-8 format
replace_whitespace <- function(x) {
  return(gsub("\\s", "-", x))
}

bezirke_cleaned <- apply(bezirke, MARGIN = 1:2, FUN = replace_whitespace)
bezirke_cleaned <- gsub("ä", "ae", bezirke_cleaned)
bezirke_cleaned <- gsub("ö","oe", bezirke_cleaned)
bezirke_cleaned <- as.data.frame(bezirke_cleaned)
base_url <- "https://www.willhaben.at/iad/immobilien/haus-kaufen/oberoesterreich/"

# Get the number of listings from the website
get_numberoflistings = function (bezirk) {
  link = paste(base_url,bezirk, sep = "")
  page = read_html(link)
  numberoflistings_text = page %>% html_nodes ("#result-list-title") %>% html_text() #Get Number of listings from page
  numberoflistings <- as.numeric(str_extract(numberoflistings_text, "^([0-9]+)")) #REGEX in place to get the number
  print(link)
  print(numberoflistings_text)
  print(numberoflistings)
}

# Calculate the number of pages
get_numberofpages = function (numberlistings) {
  numberpages_raw = numberlistings / 5
  numberpages <- ceiling(numberpages_raw)
  print(numberpages)
}

# Get HREF Links
get_href = function (bezirk) {
  numberoflistings = get_numberoflistings(bezirk)
  numberofpages = get_numberofpages(numberoflistings)
  
  estate_links = c()
  
  for (counter in 1:numberofpages) {
    link = paste(base_url, bezirk, "?rows=5&page=", counter, sep = "")
    page = read_html(link)
    estate_link = page %>% html_nodes(".jikMUW") %>% html_attr("href") %>% paste("https://www.willhaben.at", ., sep = "")
    estate_links = c(estate_links, estate_link)
    print(link)
    print(estate_link)
  }
  
  return(estate_links)
}


### TEST ###
hrefs = lapply(bezirke_cleaned[[1]], FUN = get_href)

hrefs <- as.list(hrefs)
class(hrefs)
hrefs_dataframe <- data.frame(hrefs = unlist(hrefs))
write.csv(hrefs_dataframe, "hrefs_dataframe.csv", row.names = FALSE)
hrefs <- Filter(function(x) is.character(x) && length(x) == 1, unlist(hrefs))

# Get the details of the pages
get_details <- function(hrefs) {
  # Check if hrefs is a list of strings
  #if (!is.list(hrefs) || any(sapply(hrefs, function(x) !is.character(x) || length(x) != 1))) {
  #  stop("Input 'hrefs' must be a list of strings.")
  #}
  
  property_details <- data.frame()
  
  for (link in hrefs) {
    print(link)
    property_page <- read_html(link)
    
    title <- property_page %>% html_node(".hJpLEP") %>% html_text(trim = TRUE)
    squaremeter <- property_page %>% html_node(".hLtuRn:nth-child(1) .hDGjys") %>% html_text(trim = TRUE)
    numberofrooms <- property_page %>% html_node(".hLtuRn+ .hLtuRn .hDGjys") %>% html_text(trim = TRUE)
    objecttype <- property_page %>% html_node(".eaMuLJ .kVJxYR") %>% html_text(trim = TRUE)
    price <- property_page %>% html_node(".fJoDoI") %>% html_text(trim = TRUE)
    objectlocation <- property_page %>% html_node(".cKvzO .Box-sc-wfmb7k-0") %>% html_text(trim = TRUE)
    objectinformation <- property_page %>% html_node(".cKvzO+ .Box-sc-wfmb7k-0 .hWFZNG") %>% html_text(trim = TRUE)
    objectfeatures <- property_page %>% html_node(".cKvzO~ .Box-sc-wfmb7k-0+ .Box-sc-wfmb7k-0 .hWFZNG") %>% html_text(trim = TRUE)
    objectdescription <- property_page %>% html_node(".jwwABQ") %>% html_text(trim = TRUE)
    additionalinformation <- property_page %>% html_node(".epoBCL .Box-sc-wfmb7k-0:nth-child(3)") %>% html_text(trim = TRUE)
    
    # Combine the extracted details
    details <- data.frame(title = title, squaremeter = squaremeter, numberofrooms = numberofrooms, objecttype = objecttype, price = price, objectlocation = objectlocation, objectinformation = objectinformation, objectfeatures = objectfeatures, objectdescription = objectdescription, additionalinformation = additionalinformation, stringsAsFactors = FALSE)
    print(details)
    Sys.sleep(0.5)
    
    # Append the details to the property_details data frame
    property_details <- rbind(property_details, details)
  }
  
  return(property_details)
}

# Save property details in database
sqlSave(conn, property_details, "property_details", rownames = FALSE, append = TRUE)

# Close the connection
odbcClose(conn)

# Usage example
property_details <- get_details(hrefs)

write.csv(property_details, "property_details.csv", row.names = FALSE)



### TEST ###
#test_hrefs <- c(
#  "https://www.willhaben.at/iad/immobilien/d/haus-kaufen/oberoesterreich/wels-land/steinhaus-belagsfertiger-bungalow-mit-741m-grundstueck-655520209/",
#  "https://www.willhaben.at/iad/immobilien/d/haus-kaufen/oberoesterreich/wels-land/steinhaus-belagsfertiges-haus-mit-747m-grundstueck-655515091/",
#  "https://www.willhaben.at/iad/immobilien/d/haus-kaufen/oberoesterreich/wels-land/sonniges-einfamilienhaus-631606885/"
#)

# Call the get_details function with the test list
#test_property_details <- get_details(test_hrefs)

# Print the result
#print(test_property_details)

