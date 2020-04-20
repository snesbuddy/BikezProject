
# install relevant packages
required_packages <- c("tidyverse","rvest","progress")
for (packge in required_packages) {
        if (packge %in% rownames(installed.packages())) {
                require(packge, character.only = TRUE)
        } else {
                install.packages(packge)
        }
}

# function to extract all bikes link
get_bikes_url <- function(from, to){
        base_url <- 'https://bikez.com'
        
        years <- from:to
        
        # progress bar
        pb <- progress_bar$new(
                format = "  Year loop: :percent [:bar] :current/:total [:elapsed <:eta, :tick_rates/it] ",
                total = length(years), clear = FALSE, width= 60)
        pb$tick(0)
        
        bike_links <- list()
        for(i in seq_along(years)){
                webpage <- read_html(paste0('https://bikez.com/year/index.php?year=', years[i]))
                links_even <- webpage %>% html_nodes('.even a') %>% html_attr('href') %>% 
                        gsub('\\.\\.', base_url, .) %>% 
                        .[grep('https', .)]
                links_odd <- webpage %>% html_nodes('.odd a') %>% html_attr('href') %>% 
                        gsub('\\.\\.', base_url, .) %>% 
                        .[grep('https', .)]
                
                bike_links[[i]] <- c(links_even, links_odd) %>% unique()
                # svMisc::progress(i, length(years))
                pb$tick()
        }
        return(unlist(bike_links))
}

bike_links <- get_bikes_url(1985, 2020)


# function to parse bike webpages and get info
parse_bike_links <- function(link){

        pb <- progress_bar$new(
                format = "  Bike loop: :percent [:bar] :current/:total [:elapsed <:eta, :tick_rates/it] ",
                total = length(link), clear = FALSE, width= 60)
        pb$tick(0)
        
        records <- list()
        for(i in seq_along(link)){
                webpage <- tryCatch(read_html(link[i]),
                                    error=function(e){return(e)})
                # error handler
                if(is(webpage, "error")){
                        next
                }
                
                model_name <- webpage %>% html_nodes('tr:nth-child(2) td+ td') %>% html_text(trim = TRUE) %>% .[1]
                year <- webpage %>% html_nodes('tr:nth-child(3) td+ td') %>% html_text(trim = TRUE) %>% .[1]
                displacement <- webpage %>% html_nodes('tr:nth-child(7) td+ td') %>% html_text(trim = TRUE) %>% .[1]
                if(is.na(displacement)){displacement <- webpage %>% html_nodes('tr:nth-child(8) td+ td') %>% html_text(trim = TRUE) %>% .[1]}
                records[[i]] <- data.frame(url = link[i], model_name, year, displacement, stringsAsFactors = FALSE)
                pb$tick()
        }
        df <- data.table::rbindlist(records)    # bind 'records' dataframes
        return(df)
}

bikes_data <- parse_bike_links(bike_links)

saveas <- paste0("bikez_", Sys.time(), ".csv") %>% gsub(":","",.)
readr::write_csv(bikes_data, saveas)
message("\nBikez successfully saved as ", saveas," in the current directory")

