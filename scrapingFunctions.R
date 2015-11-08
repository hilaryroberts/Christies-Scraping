options(stringsAsFactors = FALSE)

library(rvest)
library(stringr)


getauction <- function(url) {
    
    #Initialise DF
    output <- data.frame(character(), character(), numeric(), character(), numeric(), numeric(), character(), character(), character(), character(), character(), character())
    ordering <- c("Header", "Subheader", "Price Realised Value", "Price Realised Currency", "Minimum Estimate", "Maximum Estimate", "Lot Description","Provenance", "Pre-Lot Text", "Literature", "Exhibited", "Notes")
    colnames(output) <- ordering
    
    #Get lot-pages
    html <- read_html(url)
    works <- html_attr(html_nodes(html, ".chr-result-hd-link"), "href")
    nextp <- html_attr(html_nodes(html, ".chr-pager-next"), "href")
    while(!is.na(nextp)){
        html <- read_html(paste("http://www.christies.com", nextp, sep = ""))
        nworks <- html_attr(html_nodes(html, ".chr-result-hd-link"), "href")
        works <- append(works, nworks)
        nextp <- html_attr(html_nodes(html, ".chr-pager-next"), "href")
    }
    
    for (i in 1:length(works)) {
        html <- tryCatch(read_html(works[i]), error = function(e) NA)
        if(is.na(html)){next}
        
        header <- tryCatch(html_text(html_nodes(html, "h1")[1]), error = function(e) NA)
        if(!is.na(header)){header <- sub("\n", "", header)}
        
        subheader <- html_text(html_nodes(html, "h2")[3])
        subheader <- sub("\n", "", subheader)
        
        price_realised <- html_nodes(html, "#price-realized-wrapper > ul > li.sublist-item")
        price_realised <- html_text(price_realised)
        if(length(price_realised)<1){price_realised <- NA}
        price_realised_value <- str_extract(price_realised, "[0-9]+,[0-9]+,[0-9]+|[0-9]+,[0-9]+|[0-9]+")
        price_realised_currency <- str_extract(price_realised, "\\$|£|CNY|€|HK$|CHF|¬")
        if(!is.na(price_realised) && price_realised_currency == "¬"){
            price_realised_currency <- "€"
        }
        
        estimate <- html_text(html_nodes(html, "#lot-images-summary > div.lot-summary.copy > div.quick-facts > div.wrapper.estimate-wrapper > ul > li")[1])
        estimate <- str_extract_all(estimate, "[0-9]+,[0-9]+,[0-9]+|[0-9]+,[0-9]+|[0-9]+")
        estimate <- strsplit(estimate[[1]], " ")
        if(estimate[1] == "NULL"){
            min_est <- NA
            max_est <- NA
        } else {
            min_est <- estimate[[1]][1]
            max_est <- estimate[[2]][1]
        }
        
        
        
        #extract the meta-information
        meta <- html_text(html_nodes(html, ".overview"))
        for (p in 1:length(meta)) {
            meta[p] <- gsub("\r", "", meta[p])
            meta[p] <- gsub("\n", "", meta[p])
        }
        
        #find out what each element in meta designates
        meta_labels <- html_text(html_nodes(html, "#tabWindow1 h2"))
        meta_labels <- strsplit(meta_labels, '" "')
        
        names(meta) <- meta_labels
        if(is.na(names(meta[length(meta)]))) {names(meta)[length(meta)] <- "Notes"}
        
        results <- c(header, subheader, price_realised_value, price_realised_currency, min_est, max_est)
        names(results) <- c("Header", "Subheader", "Price Realised Value", "Price Realised Currency", "Minimum Estimate", "Maximum Estimate")
        results <- append(results, meta)
        
        output <- rbind(output, results[ordering], make.row.names = FALSE)
        colnames(output) <- ordering
       
    }
    
    return(output)
    
}



getmonth <- function(url) {
    html <- read_html(url)
    pages <- html_attr(html_nodes(html, ".description"), "href")
    pages <- pages[1:floor(length(pages)/2)*2]
    #pages <- sub("^/", "http://www.christies.com/", pages)
    pages <- str_extract(pages, "[0-9]+.aspx")
    pages <- sub(".aspx", "", pages[!is.na(pages)])
    pages <- paste("http://www.christies.com/lotfinder/salebrowse.aspx?intsaleid=", pages, "&viewType=list", sep = "")
    
    for (i in 1:length(pages)){
        result <- getauction(pages[i])
        html <- read_html(pages[i])
        title <- html_nodes(html, ".chr-sale-top-hd")
        title <- sub('<h2 class="chr-sale-top-hd">&#13;\n                                    &#13;\n                                    ', '', title)
        title <- sub('&#13;\n                                </h2>', '', title)
        result$auction <- title
        if(i == 1){output <<- result} 
        else {output <<- rbind(output, result)}
        
    }
    
    return(output)
}
