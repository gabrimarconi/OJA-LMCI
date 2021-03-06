


#libraries and data connection
#install.packages("wihoja")
library(wihoja)
open_oja_db()
#install.packages("tidyverse")
library(tidyverse)



country <- "IT"

check_company_names_fct <- function(country) {
  print(country)
  ###creating a table with company names' frequency, sorted by frequency or company name
  #query and deduplication
  #companies_names_query <- query_athena("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
  query <- paste0("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='",country,"' ORDER BY RAND()  LIMIT 1000000")
  companies_names_query <- query_athena(query)
  dim(companies_names_query)
  companies_names_query$dup <- ifelse(duplicated(companies_names_query$general_id), 1, 0)
  companies_names_query <- companies_names_query[companies_names_query$dup==0]
  #background checks
  table(companies_names_query$dup)
  dim(companies_names_query)
  
  #creating a table with company names' frequency, sorted by frequency or company name
  companies_names_dataframe <- as.data.frame(table(companies_names_query$companyname))
  colnames(companies_names_dataframe) <- c("companyname","Freq")
  companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))
  companies_names_dataframe_bynames <- arrange(companies_names_dataframe , companyname)
  str(companies_names_dataframe)
  
  
  #doing some standardisation of company names and dropping empty company names
  companies_names_dataframe$companyname <- str_to_lower(companies_names_dataframe$companyname)
  #companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
  companies_names_dataframe$companyname <- str_trim(companies_names_dataframe$companyname)
  companies_names_dataframe$companyname <- gsub(" ","_",companies_names_dataframe$companyname)
  companies_names_dataframe$notgood <- ifelse(companies_names_dataframe$companyname=="",1,0)
  companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$notgood != 1 , -3]
  dim(companies_names_dataframe)
  
  #applying the job agency filter
  staff_agencies <- read.csv("staff_agencies.csv" , sep = ";")
  blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
  blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
  #filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | (companies_names_dataframe$companyname == paste(blacklist_exact, collapse = '|')) )
  length(blacklist)
  filteredout <- cbind.data.frame(0,0)[-1,]
  colnames(filteredout) <- c("companyname" , "Freq")
  for(i in 1:length(blacklist)) {
    filteredout <- rbind(filteredout , filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i]) ) )
    companies_names_dataframe <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i] , negate = TRUE))
  }
  for(i in 1:length(blacklist_exact)) {
    filteredout <- rbind(filteredout, filter(companies_names_dataframe, blacklist_exact[i] == companies_names_dataframe$companyname) )
    companies_names_dataframe <- filter(companies_names_dataframe, blacklist_exact[i] != companies_names_dataframe$companyname)
  }
  filteredout <- arrange(filteredout , desc(Freq))
  dim(filteredout)
  dim(companies_names_dataframe)
  #the following commands would be equivalent to the previous loops but do not work with long strings as conditions
  #filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "" )
  #companies_names_dataframe <- mutate(companies_names_dataframe, companyname = replace(companyname, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "", NA))
  #companies_names_dataframe <- companies_names_dataframe[!is.na(companies_names_dataframe$companyname) , ]
  
  
  # generating a table of number of companies having x ads
  companies_freqtable <- as.data.frame(table(companies_names_dataframe$Freq))
  colnames(companies_freqtable) <- c("ads_per_company" , "n_companies")
  #ensuring that the variables of this table are numeric. NB: as.numeric does not work well for the variable ads_per_company, so I have to get the numeric value in a different way (through a merge)
  companies_names_dataframe$ads_per_company <- as.factor(companies_names_dataframe$Freq)
  companies_freqtable <- merge(companies_freqtable , companies_names_dataframe[duplicated(companies_names_dataframe$ads_per_company) == FALSE , -1])[ , -1]
  colnames(companies_freqtable) <- c("n_companies" , "ads_per_company")
  companies_freqtable$n_companies <- as.numeric(companies_freqtable$n_companies)
  str(companies_freqtable)
  #calculating the cumulative number of ads for the x biggest company names
  companies_freqtable <- arrange(companies_freqtable , desc(ads_per_company))
  companies_freqtable$tot_ads <- companies_freqtable$n_companies * companies_freqtable$ads_per_company
  companies_freqtable$cum_prop_ads <- 100 * cumsum(companies_freqtable$tot_ads) / sum(companies_freqtable$tot_ads)
  companies_freqtable$cum_prop_companies <- 100 * cumsum(companies_freqtable$n_companies) / sum(companies_freqtable$n_companies)
  companies_freqtable$cum_n_companies <- cumsum(companies_freqtable$n_companies)
  head(companies_freqtable)
  
  ### returning output
  #output <- list(companies_freqtable, companies_names_dataframe, filteredout)
  output <- companies_names_dataframe[1:100,]
  return(output)
}


#fctoutput <- check_company_names_fct("IT")


countrylist <- as.matrix(c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE"))
dim(countrylist)
company_names_allcountries <- apply(countrylist,1,check_company_names_fct)

mydata <- as.data.frame(as.matrix(c(1:100)))
for(i in 1:length(countrylist)) {
  print(i)
  print(dim(company_names_allcountries[[i]]))
  mydata <- cbind(mydata,company_names_allcountries[[i]])
}
colnames(mydata) <- c("rank","BE","Freq","Freq","BG","Freq","Freq","CZ","Freq","Freq","DK","Freq","Freq","DE","Freq","Freq","EE","Freq","Freq","IE","Freq","Freq","EL","Freq","Freq","ES","Freq","Freq","FR","Freq","Freq","HR","Freq","Freq","IT","Freq","Freq","CY","Freq","Freq","LV","Freq","Freq","LT","Freq","Freq","LU","Freq","Freq","HU","Freq","Freq","MT","Freq","Freq","NL","Freq","Freq","AT","Freq","Freq","PL","Freq","Freq","PT","Freq","Freq","RO","Freq","Freq","SI","Freq","Freq","SK","Freq","Freq","FI","Freq","Freq","SE","Freq","Freq")

mydata <- as.data.frame(mydata)
### print and view output

#print output
write.csv2(mydata , "top100companies_allcountries.csv")

help(write.csv)
View(mydata)





