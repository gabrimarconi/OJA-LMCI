

source("check_company_names.R")

str(companies_names_dataframe)

companies_to_clean <- companies_names_dataframe[companies_names_dataframe$Freq>99 , -3]

write.csv(companies_to_clean[ , 1] , "companies_to_clean_export.csv")

# reading the keywords for data cleaning from imported files
# NB if it fails to refresh the file, use a slightly different file name
clean_names <- read.csv("companies_to_clean_import.csv" , sep = ";")
#  clean_names <- read.csv("companies_to_clean.csv" , sep = ";")
head(clean_names)

#backing up the data file
companies_names_dataframe_2 <- companies_names_dataframe
# companies_names_dataframe <- companies_names_dataframe_2


names_replaced <- function(i) {
  temp <- companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) ==TRUE]
  temp <- paste(temp, collapse=" ; ")
  return(temp)
}

names_replaced_list <- apply(as.matrix(1:dim(clean_names)[1]),1,names_replaced)
write.csv2(names_replaced_list,"names_replaced_list.csv")

dim(clean_names)[1]

for(i in 1:dim(clean_names)[1]) {
  #cleaning the company name
  companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) == TRUE & companies_names_dataframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
  companies_names_dataframe$companyname[companies_names_dataframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
}

companies_names_dataframe <- group_by(companies_names_dataframe,companyname)
companies_names_dataframe <- summarise(companies_names_dataframe, Freq=sum(Freq))
companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))

View(companies_names_dataframe_2)
View(companies_names_dataframe)


companies_freqtable_clean <- as.data.frame(table(companies_names_dataframe$Freq))
colnames(companies_freqtable_clean) <- c("ads_per_company" , "n_companies")
str(companies_freqtable_clean)
head(companies_freqtable_clean)

# generating a table of number of companies having x ads
companies_freqtable_clean <- as.data.frame(table(companies_names_dataframe$Freq))
colnames(companies_freqtable_clean) <- c("ads_per_company" , "n_companies")
companies_freqtable_clean <- arrange(companies_freqtable_clean,desc(ads_per_company))

View(companies_freqtable_clean)
#ensuring that the variables of this table are numeric. NB: as.numeric does not work well for the variable ads_per_company, so I have to get the numeric value in a different way (through a merge)
companies_names_dataframe$ads_per_company <- as.factor(companies_names_dataframe$Freq)
companies_freqtable_clean <- merge(companies_freqtable_clean , companies_names_dataframe[duplicated(companies_names_dataframe$ads_per_company) == FALSE , -1])[ , -1]
colnames(companies_freqtable_clean) <- c("n_companies" , "ads_per_company")
companies_freqtable_clean$n_companies <- as.numeric(companies_freqtable_clean$n_companies)
head(companies_freqtable_clean)
#calculating the cumulative number of ads for the x biggest company names
companies_freqtable <- arrange(companies_freqtable , desc(ads_per_company))
companies_freqtable$tot_ads <- companies_freqtable$n_companies * companies_freqtable$ads_per_company
companies_freqtable$cum_prop_ads <- 100 * cumsum(companies_freqtable$tot_ads) / sum(companies_freqtable$tot_ads)
companies_freqtable$cum_prop_companies <- 100 * cumsum(companies_freqtable$n_companies) / sum(companies_freqtable$n_companies)
companies_freqtable$cum_n_companies <- cumsum(companies_freqtable$n_companies)
head(companies_freqtable)








































#applying the job agency filter
staff_agencies <- read.csv("staff_agencies_IT.csv" , sep = ";")
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



