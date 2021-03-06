



### check if agency and company posts differ systematically in some ways

general_query <- query_athena("SELECT companyname, general_id, grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idsector, idcategory_sector FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
dim(general_query)
general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
general_query$companyname <- str_to_lower(general_query$companyname)
#companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
general_query$companyname <- str_trim(general_query$companyname)
general_query$companyname <- gsub(" ","_",general_query$companyname)
general_query$notgood <- ifelse(general_query$companyname=="",1,0)
general_query <- general_query[general_query$notgood != 1 , ]
View(general_query)


filteredout_m <- as.data.frame(table(filteredout$companyname))
filteredout_m$Freq <- 1
colnames(filteredout_m) <- c("companyname", "filteredout")
keep_m <- companies_names_dataframe[companies_names_dataframe$Freq>109 , ]
keep_m <- as.data.frame(table(keep_m$companyname))
keep_m$Freq <- 1
colnames(keep_m) <- c("companyname", "keep")


DF <- merge(general_query, filteredout_m, all.x=TRUE)
DF <- merge(DF, keep_m, all.x=TRUE)
dim(DF)


DF <- group_by(DF , companyname)
sumstats_by_company <- summarise(DF , tot_n=n(), nd_esco4=n_distinct(idesco_level_4), nd_esco3=n_distinct(idesco_level_3), tot_dups=sum(dup), nd_city=n_distinct(idcity), nd_prov=n_distinct(idprovince), nd_sect=n_distinct(idcategory_sector), nd_grab=n_distinct(grab_date),  filteredout=median(filteredout), keep=median(keep))
sumstats_by_company <- arrange(sumstats_by_company , desc(tot_n))
sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
sumstats_by_company$r_dup <- 100*sumstats_by_company$tot_dups / sumstats_by_company$tot_n
sumstats_by_company$r_esco4 <- 100*sumstats_by_company$nd_esco4 / sumstats_by_company$tot_n
sumstats_by_company$r_grab <- 100*sumstats_by_company$nd_grab / sumstats_by_company$tot_n
sumstats_by_company$r_sect <- 100*sumstats_by_company$nd_sect / sumstats_by_company$tot_n


### summary stats by variable "filteredout", which is equal to 1 if the observation has been labelled as agency, 0 for company, NA for not labelled yet

sumstats_by_company <- sumstats_by_company[sumstats_by_company$tot_n>29 , ]
DF2 <- group_by(sumstats_by_company , filteredout)
summarise(DF2, tot_n=n_distinct(companyname), sd_esco4=sd(r_esco4), r_esco4=mean(r_esco4), sd_m_esco4=sd(nd_esco4) , m_esco4=mean(nd_esco4), sd_city=sd(r_city), r_city=mean(r_city), sd_dup=sd(r_dup), r_dup=mean(r_dup))

###cross filteredout and other indicators through filters
prova <- sumstats_by_company[sumstats_by_company$r_esco4>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_esco4<1 , ]
table(prova$filteredout)

prova <- sumstats_by_company[sumstats_by_company$r_city>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_city<1 , ]
table(prova$filteredout)


### cross through charts (after recoding filteredout=-1 for observations that have not yet been labelled as company or agency)

sumstats_by_company <- mutate(sumstats_by_company, filteredout = replace(filteredout, is.na(filteredout), -1))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_esco4, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_city, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_sect, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_sect, y = filteredout)) +
  xlim(0,5)

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_grab, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))



### given the discriminative power of r_sect, some more cross-tabulation can help
dim(sumstats_by_company)
prova <- sumstats_by_company[sumstats_by_company$r_sect>3 & sumstats_by_company$r_sect<100 & sumstats_by_company$tot_n>99 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_sect<1 , ]
table(prova$filteredout)

filteredout_sect <- as.data.frame(sumstats_by_company$companyname[sumstats_by_company$r_sect>3])
colnames(filteredout_sect) <- "companyname"
dim(filteredout_sect)
filteredout_sect$filteredout_sect <- 1

companies_names_dataframe <- merge(companies_names_dataframe, filteredout_sect, all.x=TRUE)
companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$filteredout_sect!=1 , ]

sumstats_by_company$n_sect <- sumstats_by_cfilteredoutompany$r_sect * sumstats_by_company$tot_n / 100

(t(sumstats_by_company$tot_n)%*%sumstats_by_company$tot_n)^-1%*%t(sumstats_by_company$tot_n)%*%sumstats_by_company$n_sect



sumstats_by_company$ln_esco4 <- log(sumstats_by_company$nd_esco4)
sumstats_by_company$ln_esco3 <- log(sumstats_by_company$nd_esco3)
sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)
sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
sumstats_by_company$filteredout <- as.factor(sumstats_by_company$filteredout)
tab_esco4 <- table(general_query$idesco_level_4)

plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_esco3, colour=filteredout))

str(sumstats_by_company$filteredout)





