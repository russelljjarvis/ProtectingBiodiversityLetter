# SUPER MESSY SCRIPT FOR DOWNLOADING, PARSING AND CLEANING NAMES

# FUNCTIONS
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
simpleCap2 <- function(x) {
  s <- strsplit(x, "/")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse="/")
}

#LIBS
library(RJSONIO)

# PROCESS
# get data
url <- 'http://www.protect-biodiversity.com/signatures?$sort[createdAt]=-1'
json_obj <- fromJSON(content=url)

# cnvrt to data.frame
tbl <- data.frame('LastName'=rep(NA, length(json_obj)),
                  'FirstName'=rep(NA, length(json_obj)),
                  'Country'=rep(NA, length(json_obj)),
                  'Affiliation'=rep(NA, length(json_obj)),
                  'Scientist'=rep(NA, length(json_obj)),
                  'Signed'=rep(NA, length(json_obj)))
for(i in seq_along(json_obj)) {
  tbl[['FirstName']][i] <- json_obj[[i]][['firstName']]
  tbl[['LastName']][i] <- json_obj[[i]][['lastName']]
  tbl[['Affiliation']][i] <- json_obj[[i]][['affiliation']]
  tbl[['Country']][i] <- tolower(json_obj[[i]][['country']])
  tbl[['Scientist']][i] <- json_obj[[i]][['isScientist']]
  tbl[['Signed']][i] <- json_obj[[i]][['createdAt']]
}

# REMOVE EVERYTHING AFTER 1600 (CET WINTER)
tbl[1:10, ]
# drop names before this one
mxi <- which(tbl$LastName == 'Mårtensson')
tbl <- tbl[-1*1:mxi, ]

# CONVERT SCIENTIST
tbl[['Scientist']] <- ifelse(tbl[['Scientist']], 'Yes', 'No')

# PARSE COUNTRY
tbl[['Country']] <- gsub('suisse', 'switzerland', tbl[['Country']])
tbl[['Country']] <- gsub('swiss', 'switzerland', tbl[['Country']])
tbl[['Country']] <- gsub('stockholm', 'sweden', tbl[['Country']])
tbl[['Country']] <- gsub('switzerland ', 'switzerland', tbl[['Country']])
tbl[['Country']] <- gsub('scotland', 'uk', tbl[['Country']])
tbl[['Country']] <- gsub('switzerland ', 'switzerland', tbl[['Country']])
tbl[['Country']] <- gsub('.*@.*', '-', tbl[['Country']])
tbl[['Country']] <- gsub('deutschland', 'germany', tbl[['Country']])
tbl[['Country']] <- gsub('england', 'uk', tbl[['Country']])
tbl[['Country']] <- gsub('united kingdom', 'uk', tbl[['Country']])
tbl[['Country']] <- gsub('united states', 'usa', tbl[['Country']])
tbl[['Country']] <- gsub('uk ', 'uk', tbl[['Country']])
tbl[['Country']] <- gsub('usa ', 'usa', tbl[['Country']])
tbl[['Country']] <- gsub('ukraine ', 'ukraine', tbl[['Country']])
tbl[['Country']] <- gsub('netherlands  ', 'netherlands', tbl[['Country']])
tbl[['Country']] <- gsub('schweiz', '-', tbl[['Country']])
tbl[['Country']] <- gsub('germany ', 'germany', tbl[['Country']])
tbl[['Country']] <- gsub('malaysian ', 'malaysia', tbl[['Country']])
tbl[['Country']] <- gsub('netherlands ', 'netherlands', tbl[['Country']])
tbl[['Country']] <- gsub('colombia ', 'colombia', tbl[['Country']])
tbl[['Country']] <- gsub('bazil', 'brazil', tbl[['Country']])
tbl[['Country']] <- gsub('australia ', 'australia', tbl[['Country']])
tbl[['Country']] <- gsub('méxico', 'mexico', tbl[['Country']])
tbl[['Country']] <- gsub('usaof america', 'usa', tbl[['Country']])
tbl[['Country']] <- gsub('the netherlands', 'netherlands', tbl[['Country']])
tbl[['Country']] <- gsub('^us$', 'usa', tbl[['Country']])
# re-capitilise
tbl[['Country']] <- sapply(tbl[['Country']], simpleCap)
tbl[['Country']] <- sapply(tbl[['Country']], simpleCap2)
tbl[['Country']] <- gsub('^Usa$', 'USA', tbl[['Country']])
tbl[['Country']] <- gsub('^Uk$', 'UK', tbl[['Country']])
tbl[['Country']] <- gsub('Perú', 'Peru', tbl[['Country']])
tbl[['Country']] <- gsub('^German$', 'Germany', tbl[['Country']])
tbl[['Country']] <- gsub('.*Hong Kong.*', 'Hong Kong/China', tbl[['Country']])
tbl[['Country']] <- gsub('Uk', 'UK', tbl[['Country']])
tbl[['Country']] <- gsub('Usa', 'USA', tbl[['Country']])
all_cntrs <- tbl[['Country']]
all_cntrs <- unlist(strsplit(all_cntrs, split='/'))
sort(table(all_cntrs), decreasing=TRUE)
length(unique(all_cntrs))

# Check for name dups
name_check <- data.frame('LastName'=tbl[['LastName']], 'similar'=NA,
                         stringsAsFactors=FALSE)
for(i in 1:(nrow(name_check))) {
  name_check[i, 'similar'] <- name_check[i, 'LastName'] %in%
    name_check[(i+1):(i+10), 'LastName']
}
pull <- which(name_check[['similar']])
i <- 1
tbl[pull[i]:(pull[i]+10),]

name_check <- data.frame('FirstName'=tbl[['FirstName']], 'similar'=NA,
                         stringsAsFactors=FALSE)
for(i in 1:(nrow(name_check))) {
  name_check[i, 'similar'] <- name_check[i, 'FirstName'] %in%
    name_check[(i+1):(i+10), 'FirstName']
}
pull <- which(name_check[['similar']])
i <- 1
tbl[pull[i]:(pull[i]+10),]

# Manually correct
tbl[grepl('NAGANO', tbl[['LastName']]), 'LastName'] <- 'Nagano'
tbl[grepl('GUINAND', tbl[['LastName']]), 'LastName'] <- 'Guinand'
tbl[grepl('HADDOCK', tbl[['LastName']]), 'LastName'] <- 'Haddock'
tbl[grepl('RIVEROS', tbl[['LastName']]), 'LastName'] <- 'Riveros'
tbl[grepl('PAIVA', tbl[['LastName']]), 'LastName'] <- 'Paiva'
tbl[grepl('JACQUELINE', tbl[['LastName']]), 'LastName'] <- 'Jacqueline'
pull <- grepl('^[[:upper:]]+$', perl=TRUE, tbl[['LastName']])
tbl[pull, ]
tbl[grepl('LIMA', tbl[['FirstName']]), 'FirstName'] <- 'Lima'
tbl[grepl('NAOMI', tbl[['FirstName']]), 'FirstName'] <- 'Naomi'
tbl[grepl('^JC$', tbl[['FirstName']]), 'FirstName'] <- 'J.C.'
tbl[grepl('^CJ$', tbl[['FirstName']]), 'FirstName'] <- 'C.J.'
tbl[grepl('^AJ$', tbl[['FirstName']]), 'FirstName'] <- 'A.J.'
pull <- grepl('^[[:upper:]]+$', perl=TRUE, tbl[['FirstName']])
tbl[pull, ]
# gregersen and nielsen are real
tbl[grepl('NA', tbl[['FirstName']]), 'FirstName'] <- '-'
# lowercase
tbl[grepl('^cordeiro$', tbl[['LastName']]), 'LastName'] <- 'Cordeiro'
tbl[grepl('^dauwe$', tbl[['LastName']]), 'LastName'] <- 'Dauwe'
tbl[grepl('^domingues$', tbl[['LastName']]), 'LastName'] <- 'Domingues'
tbl[grepl('^grenier$', tbl[['LastName']]), 'LastName'] <- 'Grenier'
tbl[grepl('^iversen$', tbl[['LastName']]), 'LastName'] <- 'Iversen'
tbl[grepl('^kearns$', tbl[['LastName']]), 'LastName'] <- 'Kearns'
tbl[grepl('^mroz$', tbl[['LastName']]), 'LastName'] <- 'Mroz'
tbl[grepl('^rodrigues$', tbl[['LastName']]), 'LastName'] <- 'Rodrigues'
tbl[grepl('^silva$', tbl[['LastName']]), 'LastName'] <- 'Silva'
tbl[grepl('^souza$', tbl[['LastName']]), 'LastName'] <- 'Souza'
tbl[grepl('^santos$', tbl[['LastName']]), 'LastName'] <- 'Santos'
pull <- grepl('^[[:lower:]]+$', perl=TRUE, tbl[['LastName']])
tbl[pull, ]
tbl[grepl('^steven$', tbl[['FirstName']]), 'FirstName'] <- 'Steven'
tbl[grepl('^martin$', tbl[['FirstName']]), 'FirstName'] <- 'Martin'
tbl[grepl('^florent$', tbl[['FirstName']]), 'FirstName'] <- 'Florent'
tbl[grepl('^lars$', tbl[['FirstName']]), 'FirstName'] <- 'Lars'
tbl[grepl('^lukass$', tbl[['FirstName']]), 'FirstName'] <- 'Lukass'
pull <- grepl('^[[:lower:]]+$', perl=TRUE, tbl[['FirstName']])
tbl[pull, ]
tbl[grepl('^marta justino$', tbl[['FirstName']]), 'FirstName'] <- 'Marta Justino'
tbl[grepl('^juscelino germano$', tbl[['FirstName']]), 'FirstName'] <- 'Juscelino Germano'
tbl[grepl('^patricia da silva dos santos$', tbl[['FirstName']]), 'FirstName'] <- 'Patricia da Silva dos Santos'
tbl[grepl('^jucelia rocha$', tbl[['FirstName']]), 'FirstName'] <- 'Jucelia Rocha'
pull <- grepl('^([[:lower:]]|\\s)+$', perl=TRUE, tbl[['FirstName']])
tbl[pull, ]

# Name switch
tbl[tbl[['LastName']] == 'Susumu', ]
tbl[tbl[['LastName']] == 'Susumu', c('FirstName', 'LastName')] <-
  c('Susumu', 'Tomiya')

# Check names
nmlngths <- sapply(tbl[['FirstName']], nchar)
hist(nmlngths)
tbl[nmlngths > 25,]

# get all countries
all_cntrs <- as.data.frame(sort(table(all_cntrs), decreasing=TRUE))
colnames(all_cntrs) <- c('Country', 'N')

# get all affiliations
all_affls <- as.data.frame(sort(table(tbl[['Affiliation']]),
                                decreasing=TRUE))
colnames(all_affls) <- c('Affiliation', 'N')

# Replace '' with '-'
tbl[['Affiliation']][tbl[['Affiliation']] == ''] <- '-'
tbl[['Country']][tbl[['Country']] == ''] <- '-'

# plot countries
library(ggplot2)
p_data <- unlist(strsplit(tbl[['Country']], '/'))
pull <- p_data %in% all_cntrs[1:20, 'Country']
lvls <- all_cntrs[1:20, 'Country']
lvls <- lvls[length(lvls):1]
p_data <- data.frame('Country'=p_data[pull],
                     stringsAsFactors=FALSE)
p_data$Country <- factor(p_data$Country, levels=lvls)
p <- ggplot(p_data, aes(Country)) + geom_bar() + coord_flip() +
  ylab('') + theme_bw()
png('countries_histogram.png')
print(p)
dev.off()



# WRITE OUT
nrow(tbl)
nrow(all_cntrs)
sum(tbl[['Scientist']] == 'Yes')*100/nrow(tbl)
sum(tbl[['Scientist']] == 'Yes')
tbl <- tbl[order(tbl[['Affiliation']]),]
tbl <- tbl[order(tbl[['Country']]),]
tbl <- tbl[order(tbl[['LastName']]),]
write.table(tbl, file='petition_authors.tsv', sep='\t',
            row.names=FALSE)
write.table(all_cntrs, file='petition_authors_country.tsv', sep='\t',
            row.names=FALSE)