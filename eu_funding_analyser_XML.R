# FP7 & H2020 parser

# libaries
library(tidyverse)
library(magrittr) # more pipes
library(lubridate) # date/time helpers
library(xml2) # xml parsing
library(beepr) # beep functions
library(parallel) # multicore processing

# settings
setwd('C:/Users/piete/Documents/git_repos/ERC_data')
# fp7_folder <- 'cordis-fp7projects-xml'
# h2020_folder <- 'cordis-h2020projects-xml'

# read reference data
# setwd('C:/Users/piete/Documents/git_repos/ERC_data/data/ref_data')
# # matchen van landen gaat goed op 2 cases na
# # Chech Republic (CZ) staat als Czechia in de EU tabel.
# # Macau staat als Macao in de EU tabel (mag op beide manieren worden gespeld)
# # corrigeren qs data, qs data als geheel inlezen en correcties in R doen, zo blijven de wijzigingen bij elkaar
# # even wachten op antwoord AdR en RvdB
# countries_eu <- read.csv2('cordisref-countries.csv', fileEncoding = 'UTF-8-BOM') %>% filter(language == 'en')


# functions
# process XML function
# ADD: error handling, the enrire process can fail on a single worker error
# import_xml <- function(handle, folder) {
#     parsed_data <- list()
#     xml_content <- read_xml(file.path('.', folder, handle)) %>% xml_ns_strip()
#     rcn <- xml_find_first(xml_content, '/project/rcn') %>% xml_text()
#     acro <- xml_find_first(xml_content, '/project/acronym') %>% xml_text()
#     title <- xml_find_first(xml_content, '/project/title') %>% xml_text()
#     total_cost <- xml_find_first(xml_content, '/project/totalCost') %>% xml_text()
#     start_date <- xml_find_first(xml_content, '/project/startDate') %>% xml_text()
#     end_date <- xml_find_first(xml_content, '/project/endDate') %>% xml_text()
#     call_id <- xml_find_first(xml_content, '/project/relations/associations/call/identifier') %>% xml_text()
#     associations  <- xml_find_all(xml_content, '/project/relations/associations/organization')
#     for (assoc in associations) {
#         part_rcn <- xml_find_first(assoc, 'rcn') %>% xml_text()
#         part_legal_name <- xml_find_first(assoc, 'legalName') %>% xml_text()
#         part_country <- xml_find_first(assoc, 'address/country') %>% xml_text()
#         part_contr = xml_attr(assoc, 'ecContribution')
#         part_contr = xml_attr(assoc, 'terminated')
#         if (xml_attr(assoc, 'type') == 'coordinator') {
#             part_is_coord <- 'Y'
#         } else {
#         part_is_coord <- 'N'
#         }
#         parsed_assoc <- list(handle, rcn, acro, title, call_id, total_cost, start_date, end_date,
#                                 part_rcn, part_legal_name, part_country, part_contr, part_is_coord)
#         parsed_data[[length(parsed_data)+1]] <- parsed_assoc
#         }
#     return(parsed_data)
# }

# file handles
# fp7_filehandles <- list.files(file.path('.', fp7_folder)) %>% as.list()
# # fp7_filehandles <- list.files(file.path('.', fp7_folder))[1:1000] %>% as.list()
# h2020_filehandles <- list.files(file.path('.', h2020_folder)) %>% as.list()
# # h2020_filehandles <- list.files(file.path('.', h2020_folder))[1:1000] %>% as.list()

# import singlecore for testing
# fp7_filehandles <- list.files(file.path('.', fp7_folder))[1:50] %>% as.list()
# h2020_filehandles <- list.files(file.path('.', h2020_folder))[1:50] %>% as.list()
# fp7_parsed <- lapply(fp7_filehandles, import_xml, folder = fp7_folder)
# h2020_parsed <- lapply(h2020_filehandles, import_XML, folder = h2020_folder)

# import multicore
# to fix: generalise
# clust <- makeCluster(7)
# clusterEvalQ(clust, {library(xml2); library(magrittr)})
# clusterExport(clust, varlist = c('fp7_filehandles', 'fp7_folder'))
# parsed_fp7 <- parLapply(clust, fp7_filehandles, import_xml, folder = fp7_folder)
# stopCluster(clust)

# clust <- makeCluster(7)
# clusterEvalQ(clust, {library(xml2); library(magrittr)})
# clusterExport(clust, varlist = c('h2020_filehandles', 'h2020_folder'))
# parsed_h2020 <- parLapply(clust, h2020_filehandles, import_xml, folder = h2020_folder)
# stopCluster(clust)

# create dataframes
# df_fp7 <- parsed_fp7 %>% flatten() %>% transpose()
# names(df_fp7) <- c('handle', 'rcn', 'acro', 'title', 'call_id', 'total_cost', 'start_date', 'end_date',
#                    'part_rcn', 'part_legal_name', 'part_country', 'part_contr', 'part_is_coord')
# df_fp7 <- df_fp7 %>% map(unlist) %>% as_tibble()
#
# df_h2020 <- parsed_h2020 %>% flatten() %>% transpose()
# names(df_h2020) <- c('handle', 'rcn', 'acro', 'title', 'call_id', 'total_cost', 'start_date', 'end_date',
#                    'part_rcn', 'part_legal_name', 'part_country', 'part_contr', 'part_is_coord')
# df_h2020 <- df_h2020 %>% map(unlist) %>% as_tibble()

# export files
# write_csv2(df_fp7, 'fp7.csv')
# write_csv2(df_h2020, 'h2020.csv')
# save_rds(df_fp7, 'fp7.rds')
# save_rds(df_h2020, 'h2020.rds')

# ANALYSIS
# ideas
# Analysis from a NL perspective (vs other counties) and from a EUR perspective (vs other NL universities)
# EU Open Data has additional lookup tables, country etc.
# Relative succes per country, number op projects (as coordinator?) divided by the number of universities which supply 80% of the projects
# Succes within  NL (relative succes maybe)
# Do coordinators also make the most money?
# Network analysis
# Maybe necessary to focus analysis on specific EU progerammes, facet on programmes for now
# Use a spatial map to visualise ther EUR network
# Use a spatial map to visualise the Dutch network

# read and finetune data
fp7 <- read_rds('fp7.rds')
h2020 <- read_rds('h2020.rds')
erc_all <- bind_rows('fp7' = fp7, 'h2020' = h2020, .id = 'source')
# write_csv2(erc_all, 'erc_all.csv')

erc_all$source %<>% as_factor()
erc_all$total_cost %<>% as.numeric()
erc_all$start_date %<>% as.Date()
erc_all$end_date %<>% as.Date()
erc_all$part_country %<>% as_factor()
erc_all$part_contr %<>% as.numeric() # there are quite a lot NAs and 0 in this variable, look into some projects
erc_all$part_is_coord %<>% as_factor()

# rcn eur: 1905738
erc_all %>% filter(str_detect(part_legal_name, 'ERASMUS')) %>% count(part_rcn, part_legal_name)

# some quick counts
erc_all %>% filter(part_rcn == 1905738) %>% tally()
erc_all %>% filter(part_rcn == 1905738) %>% select(rcn) %>% left_join(erc_all) %>% tally()

# some explorations
# do all projects have only 1 coordinator?
erc_all %>% filter(part_is_coord == 'Y') %>% count(rcn) %>% filter(n > 1) %>% arrange(desc(n))
# no, there are quite some projects with 2 coordinators, in a sample of these cases it turns out there are multiple
    # legal entities of the same real organisation involved (different part_rcn). Only one of these legal entities has part_cont > 0
    # the other legal entities have part_contr == 0
# a visual inspection of projects with part_contr == o and part_is_coord == Y is wise
check_no_part_contr_but_coord <- erc_all %>% filter(part_contr == 0 & part_is_coord == 'Y') %>% select(rcn) %>% distinct() %>%
    inner_join(erc_all)
# however not all legal entities which have part_contr == 0 are a duplicate coordinator
    # so what's up with the other part_contr == 0?

# there are also quite a lot of entries with part_contr NA, what is going on here?
    # could there be a subset of projects for which no participant receives any money? What are these projects?
erc_all %>% filter(is.na(part_contr)) %>% select(rcn) %>% distinct() %>% inner_join(erc_all) %>% count(part_contr)
    # no, there are also projects in which one partners has a NA part_contr and the other partners do have a part_contr
    # in these cases the partner is listed as Terminated, they probably have left the projects and have returned the part_contr
    # add the Terminated attribute to the dataset in the next iteration (DONE) and investigate further
