##################################################
# eu fp7 & h2020 parser and analyser
# pieter vreeburg, vreeburg@ese.eur.nl
##################################################

##################################################
# libraries
##################################################
library(tidyverse)
library(magrittr)
library(splitstackshape)

##################################################
# settings
##################################################
setwd('C:/Users/piete/Documents/git_repos/eu_data')
data_dir <- file.path('.', 'cordis_aggr_data')
ref_dir <- file.path('.', 'ref_data')

##################################################
# read source data
##################################################
h2020_proj <- read_csv2(file.path(data_dir, 'cordis-h2020projects.csv')) %>%
    select(framework = frameworkProgramme, proj_rcn = rcn, proj_id = id, acronym, status, programme, topics, title,
           start_date = startDate, end_date = endDate, total_cost = totalCost,
           ec_max_contr = ecMaxContribution, call, funding_scheme = fundingScheme)

h2020_parts <- read_csv2(file.path(data_dir, 'cordis-h2020organizations.csv'), na = '') %>%
    select(proj_rcn = projectRcn, part_id = id, role, name, short_name = shortName,
           activity_type = activityType, end_of_part = endOfParticipation, ec_contr = ecContribution,
           country, city)

##################################################
# Harmonize WUR
##################################################
# WUR is present with 2 entitites, this influences counts and must be harmonized

h2020_wur <- h2020_parts %>% filter(part_id == 999547365 | part_id == 999981634) %>%
                group_by(proj_rcn) %>%
                summarise(part_id = 999981634,
                          role = if_else(any(role == 'coordinator'), 'coordinator', 'participant'),
                          ec_contr = sum(ec_contr))

# THIS IS NOT YET COMPLETELY CORRECT
# IF A PROJECT HAS ONLY WAGENINGEN RESEARCH AS A PARTICIPANT THIS PARTICIPATION GETS DELETED

h2020_parts %>% filter(part_id != 999547365) %>%
    left_join(h2020_wur, by = c('proj_rcn', 'part_id')) %>%
    mutate(role = if_else(is.na(role.y), role.x, role.y),
           ec_contr = if_else(is.na(ec_contr.y), ec_contr.x, ec_contr.y)) %>%
    select(-role.x, -role.y, -ec_contr.x, -ec_contr.y) %>%
    select(proj_rcn, part_id, role, name, short_name, activity_type, end_of_part, ec_contr, country, city)

h2020_compl <- inner_join(h2020_proj, h2020_parts) %>% arrange(proj_rcn, role)
# write_csv2(h2020_compl, 'h2020_compl.csv')

fp7_proj <- read_csv2(file.path(data_dir, 'cordis-fp7projects.csv')) %>%
    select(framework = frameworkProgramme, proj_rcn = rcn, proj_id = id, acronym, status, programme, topics, title,
           start_date = startDate, end_date = endDate, total_cost = totalCost,
           ec_max_contr = ecMaxContribution, call, funding_scheme = fundingScheme)

fp7_parts <- read_csv2(file.path(data_dir, 'cordis-fp7organizations.csv'), na = '') %>%
    select(proj_rcn = projectRcn, part_id = id, role, name, short_name = shortName,
           activity_type = activityType, end_of_part = endOfParticipation, ec_contr = ecContribution,
           country, city)

# ADD WUR HARMONIZATION FOR FP7

fp7_compl <- inner_join(fp7_proj, fp7_parts) %>% arrange(proj_rcn, role)
# write_csv2(fp7_compl, 'fp7_compl.csv')

eu_compl <- bind_rows(fp7_compl, h2020_compl)

##################################################
# read reference data
##################################################

# The reference data works for FP7 (every projects has one programme)
# H2020 programmes can have more (; separated programmes), use string matching to allocate EU playing field
# check both reference tables with Adhemare & Roel

ref_countries <- read_csv2(file.path(ref_dir, 'cordisref-countries.csv'), na = character()) %>%
    filter(language == 'en') %>% select (eu_code = euCode, iso_ode = isoCode, everything()) %>%
    arrange(name)
ref_h2020_progs <- read_csv2(file.path(ref_dir, 'ref_h2020_progs.csv')) # H2020 projects have multiple programmes attached
ref_act_type <- read_csv2(file.path(ref_dir, 'cordisref-organizationActivityType.csv')) %>%
    filter(language == 'en') %>% select(code = Code, title = Title, everything()) %>%
    arrange(code)
ref_scheme <- read_csv2(file.path(ref_dir, 'cordisref-projectFundingSchemeCategory.csv')) %>%
    filter(language == 'en') %>% arrange(code)

##################################################
# do all projects have only 1 coordinator?
##################################################
(two_coord <- eu_compl %>% filter(role == 'coordinator') %>% count(proj_rcn) %>% filter(n > 1) %>% arrange(desc(n)))
# no, there are quite some projects with 2 coordinators
# two_coord %>% select(proj_rcn) %>% inner_join(eu_compl) %>% write_csv2('two_coord.csv')
# in the FP7 data there are some doubles, it seems this happpens because for the coordinator a PI and related contact are registered
    # I don't use the personal info in my analysis so a distinct() will resolve this
fp7_parts %>% distinct() %>% filter(role == 'coordinator') %>% count(proj_rcn) %>% filter(n > 1)
# the H2020 data has the same format, but does not have the personal info
    # it seems that in this case the two 2 coordinator problem is caused by the inclusion of inactive coordinators
    # the FP7 data has very little inactive coordinators included
h2020_parts %>% filter(role == 'coordinator' & end_of_part == TRUE) %>% count(proj_rcn) %>% filter(n > 1)

# new eu_compl
fp7_compl_2 <- inner_join(fp7_proj, fp7_parts %>% distinct() %>% filter(end_of_part == FALSE)) %>% arrange(proj_rcn, role)
h2020_compl_2 <- inner_join(h2020_proj, h2020_parts %>% filter(end_of_part == FALSE))
eu_compl_2 <- bind_rows(fp7_compl_2, h2020_compl_2)

# Again: do all projects have only 1 coordinator?
eu_compl_2 %>% filter(role == 'coordinator') %>% count(proj_rcn) %>% filter(n > 1) %>% arrange(desc(n))
# yes

##################################################
# missing value analysis
##################################################
na_per_var <- eu_compl_2 %>% transmute_all(is.na) %>% map_dbl(sum)

# a lot of NAs are in the ec_contr variable
# case-by-case inspection reveals a lot of the partner roles aren't getting any money
# what are partner roles?
eu_compl_2 %>% filter(is.na(ec_contr)) %>% count(framework, role)
# partner roles are only applicable to h2020
# do all partners get ec_contr NA?
eu_compl_2 %>% filter(role == 'partner') %>% count(is.na(ec_contr))
# yes, all partner roles get no money
# There are valid reasons why coordinators, participants and partners get no money
    # for example in International Training Networks and Network of Excellence projects
    # these actors have other reasons to participate (eg offer traineeships to PhDs)
    # Missing data for ec_contr is valid and these cases should not be dropped if not necessary
    # consider replacing the mising values for this variable with zero
# The other missing values in the dataset could be really missing and should be monitored for their
    # impact in any analysis
    # export all rows with a missing value somewhere (except ec_contr) for rowwise inspection
na_somewhere <- eu_compl_2 %>% select(-ec_contr) %>% is.na() %>% rowSums %>% as.logical() %>% filter(eu_compl_2, .)
# technical note: the pipe places the passed data on the lhs of the function, the dot allows placement on the Rrhs
# write_csv2(na_somewhere, 'na_somewhere.csv')
# no start_data and no end_date often go together and occur mostly in the FP7 dataset: real missings
    # but not very impactful as I don't see this variable being used in the analysis for now
# not total costs occurs in both datasets, could be a real missing
    # but not very impactful as I dont'see this variable being used in the analysis for now
# no part_id only occurs in the FP7 dataset,
    # this can very impactful when counting partners based on part_id

eu_compl_2 %>% filter(is.na(part_id)) %>% select(proj_rcn) %>% distinct() %>% tally()
eu_compl_2 %>% filter(is.na(part_id)) %>% select(programme) %>% distinct()

# but this only happens for 90 projects within 5 programmes
    # drop these records as a precaution
eu_compl_3 <- eu_compl_2 %>% filter(!is.na(part_id))
# no short_name occurs in both files
    # not very impactful, will only be a problem when labelling the network
# no country occurs for Windhoek (Namibia with code NA which gets read as a literal NA
    # this can be solved by not adding the argument na = character() to the read_csv call
    # but this causes empty strings not to be parsed as NA
    # fixed in the import
# no country also occurs for Skopje and Bitala, Macedonia is not a former member of the EU, but receives money from projects
    # repair by hand
eu_compl_3 %<>% mutate(city = city %>% str_to_lower())
eu_compl_3 %<>% mutate(country = case_when(
    city == 'skopje' | city == 'bitola' ~ 'MK',
    TRUE ~ country,
    is.na(city) ~ country))
eu_compl_3 %>% transmute_all(is.na) %>% map_dbl(sum)
# no cities are real missings
    # not very impactful as I don't plan on using cities in my analysis

##################################################
# harmonize potential factors and convert
##################################################
eu_compl_3 %>% count(framework)
eu_compl_3 %>% count(framework, status)
eu_compl_3 %>% count(framework, role)
eu_compl_3 %>% count(activity_type)
# status needs cleaning up
eu_compl_3 %<>% mutate(status = recode(status,
    ONG = 'SIGNED',
    CLO = 'CLOSED',
    CAN = 'TERMINATED'))
# add factors
eu_compl_3 %<>% mutate_at(vars(framework, status, role, activity_type, country),
                         factor)
# tech note: mutate add variables to the current table, when not assigned back to a symbiol the function prints
# tech note: transmute always creates a new table

##################################################
# create additional variables
##################################################
# Collect Dutch universities for easy selection and all var to dataset
# Note: medical centres are separate entities in this dataset
ref_dutch_uni <- tribble(
~acro, ~name, ~part_id,
'EUR', 'Erasmus University Rotterdam', 999839335,
'TUe', 'Eindhoven University of Technology', 999977269,
'UM', 'Maastricht University', 999975911,
'UT', 'University of Twente', 999900833,
'RUN', 'Radboud University', 999992110,
'RUG', 'University of Groningen', 999989782,
'TU', 'Tilburg University', 999899475,
'TUD', 'Delft University of Technology', 999977366,
'UL', 'Leiden University', 999974553,
'VU', 'VU Amsterdam', 954530344,
'WUR', 'Wageningen University', 999981634,
'WUR', 'Wageningen Research', 999547365,
'UU', 'Utrecht University', 999985805,
'UvA', 'University of Amsterdam', 999985708
) %>% arrange(acro)

eu_compl_3 %<>% left_join(ref_dutch_uni %>% select(acro, part_id)) %>% rename(d_uni_acro = acro)
eu_compl_3 %<>% mutate(d_uni = if_else(!is.na(d_uni_acro), 'Y', 'N') %>% factor())



# filter(d_uni_acro == 'WUR')
# group_by(proj_id)

##################################################
# Add EUR playing field for fair comparisons
##################################################

# FP7
    # Cooperation - Health (FP7-HEALTH)
    # Cooperation - Transport (FP7-TRANSPORT)
    # Cooperation - Socio-economic Sciences and Humanities (FP7-SSH)
    # Cooperation - Security (FP7-SECURITY)
    # Ideas - European Research Council (FP7-IDEAS-ERC)
    # People (FP7-PEOPLE)
    # Capacities - Science in Society (FP7-SIS)
    # Capacities - Support to the Coherent Development of Research Policies (FP7-COH)

# H2020
    # Excellent science
    # SOCIETAL CHALLENGES - Health, demographic change and well-being
    # SOCIETAL CHALLENGES - Smart, Green And Integrated Transport
    # SOCIETAL CHALLENGES - Europe In A Changing World - Inclusive, Innovative And Reflective Societies
    # Secure societies - Protecting freedom and security of Europe and its citizens
    # SCIENCE WITH AND FOR SOCIETY

# [ADD CODE HERE]

##################################################
# clean up
##################################################
# objects() %>% str_subset('^fp7|^h2020|na_somewhere|two_coord') %>% rm(list = ., envir = .GlobalEnv)
# # tech note: .GlobalEnv is necessary due to the pipe used, otherwise rm searches in the piped environment
eu_compl <- eu_compl_3
rm(list = c('eu_compl_2',
            'eu_compl_3',
            'fp7_compl',
            'fp7_compl_2',
            'h2020_compl',
            'h2020_compl_2',
            'na_somewhere',
            'two_coord'))
na_per_var <- eu_compl %>% transmute_all(is.na) %>% map_dbl(sum)
gc()
# write eu_compl for sharing etc.
write_csv2(eu_compl, 'eu_compl.csv')


##################################################
# exploratory analysis
##################################################
# EXPLORATORY ANALYSIS
# ideas
    # Analysis from a NL perspective (vs other counties) and from a EUR perspective (vs other NL universities)
    # Relaive succes per country, number op projects (as coordinator?) divided by the number of universities which supply 80 perc  of the projects
    # Succes within  NL (relative succes maybe)
    # Do coordinators also make the most money?
    # Network analysis -> Bas Karreman
    # Maybe necessary to focus analysis on specific EU progerammes, facet on programmes for now
    # Use a spatial map to visualise ther EUR network
    # Use a spatial map to visualise the Dutch network
    # hoe vaak is EUR coordinator vs de andere universiteiten?
    # Hoe vaak heeft de EUR haar deelname beeindigd
    # Gemiddelde vs EUR contributie
    # Relatieve groei FP7 vd H2020 per universiteit, EUR x% meer tov van FP7 in H2020 [GOED IDEE]
    # Hoe vaak zijn de NL universiteiten PI?
    # Hoe doen de MCs het tov elkaar

# coordinators, projecten met 1 deelnemer eruit
eu_compl %>% filter(d_uni == 'Y' & role == 'coordinator') %>% count(framework, d_uni_acro)

# total participations for dutch universities, facet by framework
ggplot(data = eu_compl %>% filter(d_uni == 'Y')) +
    geom_bar(mapping = aes(x = d_uni_acro)) +
    facet_wrap(facets = vars(framework)) +
    labs(title = 'Count partcipation by Dutch universities ')

# total amount earned for dutch universities, facet by framework
ggplot(data = eu_compl %>% filter(d_uni == 'Y')) +
    geom_col(mapping = aes(x = d_uni_acro, y = ec_contr)) +
    facet_wrap(facets = vars(framework)) +
    labs(title = 'Total amount earned by Dutch universities')


# due to the complex structure of fp7 and h2020 it is not easy to select a subset of programmes to report on
# maybe work the other way around: get all the projects in which EUR and Tilburg are represented and use these as
# a proxy for SSH-like projects
# discuss this with Adhemare and Roel

# set EUR and TU projects as SSH proxy
ssh_proxy <- eu_compl %>% filter(d_uni_acro == 'EUR' | d_uni_acro == 'TU') %>%
    select(proj_rcn) %>% distinct()

# total participation for dutch universities in SSH proxy, facet by framework
ggplot(data = eu_compl %>% inner_join(ssh_proxy) %>% drop_na(d_uni_acro)) +
    geom_bar(mapping = aes(x = d_uni_acro)) +
    facet_wrap(facets = vars(framework)) +
    labs(title = 'Count participation SSH by Dutch universities', subtitle = 'EUR, TU projects as SSH')


# total amount earned for dutch universities in SSH proxy, facet by framework
ggplot(data = eu_compl %>% inner_join(ssh_proxy) %>% drop_na(d_uni_acro)) +
    geom_col(mapping = aes(x = d_uni_acro, y = ec_contr)) +
    facet_wrap(facets = vars(framework)) +
    labs(title = 'Total amount earned SSH by Dutch universities', subtitle = 'EUR, TU projects as SSH')


