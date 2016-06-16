# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Title: R code for the research project "Differences in reported sepsis
# incidence according to study design: a literature review"
# Author: Andreas Halgreen Eiset
# System: x86_64-pc-linux-gnu; Linux 4.2.0-35-generic; R version: 3.3.0
# Packages: dplyr_0.4.3; tidyr_0.4.1; ggplot2_2.1.0
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# All code and data are available from github under CC-BY-4.0 license
# From http://choosealicense.com/licenses/cc-by-4.0/:
# "Permits almost any use subject to providing credit and license notice."

# Packages used
lapply(c("dplyr", "tidyr", "ggplot2"), library, character.only = TRUE)

# Read in the data base file (.csv)
dataurl1 <- file.path("https://raw.githubusercontent.com/eiset/SepsisIncidence/master/db_articles.csv")
dta <- read.csv(dataurl1, strip.white = TRUE, na.strings = c("NA", ""))

# Countries as grouped by The World Bank
dataurl2 <- file.path("https://raw.githubusercontent.com/eiset/SepsisIncidence/master/country_class.csv")
countries <- read.csv2(dataurl2) %>%
        select(- c(1, 2, 5, 8:11)) %>%
        slice(-1) %>%
        sapply(., gsub, pattern = ",.*", replacement = "") %>%
        data.frame() %>%
        mutate(Economy = gsub("United States", "USA", .$Economy))
countries$Region <- as.character(levels(countries$Region))[countries$Region]

# Initial data management --------------------------------------------------------------------

str(dta)
# Get the variables in right format
dta$author <- as.character(levels(dta$author))[dta$author]
dta$title <- as.character(levels(dta$title))[dta$title]
dta$country <- as.character(levels(dta$country))[dta$country]
dta$data.source <- as.character(levels(dta$data.source))[dta$data.source]
dta$note <- as.character(levels(dta$note))[dta$note]

dta <- mutate(dta, full.read = inc.full.read == 0 | inc.full.read == 1) %>%
        mutate(cntry.id = country) %>%
        mutate(cntry.id = gsub("Australia_New_Zealand", "Australia", .$cntry.id)) %>%
        mutate(cntry.id = gsub("England_Wales_Northern_Ireland",
                               "United Kingdom", .$cntry.id)) %>%
        left_join(countries, by = c("cntry.id" = "Economy")) %>%
        mutate(wb.region = Region) %>%
        select(- c(Code, Region))

dta$pub.year <- factor(dta$pub.year)
dta$country <- factor(dta$country)
dta$data.source <- factor(dta$data.source)
dta$cntry.id <- factor(dta$cntry.id)
dta$wb.region <- factor(dta$wb.region)

all.inc.srch <- filter(dta, ! is.na(inc.abst.sem) & ! is.na(inc.abst.sem))
all.inc.read <- filter(dta, inc.full.read == 1)

# Tidying up -----------------------------------------------------------------
intrm <- gather(all.inc.read, type, incid, incid.1:severe.incid.12) %>%
        mutate(type = gsub("incid", "sepsis", .$type)) %>%
        separate(type, into = c("type", "obs.nr"), sep = "s\\.") %>%
        mutate(type = gsub("sepsi", "sepsis", .$type))

intrm2 <- gather(all.inc.read, type, n.cases, cases.sepsis.1:cases.severe.sep.12) %>%
        mutate(type = gsub("cases.", "", .$type)) %>%
        mutate(type = gsub (".sep", ".sepsis", .$type)) %>%
        separate(type, into = c("type", "obs.nr"), sep = "s\\.")%>%
        mutate(type = gsub("sepsi", "sepsis", .$type))

intrm3 <- gather(all.inc.read, obs.nr, year, study.year.1:study.year.12) %>%
        mutate(obs.nr = gsub("study.year.", "", .$obs.nr))

intrm4 <- gather(all.inc.read, obs.nr, avg.age, avg.age.1:avg.age.2) %>%
        mutate(obs.nr = gsub("avg.age.", "", .$obs.nr))

dta <- full_join(intrm, intrm2) %>%
        full_join(intrm3) %>%
        full_join(intrm4) %>%
        select(- contains("incid.")) %>%
        select(- contains("cases.")) %>%
        select(- contains("study.year.")) %>%
        select(- contains("avg.age.")) %>%
        mutate(id.artcl = gsub("a", "", .$id)) %>%
        mutate(id.artcl = gsub("b", "", .$id.artcl)) %>%
        mutate(id.artcl = gsub("c", "", .$id.artcl))

dta$type <- factor(dta$type)
dta$obs.nr <- factor(dta$obs.nr)
dta$id.artcl <- factor(dta$id.artcl)

rm(intrm, intrm2, intrm3, intrm4, dataurl1, dataurl2)

# Summary stats -------------------------------------------------------------

summary(dta[dta$type == "sepsis", "incid"])
summary(dta[dta$type == "severe.sepsis", "incid"])

# Distribution of definitions used for reported OBSERVATIONS (not studies)...
# in chart based studies
filter(dta, ! is.na(incid)) %>%
        distinct(id) %>%
        group_by(sepsis.def) %>%
        summarise(n())

filter(dta, ! is.na(incid)) %>%
        distinct(id) %>%
        group_by(org.fail.def) %>%
        summarise(n())

# in code based studies
filter(dta, ! is.na(incid)) %>%
        distinct(id) %>%
        group_by(coding.sys) %>%
        summarise(n())

# Distribution of definitions used for reported STUDIES (not observations)...
# in chart based studies
filter(dta, ! is.na(incid)) %>%
        distinct(id.artcl) %>%
        group_by(sepsis.def) %>%
        summarise(n())

filter(dta, ! is.na(incid)) %>%
        distinct(id.artcl) %>%
        group_by(org.fail.def) %>%
        summarise(n())

# in code based studies
filter(dta, ! is.na(incid)) %>%
        distinct(id.artcl) %>%
        group_by(coding.sys) %>%
        summarise(n())

# Summaries for the flow chart (figure 1) ----------------------------------------------------------

# Step 1: total number of articles screened
flow <- mutate(all.inc.srch, id.artcl = gsub("a", "", all.inc.srch$id)) %>%
        mutate(id.artcl = gsub("b", "", .$id.artcl)) %>%
        mutate(id.artcl = gsub("c", "", .$id.artcl)) %>%
        mutate(id.artcl = factor(id.artcl)) %>%
        distinct(id.artcl)

table(flow$found.by.ref, useNA = c("always"))
# "1" = article found by searching reference list of other article

# excluding those found by searching reference list of other article
datbas.srch <- filter(flow, found.by.ref == 0)

# Step 2: Number excluded after screening
ftable(datbas.srch$inc.abst.ahe, datbas.srch$inc.abst.sem,
       datbas.srch$inc.full.read, useNA = c("always"))
# "0, 0, NA" = those who were excluded directly after screening (where the two
# first authors agreed)

# The reasons for exclusion
excl.1 <- filter(datbas.srch, inc.full.read %in% NA)
table(excl.1$reason)
# Subsets
# not relevant design:
filter(excl.1, reason %in% c(0, 1)) %>% summarise(n())
# sepsis incidence not an outcome:
filter(excl.1, reason %in% 2) %>% summarise(n())
# sepsis incidence reported for a subgroup:
filter(excl.1, reason %in% c(3:17)) %>% summarise(n())
# sepsis incidence not reported on a person-year basis:
filter(excl.1, reason %in% c(18, 19)) %>% summarise(n())
# unable to obtain further information:
filter(datbas.srch, reason %in% 20) %>% summarise(n())

# Step 3: Number of articles included for full assessment after initial data base
# search
filter(datbas.srch, !reason %in% 20) %>%
        group_by(inc.full.read) %>%
        tally()
# "0" and "1" are articles read in full

# Step 4: Studies found in reference lists
excl.2 <- filter(flow, found.by.ref == 1)
# number and reason for exclusion of these articles
table(excl.2$full.read, excl.2$reason, useNA = c("always"))
# "NA" (horisontal) is number of articles included. "NA" (vertical) are articles
# excluded after screening of title and abstract.

# trin 5 - hele artikler der læses
# Step 5: Articles read in full
excl.3 <- filter(flow, full.read == 1) %>%
        filter(!reason %in% 20)
# Number of articles still included after reading through the full article:
table(excl.3$inc.full.read)
# "1" is articles included

# Reasons for exclusion:
table(excl.3$reason)
# not relevant design:
filter(excl.3, reason %in% c(0, 1)) %>% summarise(n())
# sepsis incidence not an outcome:
filter(excl.3, reason %in% 2) %>% summarise(n())
# sepsis incidence reported for a subgroup:
filter(excl.3, reason %in% c(3:17)) %>% summarise(n())
# sepsis incidence not reported on a person-year basis
filter(excl.3, reason %in% c(18, 19)) %>% summarise(n())
# unable to obtain further information:
filter(excl.3, reason %in% c(20)) %>% summarise(n())

# Step 6: Articles included for analysis, stratified on coding system
table(all.inc.read[!duplicated(all.inc.read$title), "coding.sys"])

rm(excl.1, excl.2, excl.3, datbas.srch, all.inc.read, all.inc.srch)

# Data frame for plots --------------------------------------------------------------------
dtap <- dta %>%
        mutate(coding.sys = ifelse(coding.sys == "Chart", "Chart", "Code")) %>%
        mutate(type = ifelse(type == "severe.sepsis", "Severe sepsis", "Sepsis"))
dtap$type <- factor(dtap$type)
dtap$coding.sys <- factor(dtap$coding.sys)

clrs <- c("#32CD32", "#e31a1c", "#6a3d9a", "#000080", "#1f78b4", "#b15928",
          "#FF00FF", "#000000", "#ff7f00", "#7FFFD4", "#00BFFF", "#fdbf6f",
          "#FF1493", "#00CED1", "#C1FFC1", "#228B22", "#FFDEAD", "#2B2B2B",
          "#595959", "#B5B5B5", "#00CD00", "#FF6EB4", "#F0E68C", "#ADD8E6",
          "#B4CDCD", "#FF0000", "#FFA07A", "#8470FF", "#FFD700", "#CD9B1D")

# Figure 2 ------------------------------------------------
ggincdYr <- dtap %>%
        mutate(auth2 = gsub("([A-Za-z]+).*", "\\1", .$author)) %>%
        unite(intrm, c(auth2, pub.year), sep = " et al., ", remove = FALSE) %>%
        mutate(intrm2 = gsub("[0-9]", "", .$id)) %>%
        unite(Study, c(intrm, intrm2), sep = "", remove = TRUE) %>%
        filter(!incid %in% NA) %>%
        ggplot(aes(year, incid)) +
        geom_point(aes(colour = Study), size = 3) +
        geom_line(aes(colour = Study)) +
        scale_colour_manual(values = sample(clrs)) +
        scale_x_continuous(name = "Year") +
        scale_y_continuous(name = "Incidence pr. 100,000 person-years") +
        facet_grid(type ~ coding.sys)
ggincdYr
# Gives a warning that can be ignored

# Figure 3 ----------------------------------------------------------------
ggwbr <- mutate(dtap, coding.sys = ifelse(coding.sys == "Chart", "Chart", "Code")) %>%
        filter(! is.na(incid)) %>%
        ggplot(aes(wb.region, incid)) +
        geom_boxplot() +
        geom_jitter(aes(colour = coding.sys)) +
        labs(x = "World Bank region", y = "Incidence pr. 100,000 person-years") +
        scale_colour_discrete(name = "Method to identify\n(severe) sepsis") +
        facet_grid(. ~ type)
ggwbr

# Additional file 4 -------------------------------------------------------
ggmethSep <- mutate(dtap, coding.sys =
                            ifelse(coding.sys == "Chart", "Chart", "Code")) %>%
        filter(! is.na(incid)) %>%
        filter(type == "Sepsis") %>%
        ggplot(aes(coding.sys, incid)) +
        geom_boxplot() +
        geom_jitter(aes(colour = sepsis.def)) +
        scale_colour_discrete(name = "Criteria used in chart studies") +
        labs(x = "Method to identify sepsis",
             y = "Incidence pr. 100,000 person-years") +
        facet_grid(type ~ data.source)
ggmethSep

# Additional file 5 -------------------------------------------------------
ggmethSevSep <- mutate(dtap, coding.sys =
                               ifelse(coding.sys == "Chart", "Chart", "Code")) %>%
        filter(! is.na(incid)) %>%
        filter(type == "Severe sepsis") %>%
        ggplot(aes(coding.sys, incid)) +
        geom_boxplot() +
        geom_jitter(aes(colour = org.fail.def)) +
        scale_colour_discrete(name = "Criteria used in chart studies") +
        labs(x = "Method to identify severe sepsis",
             y = "Incidence pr. 100,000 person-years") +
        facet_grid(type ~ data.source)
ggmethSevSep
