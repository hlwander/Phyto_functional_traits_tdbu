# Script to bring in reviewed Rayyan articles and clean spreadsheet
# n = 540 full-text articles screened; 256 included 

#load in packages
if (!require("pacman")) {install.packages("pacman") }
pacman::p_load(readxl, dplyr, tidyverse, ggpubr, stringr, ggplot2)

# Path to your Excel file
file_path <- "data/TD_BU_data_extraction.xlsx"

# List of sheet names to extract
sheets_to_get <- c("Ewaldo", "Isabelle", "Anika", "Heather", "Arianna", 
                   "Megan", "Britt", "Emmy")

#read in excel spreadsheet
combined <- sheets_to_get |>
  lapply(function(s) {
    read_excel(file_path, sheet = s) |>
      select(study:include) |>
      mutate(across(everything(), as.character),
             reviewer = s)}) |>
  bind_rows() 

#clean up the column names 
rayyan_papers <- combined |>
  mutate(microbes = coalesce(microbes, microbes_het)) |>
  dplyr::select(-microbes_het) |>
  drop_na(study)

#clean the study column to help with matching
rayyan_papers_clean <-   rayyan_papers |>
  mutate(study = str_trim(study), #remove commas and white space
  study = str_remove_all(study, "[,;]"),
  study = str_replace_all(study, #fix variations of et al
         regex("\\b[eE][yt]*\\s*a[l]?\\.?\\s+(?=\\d{4})", ignore_case = TRUE),  "et al "),
  study = str_replace_all(study, "(?<!St)\\.", "")) |> #remove stray periods 
  dplyr::select(-c(rayyan_id, 'original bin number'))

#now chek that the study column matches the expected patterns
valid_pattern <- "^[A-Z][A-Za-z\\-]+( (and [A-Z][A-Za-z\\-]+|et al))? \\d{4}$"

rayyan_papers_clean |> filter(!str_detect(study, valid_pattern)) |> 
  dplyr::select(study)
#these are okay - just variations in author names

#count the number of double-reviewed studies (n=110 papers)
rayyan_papers_clean |> 
  count(study, name = "n") |> 
  arrange(desc(n)) |>
  filter(n >1)

#now look at all the distinct study names
list <- rayyan_papers_clean |> 
  count(study) |> 
  filter(n == 1) |> 
  arrange(desc(n)) |>
  dplyr::select(study)
# Congqiang et al 2018 --> Luo et al 2018 (Arianna)
# Almeira et al 2022 --> Almeida et al 2023 (Ewaldo)
# Balkic et al 2019 --> Galir Balkic et al 2019 (Emmy) 
# Boras and Vaque 2015 --> Boras et al 2015 (Arianna)
# Corno et al 2008 --> Corno and Jurgens 2008 (Anika)
# Devkota et al 2022 --> Devkota et al 2023 (Ewaldo)
# Di Pane et al 2021 --> Di Pane et al 2022 (Isabelle)
# Diaz et al 2011 --> Diaz et al 2019 (Anika)
# Gangere et al 2010 --> Grangere et al 2010 (Ewaldo)
# Geider and Laroche 1994 --> Geider and Roche 1994 (Ewaldo)
# Ghao et al 2022 --> Gao et al 2022 (Anika)
# Griffith et al 2013 --> Griffiths et al 2013 (Anika)
# Helminen et al 1997 --> Helminen and Sarvala 1997 (Emmy)
# Hill et al 2008 --> Hill and McQuaid 2008 (Britt)
# Hoehn et al 1995 --> Hoehn anf Schmidt-Halewicz 1995 (Britt) 
# Hrycik et al 2021 --> Hrycik and Stockwell 2021 (Emmy)
# Huete-Stauffer et al 2012 --> Huete-Stauffer and Moran 2012 (Ewaldo)
# Karuza et al 2015 --> Karuza et al 2016 (Heather)
# Koslow et al 2013 --> Koslow et al 2014 (Heather)
# Lepère et al 2006 --> Lepere et al 2006 (Britt)
# Metaxas et al 1996 --> Metaxas and Scheibling 1996 (Anika)
# Montes-Hugo et al 2007 --> Montes-Hugo and Alvarez-Borrego 2007 (Emmy)
# Nevalainen et al 2017 --> Nevalainen and Luoto 2017 (Emmy)
# Oguz and Gilbert 2006 -> Oguz and Gilbert 2007 (Arianna)
# Papanoniou et al 2021 --> Papantoniou et al 2021 (Britt)
# Portielje et al 1999 --> Portielje and Van der Molen 1999 (Emmy)
# Rubao et al 2013 --> Ji et al 2013 (Britt)
# Sommer and Sommer 2001 --> Sommer et al 2001 (Anika)
# Sommer and Sommer 2003 --> Sommer et al 2003 (Anika)
# Tassier and Woodruff 2022 --> Tessier and Woodruff 2002 (Ewaldo)
# Wan et al 2023 --> Wan et al 2024 (Britt)
# Wilken et al 2013 --> Wilken et al 2014 (Isabelle)
# Yaragina et al 2009 --> Yaragina and Dolgov 2009 (Anika)
# Balkic et al 2023/4 --> Balkic et al 2024 (Megan)
# Belkania et al 2021 --> Belkahia et al 2021 (Megan)
# Buovy et al 2022 --> Bouvy et al 2022 (Megan)
# Dietrich & Arndt 2000 --> Dietrich and Arndt 2000 (Megan)
# Frau & Gutierrez 2024 --> Frau and Gutierrez 2024 (Megan)
# Frueh et al 2011 --> Fruh et al 2011 (Megan)
# Fryxell et al 2017 --> Fryxell and Palkovacs 2017 (Megan)
# Gaxiola-Castr 2008 --> Gaxiola-Castro et al 2008 (Megan)
# Hayraptyan et al 2023 --> Hayrapetyan et al 2023 (Megan)
# Sanderson and Frost1996 --> Sanderson and Frost 1996 (Megan)
# Segovia et al 2018/7 --> Segovia et al 2017 (Megan)
# Steiner et al 2002 --> Steiner 2002 (Megan)
# Sunback et al 2007 --> Sundback et al 2007 (Megan)
# Wilken 2014 --> Wilken et al 2014 (Megan)
# Othman et al 2018 --> Ben Othman et al 2018 (Emmy)
# Cossarini et al 2008 --> Cossarini and Solidoro 2008 (Britt and Anika)
# Dory et al 2022 --> Dory et al 2023 (Ewaldo)
# Hjoth et al 2008 --> Hjorth et al 2008 (Isabelle)
# Levesque et al 2018 --> Levesque et al 2019 (Heather)
# Roberts et al 2004 --> Roberts et al 2003 (Isabelle and Heather)
# d'Oultremont et al 2002 --> d’Oultremont and Gutierrez 2002 (Emmy)
# Dutkiewics et al 2021--> Dutkiewicz et al 2021 (Emmy)
# Hugget et al 2023 --> Huggett et al 2023 (Megan)

#look at all single author papers
rayyan_papers_clean |>
  filter(!str_detect(study, regex("\\bet al\\b|\\band\\b", ignore_case = TRUE))) |>
  dplyr::select(study)
# Le Noac'h 2021 --> Le Noac'h et al 2021 (Ewaldo)
# Goericke 2009 --> Goericke 2002 (Anika)
# Corno 2005 --> Corno 2006 (Isabelle)
# Samuelsson 2003 --> Samuelsson and Andersson 2003 (Britt)
# Samuelsson et al 2003 --> I believe this should be Samuelsson and Andersson 2003 but need to check w/ Britt
# Bertolo 1999 --> Bertolo et al 1999 (Megan)

#### correct study typos ####
study_corrections <- tibble::tibble(
  study_orig = c("Le Noac'h 2021", "Goericke 2009", "Corno 2005",
                 "Samuelsson 2003", "Congqiang et al 2018", "Almeira et al 2022",
                 "Balkic et al 2019", "Boras and Vaque 2015", "Corno et al 2008",
                 "Devkota et al 2022", "Di Pane et al 2021", "Diaz et al 2011",
                 "Gangere et al 2010", "Geider and Laroche 1994", "Ghao et al 2022",
                 "Griffith et al 2013", "Helminen et al 1997", "Hill et al 2008",
                 "Hoehn et al 1995", "Hrycik et al 2021", "Huete-Stauffer et al 2012",
                 "Karuza et al 2015", "Koslow et al 2013", "Lepère et al 2006",
                 "Metaxas et al 1996", "Montes-Hugo et al 2007", "Nevalainen et al 2017",
                 "Oguz and Gilbert 2006", "Papanoniou et al 2021", "Portielje et al 1999",
                 "Rubao et al 2013", "Sommer and Sommer 2001", "Sommer and Sommer 2003",
                 "Tassier and Woodruff 2022", "Wan et al 2023", "Wilken et al 2013",
                 "Yaragina et al 2009","Mathes et al 1995","Mathes et al 1995",
                 "Balkic et al 2023/4", "Bertolo 1999", "Buovy et al 2022",
                 "Dietrich & Arndt 2000", "Frau & Gutierrez 2024", "Frueh et al 2011",
                 "Fryxell et al 2017", "Gaxiola-Castr 2008", "Hayraptyan et al 2023",
                 "Sanderson and Frost1996", "Segovia et al 2018/7", "Steiner et al 2002",
                 "Sunback et al 2007", "Wilken 2014", "Othman et al 2018",
                 "Cossarini et al 2008", "Cossarini et al 2008", "Dory et al 2022",
                 "Hjoth et al 2008", "Levesque et al 2018", "Roberts et al 2004",
                 "Roberts et al 2004", "d'Oultremont et al 2002","Dutkiewics et al 2021",
                 "Grillo-Avila et al 2025", "Hugget et al 2023", "Samuelsson et al 2003"),
  study_fixed = c("Le Noac'h et al 2021", "Goericke 2002", "Corno 2006",
                  "Samuelsson and Andersson 2003", "Luo et al 2018",
                  "Almeida et al 2023", "Galir Balkic et al 2019", "Boras et al 2015",
                  "Corno and Jurgens 2008", "Devkota et al 2023", "Di Pane et al 2022",
                  "Diaz et al 2019", "Grangere et al 2010", "Geider and Roche 1994",
                  "Gao et al 2022", "Griffiths et al 2013", "Helminen and Sarvala 1997",
                  "Hill and McQuaid 2008", "Hoehn anf Schmidt-Halewicz 1995",
                  "Hrycik and Stockwell 2021", "Huete-Stauffer and Moran 2012",
                  "Karuza et al 2016", "Koslow et al 2014", "Lepere et al 2006",
                  "Metaxas and Scheibling 1996", "Montes-Hugo and Alvarez-Borrego 2007",
                  "Nevalainen and Luoto 2017", "Oguz and Gilbert 2007", 
                  "Papantoniou et al 2021", "Portielje and Van der Molen 1999",
                  "Ji et al 2013", "Sommer et al 2001", "Sommer et al 2003",
                  "Tessier and Woodruff 2002", "Wan et al 2024", "Wilken et al 2014",
                  "Yaragina and Dolgov 2009", "Mathes and Arndt 1995", "Mathes and Arndt 1995",
                  "Balkic et al 2024", "Bertolo et al 1999", "Bouvy et al 2022",
                  "Dietrich and Arndt 2000", "Frau and Gutierrez 2024", "Fruh et al 2011",
                  "Fryxell and Palkovacs 2017", "Gaxiola-Castro et al 2008",
                  "Hayrapetyan et al 2023", "Sanderson and Frost 1996", "Segovia et al 2017",
                  "Steiner 2002", "Sundback et al 2007", "Wilken et al 2014",
                  "Ben Othman et al 2018", "Cossarini and Solidoro 2008",
                  "Cossarini and Solidoro 2008", "Dory et al 2023", "Hjorth et al 2008",
                  "Levesque et al 2019", "Roberts et al 2003", "Roberts et al 2003",
                  "d’Oultremont and Gutierrez 2002", "Dutkiewicz et al 2021",
                  "Grillo‑Avila et al 2025", "Huggett et al 2023", 
                  "Samuelsson and Andersson 2003"),
  reviewer = c("Ewaldo", "Anika", "Isabelle", "Britt", "Arianna", "Ewaldo", "Emmy",
               "Arianna", "Anika", "Ewaldo", "Isabelle", "Anika", "Ewaldo", "Ewaldo",
               "Anika", "Anika", "Emmy", "Britt", "Britt", "Emmy", "Ewaldo", "Heather",
               "Heather", "Britt", "Anika", "Emmy", "Emmy", "Arianna", "Britt", 
               "Emmy", "Britt", "Anika", "Anika", "Ewaldo", "Britt", "Isabelle",
               "Anika", "Britt", "Emmy", "Megan", "Megan", "Megan", "Megan", "Megan",
               "Megan", "Megan", "Megan", "Megan", "Megan", "Megan", "Megan", "Megan",
               "Megan", "Emmy", 'Anika', "Britt", "Ewaldo", "Isabelle", "Heather",
               "Heather", "Isabelle", "Emmy", "Emmy", "Ewaldo", "Megan", "Britt"))

#now update the study names
rayyan_papers_clean <- rayyan_papers_clean |>
  left_join(study_corrections, by = c("study" = "study_orig", "reviewer" = "reviewer")) |>
  mutate(study = coalesce(study_fixed, study)) |>
  select(-study_fixed)

#check if include decisions agree (n=95)
include_disagreements <- rayyan_papers_clean |>
  group_by(study) |>
  summarise(n_reviewers = n(),
    n_unique_decisions = n_distinct(include),
    include_responses = paste(unique(include), collapse = ", "),
    .groups = "drop") |>
  filter(n_reviewers > 1 & n_unique_decisions > 1)

#### read in references and check that the keeps are all accounted for ####
refs <- read.csv("data/articles.csv") 
#n=263 but note that several of these were maybes that ended up being excluded

#create study col for matching
refs <- refs |>
  rowwise() |>
  mutate(study = {
    split_auth <- str_split(authors, " and ")[[1]]
    last_names <- str_trim(str_extract(split_auth, "^[^,]+"))
    n <- length(last_names)
    if (n == 1) {
      paste0(last_names[1], " ", year)
    } else if (n == 2) {
      paste0(last_names[1], " and ", last_names[2], " ", year)
    } else {
      paste0(last_names[1], " et al ", year)}}) |>
  ungroup() |>
  mutate(study = str_to_title(str_to_lower(study)),  #make sure the authors are not capitalized
         study = str_replace_all(study, "\\bEt Al\\b", "et al"),
         study = str_replace_all(study, "\\bAnd\\b", "and"))

#check for mismatches
missing <- refs |> 
  anti_join(rayyan_papers_clean, by = "study") |> 
  distinct(study) |> 
  arrange(study)
#note tha the Congqiang is def the first name and I corrected to Luo et al 2018
#Fonseca Da Silva et al 2019 is just da silva et al 2019
#all the rest are slight differences in names (e.g., hyphens, accents, etc...)

#### exclude papers that do not meet our lit review criteria ####
rayyan_study_yes <- rayyan_papers_clean |>
  filter(include %in% c("include","y", "yes", "Yes")) |>
  dplyr::select(study)

#filter papers that have at least one yes
rayyan_papers_include <- rayyan_papers_clean |>
  filter(study %in% rayyan_study_yes$study,
         !study %in% c("Ji et al 2013", "Kalcheva et al 2010", 
                       "Pan et al 2024", "Rumschlag et al 2020",
                       "Yang et al 2019", "Charalampous et al 2024"))
         
#drop the Zhang et al 2021 and 2023 papers (n=4 papers)
rayyan_papers_include <- rayyan_papers_include[!(rayyan_papers_include$study=="Zhang et al 2021" &
                                  rayyan_papers_include$include=="no"),]

rayyan_papers_include <- rayyan_papers_include[!(rayyan_papers_include$study=="Zhang et al 2023" &
                                                   rayyan_papers_include$include %in% c("n","maybe/exclude")),]

#studies that need to be checked:
# Almeda et al 2018 (Megan)
# Garcia-Gomez et al 2020 (Britt?) --> isn't OTU classification an omics approach?

#discrepancies
# Arnold et al 2009 --> yes
# Bhele et al 2022 --> yes
# Kong et al 2020 --> yes
# Lemmens et al 2018 --> yes
# Pan et al 2024 --> no (waiting on Britt to confirm)
# Rochera et al 2017 --> yes 
# Sommer et al 2003 --> yes (ignore Anika decision)
# Tong et al 2023 --> yes (ignore Anika decision)
# Zhang et al 2021 --> two different papers, one included and one not
# same with Zhang et al 2023


#### clean up the spreadsheet cols of interest ####
rayyan_papers_include_final <- rayyan_papers_include |>
  mutate(ecosystem = ifelse(ecosystem %in% c("marine", "marine; bay", "marine; coastal; aquaculture",
                                             "marine; salinity gradient from freshwater input",
                                             "Marine", "MArine"), "marine",
                      ifelse(ecosystem %in% c("estuary", "Estuarine", "estuarine", "brackish"),
                             "estuary",
                      ifelse(ecosystem %in% c("freshwater","freshwater; lacustrine",
                                              "freshwater, lacustrine", 
                                              "lab (volvic water and munich well water)",
                                              "Freshwater (Saline lakes)","Freshwater",
                                              "freshwater; coastal lagoon"), "freshwater", ecosystem)))) |>
#freshwater; coastal lagoon --> classifying as fw bc it's a constructed wetland that flows into a lagoon
  mutate(top_bottom_both = dplyr::na_if(top_bottom_both, "?"),
         top_bottom_both = dplyr::na_if(top_bottom_both, "NA"),
         top_bottom_both = as.numeric(top_bottom_both)) |>
  mutate(phyto_func_response = ifelse(phyto_func_response %in% c("yes","1.0", "1"), 1,
                                ifelse(phyto_func_response %in% c("no", "0", "0.0"), 0, NA))) |>
  mutate(func_group_type = ifelse(func_group_type %in% c("taxonomic", "Taxonomic", "see notes",
                                                         "Taxonomy", "taxonomic / pigment"), "taxonomic",
                           ifelse(func_group_type %in% c("morphological","Morphological (size)","Morphological"),
                                  "morphological",
                           ifelse(func_group_type %in% c("physiological", "acc. Reynolds?", "Physiological (trophic)"),
                                  "physiological",
                           ifelse(func_group_type %in% c("taxonomic, morphological","taxonomic and a little morphological",
                                                         "taxonomic and morphological","taxonomic; morphological",
                                                         "morphological, taxonomic","morphological; taxonomic",
                                                         "Taxonomic, morphological","Morphological, taxonomic",
                                                         "Taxonomic / morphological","Taxonomic / morhpological"), 
                                  "taxonomic, morphological",
                           ifelse(func_group_type %in% c("taxonomic, morphological, physiological",
                                                         "taxonomic, physiological, morphological",
                                                         "taxonomic morphological physiological",
                                                         "taxonomic, physiological and morphological",
                                                         "taxonomic; morphology; physiology (see notes)",
                                                         "physiological; morphological; taxonomic",
                                                         "Taxonomic, physiological, morphological",
                                                         "orphological, taxonomic, physiology",
                                                         "Morphological, taxonomic, physiology a bit",
                                                         "Morphological, taxonomic, physiology"), 
                                  "taxonomic, morphological, physiological",
                            ifelse(func_group_type %in% c("morphological, physiological","morphological; physiological",
                                                          "physiological, morphological","physiological; morphological",
                                                          "Morphological, physiological","Morphological / physiology"), 
                                   "morphological, physiological",
                            ifelse(func_group_type %in% c("taxonomic and physiological","physiological; taxonomic",
                                                          "taxonomic; physiological","physiological (some taxonomic)",
                                                          "Taxonomic, physiological (resistance to a heat wave)",
                                                          "Taxonomic, physiological","Taxonomic / physiology"),
                                   "taxonomic, physiological",
                            ifelse(func_group_type %in% c("taxonomic, omics","taxonomic / omics"), 
                                   "taxonomic, omics", func_group_type))))))))) |>
  mutate(fish = ifelse(fish %in% c("1 (mussels, snails, etc.)", "1 (mussels)"), "1", fish),
    across(c(zoo, fish, microbes),  ~ as.numeric(dplyr::na_if(.x, "?")))) |> #make cols numeric
  mutate(study_type = ifelse(study %in% "McMahon et al 2012", "lab", study_type)) |>
  mutate(study_type = ifelse(study_type %in% c("field", "filed","field, molecular barcoding",
                                               "field; experiment","incubation","mesocosm",
                                               "outdoor experiment", "field; experiment; incubation",
                                               "experiment"), "field",
                      ifelse(study_type %in% c("field, lab","field; lab","lab, field",
                                               "field / lab", "field and lab"), "field, lab",
                      ifelse(study_type %in% c("lab", "lab with environmental isolates"), "lab",
                      ifelse(study_type %in% c("field; model; experiment", "field / model"),
                             "field, model",
                      ifelse(study_type %in% c("model","biogeochemical ocean model",
                                               "mass-balance trophic model","modelling"),
                             "model", study_type)))))) |>
  mutate(experimental_design = ifelse(experimental_design %in% c("in situ, mesocosm","in situ, microcosm",
                                                                 "mesocosm; in situ","in-situ, incubation",
                                                                 "in situ, incubation","incubation, in situ",
                                                                 "in situ; mesocosm","in situ; microcosm",
                                                                 "in situ and incubations on ship", "outdoor mesocosm",
                                                                 "in situ / mesocosm (whole lake experiment)"),
                                      "in situ, experiment",
                                ifelse(experimental_design %in% c("in vitro","microcosm, mesocosm","mescosm","microcosm",
                                                                  "mesocosm","microcosm?, incubation", "microcosm/incubation",
                                                                  "mesocosm (field enclosure)","incubation"), "experiment",
                                ifelse(experimental_design %in% c("in situ / in silico", "in situ, insilico",
                                                                         "in silico based on in situ data cruises included in study"),
                                       "in situ, in silico",
                                ifelse(experimental_design %in% c("mesocosm / barcoding"), "experiment, omics",
                                ifelse(experimental_design %in% c("omics (barcoding)"), "omics", experimental_design)))))) |>
  mutate(importance_td_vs_bu = ifelse(importance_td_vs_bu %in% c("td","td (speculated in discussion, not a primary result)",
                                                                 "td emphasis"), "td",
                               ifelse(importance_td_vs_bu %in% c("both","NA (both)","na","td; bu", "NA"), NA, importance_td_vs_bu))) |>
  mutate(zoop_func_response = ifelse(zoop_func_response %in% c("no","0","0.0"), 0,
                              ifelse(zoop_func_response %in% c("yes","1","1.0"), 1, NA)))

#add year
rayyan_papers_include_final <-  rayyan_papers_include_final |>
  mutate(year = as.numeric(str_extract(study, "\\d{4}$")))

#export double-reviewed studies from cleaned df for checking
repeated_studies <- rayyan_papers_include_final |>
  group_by(study) |>
  summarise(n_reviewers = n(), .groups = "drop") |>
  filter(n_reviewers > 1) |>
  pull(study)

spreadsheet_to_check <- rayyan_papers_include_final |> #n=76
  filter(study %in% repeated_studies) |>
  arrange(study) |>
  dplyr::select(-c(zoo, fish, microbes, manipulation, coordinates, 
                   'spatial extent_km', 'temporal duration', 'trophic state',
                   zoop_func_response))
write.csv(spreadsheet_to_check, "data/papers_w_multiple_reviewers.csv", row.names = F)

#------------------------------------------------------------------------------#
#### FIGURES ####
year_eco_sum <- rayyan_papers_include_final |>
  filter(!is.na(ecosystem)) |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",") |>
  mutate(ecosystem = str_trim(ecosystem)) |> #split ecosystem into multiple rows when applicable 
  group_by(year, ecosystem) |>
  summarise(n = n(), .groups = "drop") 

#Fig. 1: Historical progression of use of functional groups across ecosystems
ggplot(year_eco_sum, # |> filter(!ecosystem %in% "estuary"),
       aes(x = year, y = n, color = ecosystem)) +
  geom_line(size = 1) + geom_point() + theme_bw() +
  labs(x = "", y = "Number of studies", color = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box.spacing = unit(0.001, "cm"))
#ggsave("figures/phyto_func_group_progression_years.jpg", width = 5, height = 4)

#Fig 2: total number of studies in each ecosystem
fg_eco_sum <- rayyan_papers_include_final |>
  filter(!is.na(ecosystem)) |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",") |>
  mutate(ecosystem = stringr::str_trim(ecosystem)) |>
  distinct(study, ecosystem, .keep_all = TRUE) |>  # keeps first instance only
  group_by(ecosystem) |>
  summarise(n = n(), .groups = "drop") 

ggplot(fg_eco_sum, aes(x = ecosystem, y = n, fill = ecosystem)) +
  geom_col(width = 0.7) + theme_bw() + 
  labs(x = "", y = "Number of studies", fill = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
#ggsave("figures/phyto_func_group_by_ecosyste_total.jpg", width = 5, height = 4)

#Fig 3: stacked bar plot of functional group types across ecosystems
fg_type_eco <- rayyan_papers_include_final |>
  filter(!is.na(ecosystem),
         !is.na(func_group_type)) |>
  mutate(ecosystem = stringr::str_replace_all(ecosystem, ";", ","),
         func_group_type = stringr::str_replace_all(func_group_type, ";", ",")) |>
  tidyr::separate_rows(ecosystem, sep = ",\\s*") |>
  tidyr::separate_rows(func_group_type, sep = ",\\s*") |>
  mutate(ecosystem = stringr::str_trim(ecosystem),
        func_group_type = stringr::str_trim(func_group_type)) |>
  distinct(study, ecosystem, func_group_type, .keep_all = TRUE) |>
  group_by(ecosystem, func_group_type) |>
  summarise(n = n(), .groups = "drop") 

ggplot(fg_type_eco,
       aes(x = ecosystem, y = n, fill = func_group_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  #geom_col(width = 0.7) + 
  theme_bw() +
  labs(x = "", y = "Number of studies", fill = "") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")
#ggsave("figures/phyto_func_group_by_ecosystem_total.jpg", width = 5, height = 4)

#heatmap to see how prevalent different functional group definitions are across ecosystems
ggplot(fg_type_eco, aes(x = func_group_type, y = ecosystem, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() + theme(axis.text = element_text(size=8),
                          axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Number of studies", x = "", y = "")
#ggsave("figures/phyto_func_group_by_ecosystem_heatmapl.jpg", width = 5, height = 4)

#fig 4: td/bu emphasis across ecosystems

rayyan_papers_include_final <- rayyan_papers_include_final |>
  mutate(importance_td_vs_bu = ifelse(top_bottom_both==3 & is.na(importance_td_vs_bu), 
                                      "both", importance_td_vs_bu))

fig4_df <- rayyan_papers_include_final |>
  filter(!is.na(ecosystem), !is.na(func_group_type)) |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ","),
    func_group_type = str_replace_all(func_group_type, ";", ",")) |>
  separate_rows(ecosystem, sep = ",\\s*") |>
  separate_rows(func_group_type, sep = ",\\s*") |>
  mutate(ecosystem = str_trim(ecosystem),
         func_group_type = str_trim(func_group_type),
         importance_td_vs_bu = if_else(is.na(importance_td_vs_bu),
                                       "NA",importance_td_vs_bu)) |>
  distinct(study, ecosystem, func_group_type, .keep_all = TRUE) |>
  group_by(ecosystem, func_group_type, importance_td_vs_bu) |>
  summarise(n = n(), .groups = "drop")

ggplot(fig4_df |> filter(!ecosystem %in% "estuary"),
       aes(x = func_group_type, y = n, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ ecosystem) +
  theme_bw() + labs(x = "Functional group type", y = "Number of studies",
                    fill = "Process emphasis") +
  theme(axis.text = element_text(size=8),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")
#ggsave("figures/td_bu_emphasis_by_func_groups.jpg", width = 5, height = 4)

#fig 5 - td vs bu across ecosysyems
td_eco_df <- rayyan_papers_include_final |>
  filter(!is.na(ecosystem)) |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",\\s*") |>
  mutate(ecosystem = str_trim(ecosystem),
         importance_td_vs_bu = if_else(is.na(importance_td_vs_bu), 
                                       "Not specified", importance_td_vs_bu)) |>
  distinct(study, ecosystem, .keep_all = TRUE) |>
  group_by(ecosystem, importance_td_vs_bu) |>
  summarise(n = n(), .groups = "drop")

ggplot(td_eco_df |> filter(!is.na(importance_td_vs_bu)),
       aes(x = ecosystem, y = n, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_bw() +
  labs(x = "", y = "Number of studies", fill = "Process emphasis") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")
#ggsave("figures/td_bu_emphasis_by_ecosystem.jpg", width = 5, height = 4)

#proportions
td_eco_prop <- td_eco_df |>
  group_by(ecosystem) |>
  mutate(prop = n / sum(n))

ggplot(td_eco_prop, aes(x = ecosystem, y = prop, fill = importance_td_vs_bu)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "", y = "Proportion of studies", fill = "Process emphasis")
