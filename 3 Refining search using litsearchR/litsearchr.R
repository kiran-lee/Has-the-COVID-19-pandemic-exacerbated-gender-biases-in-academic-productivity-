library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
install.packages("remotes")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
packageVersion("litsearchr")
list.files("./naive_results/")
usable_databases()

# Import naive results
naiveresults <- import_results(file="./scopus.ris")

# Identify potential keywords

keywords <- extract_terms(keywords=naiveresults[, "keywords"], method="tagged", min_n=1)
extract_terms(text=naiveresults[, "title"], method="fakerake", min_freq=3, min_n=2)
extract_terms(text=naiveresults[, "abstract"], method="fakerake", min_freq=3, min_n=2)

stopwords<-get_stopwords(language = "English")

title_terms <- extract_terms(
  text=naiveresults[, "title"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=stopwords
)

abstract_terms <- extract_terms(
  text=naiveresults[, "abstract"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=stopwords
)
terms <- unique(c(keywords, title_terms, abstract_terms))

# Network analysis

docs <- paste(naiveresults[, "title"], naiveresults[, "abstract"])
docs[1]
dfm <- create_dfm(elements=docs, features=terms)
dfm[1:3, 1:4]
g <- create_network(dfm, min_studies=3)

# Pruning key words
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths
term_strengths

write.csv(term_strengths, file = "term_strengths.csv")

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.6)
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")
get_keywords(reduce_graph(g, cutoff_cum))

cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)
cutoff_change
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)
extra_terms <- c(
  "men",
  "gap",
  "inequality",
  "inequity",
  "unequal",
  "parity",
  "imbalance",
  "disproportion",
  "skew",
  "fewer",
  "publish",
  "productivity",
  "academia",
  "journal"
)
selected_terms <- c(selected_terms, extra_terms)

selected_terms

# Grouping key terms

grouped_terms <-list(
  pandemic=selected_terms[c(31, 32, 33,34,35,36,53,104,107,138,139)],
  gender=selected_terms[c(58, 59, 89, 114, 115, 148, 169, 196)],
  gap=selected_terms[c(16, 47, 60,186,198,199,200,201,202,203,204,205)],
  academia=selected_terms[c(14,38,39,92,93,129,141,142,143,163,167,188,208,209)],
  performance=selected_terms[c(109,123,206,207)]
)
grouped_terms

# litsearchR recommended key terms. Note: I exclude epidemic, outbreak and sars-cov-2 from pandemic group; pregnant and pregnancy from the gender group; and medicine, medline, scopus, web of science and included studies from the academia group to make results more specific to the study. 
write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=FALSE,
  closure="left",
  writesearch=TRUE
)

new_results <- import_results(file="scopus_126.bib")
nrow(new_results)


# Write in scoped article titles
important_titles <- c(
  "The impact of COVID-19 on academic productivity by female physicians and researchers in transfusion medicine",
  "Longitudinal analyses of gender differences in first authorship publications related to COVID-19",
  "Gender disparity in the authorship of biomedical research publications during the COVID-19 pandemic: Retrospective observational study",
  "Impact of the Coronavirus Disease 2019 Pandemic on Authorship Gender in The Journal of Pediatrics: Disproportionate Productivity by International Male Researchers",
  "Impact of COVID-19 on longitudinal ophthalmology authorship gender trends",
  "The Pandemic Penalty: The Gendered Effects of COVID-19 on Scientific Productivity",
  "Gender inequality in publishing during the COVID-19 pandemic",
  "Gender Differences in Publication Authorship During COVID-19: A Bibliometric Analysis of High-Impact Cardiology Journals",
  "COVID-19 medical papers have fewer women first authors than expected",
  "Gender Differences in First and Corresponding Authorship in Public Health Research Submissions During the COVID-19 Pandemic",
  "Gender gap in women authors is not worse during COVID-19 pandemic: Results from Research and Practice in Thrombosis and Haemostasis",
  "Academic Productivity Differences by Gender and Child Age in Science, Technology, Engineering, Mathematics, and Medicine Faculty During the COVID-19 Pandemic",
  "Effects of the COVID-19 pandemic on gender representation among corresponding authors of Neuropsychopharmacology (NPP) manuscripts: submissions during January–June, 2020",
  "Comparison of the Proportions of Female and Male Corresponding Authors in Preprint Research Repositories Before and During the COVID-19 Pandemic",
  "The Impact of the COVID-19 Pandemic on Journal Scholarly Activity Among Female Contributors",
  "Consequences of the COVID-19 Pandemic on Manuscript Submissions by Women",
  "Gender differences in authorship of obstetrics and gynecology publications during the coronavirus disease 2019 pandemic"
)

data.frame(check_recall(important_titles, new_results[, "title"]))

data.frame(check_recall(important_titles, naiveresults[, "title"]))


# Import results from revised search string and de-duplicate
revisedimport <- import_results(file = "./revised_results.ris")

colnames(revisedimport)

revisedresults <-
  litsearchr::remove_duplicates(revisedimport, "title", "stringdist", threshold=0.99)

revisedresults <-
  litsearchr::remove_duplicates(revisedresults, field = "abstract", method = "string_osa")


revisedresults <-
  litsearchr::remove_duplicates(revisedimport, field = "title", method = "string_osa")
revisedresults <-
  litsearchr::remove_duplicates(revisedresults, field = "abstract", method = "string_osa")

# Export revised, de-duplicated results for screening in Rayyan
write.csv(revisedresults, "revisedresults.csv")

str(revisedresults)

# Code not used, but left here because it is potentially useful later on
import<-
  import_results(
    directory = "./naive_results/"
  )

search_directory <- system.file("extdata", package="litsearchr")
naiveimport <-
  litsearchr::import_results(directory = "./naive_results/", verbose = TRUE)

naiveimport <- import_results(file="./naive_results/scopus.ris")

naiveresults <- import_results(file="./naive_results/scopus.ris")

naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "abstract", method = "string_osa")
