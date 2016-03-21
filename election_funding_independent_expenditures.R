# Analysis of election spending FEC data 2016
# Author:   Austin Czarnecki
# Contact:  austin.czarnecki@gmail.com
# Description: This script extracts data from the FEC data catalog to generate the diagrams for
#              the article on austinczarnecki.com "Election Funding 2016"

library("stringr")
library("R.utils")
library("rjson")
options(scipen=20)

# setwd() NOTE: use this to set your working directory to the directory of the script if needed

verbose <- FALSE; # set to TRUE for verbose logging

# Data file:    "Independent Expenditure" from the FEC data catalog
# Description:  Itemized spending is included for organizations and individuals 
#               spending > $1000 per election cycle.
#
# detailed description and metadata dictionary:
#   http://www.fec.gov/data/IndependentExpenditure.do?format=html&election_yr=2016
#
# direct download link: 
#   http://www.fec.gov/data/IndependentExpenditure.do?format=csv
#
# WARNING: Download directly in the read.csv call fails, but downloading and saving to the 
#          working directory works fine.
x <- read.csv("independent-expenditure.csv", as.is=TRUE);

# Data file:    "Candidate Summary" from the FEC data catalog
# Description:  Summary of candidate revenues and expenses for the current election
#               cycle, 2014-2016.
#
# detailed description and metadata dictionary:
#   http://www.fec.gov/data/CandidateSummary.do?format=html&election_yr=2016
#
# direct download link: 
#   http://www.fec.gov/data/CandidateSummary.do?format=csv
#
# WARNING: At the time of writing (3-14-2016) using the direct download link in this code fails, but 
#          downloading, opening the csv with Excel, resaving, and referencing the local file in this
#          code works. FEC get your act together please this shouldn't be necessary.
z <- read.csv("CandidateSummaryAction.csv", as.is=TRUE);

# columns from candidate summary data that contain dollar amounts
numeric_summary_cols <- c("ind_ite_con", "ind_uni_con", "ind_con", "par_com_con", "oth_com_con", 
                          "can_con", "tot_con", "tra_fro_oth_aut_com", "can_loa", "oth_loa", "tot_loa", 
                          "off_to_ope_exp", "off_to_fun", "off_to_leg_acc", "oth_rec", "tot_rec", 
                          "ope_exp", "exe_leg_acc_dis", "fun_dis", "tra_to_oth_aut_com", "can_loa_rep", 
                          "oth_loa_rep", "tot_loa_rep", "ind_ref", "par_com_ref", "oth_com_ref", 
                          "tot_con_ref", "oth_dis", "tot_dis", "cas_on_han_beg_of_per", 
                          "cas_on_han_clo_of_per", "net_con", "net_ope_exp", "deb_owe_by_com", 
                          "deb_owe_to_com")

# convert candidate summary numeric columns to dollar amounts
for (col in numeric_summary_cols) {
  writeLines("Converting numeric columns in candidate summary data.")
  if (verbose) {
    writeLines(paste("Converting column ", col, " to numeric..."))
  }
  z[,c(col)] <- round(as.numeric(gsub(',', '',gsub('\\$','',z[,c(col)]))))
}

# we only care about presidential primary candidates (Dropouts from this list will continue to be included)
candidates <- c("Clinton, Hillary", "Sanders, Bernie", "Cruz, Ted",
                "Trump, Donald", "Rubio, Marco", "Kasich, John", "Bush, Jeb");
candidatesDisplay <- c("Hillary Clinton", "Bernie Sanders", "Ted Cruz", "Donald Trump", "Marco Rubio",
                       "John Kasich", "Jeb Bush")
candidateParties <- c(rep("dem", 2), rep("rep", 5))
candidatesUpper <- toupper(candidates); #unify on upper case here to simplify data cleanup
candidateIds <- c("P00003392", "P60007168", "P60006111", "P80001571", "P60006723", "P60003670", "P60008059");

x$can_nam <- toupper(x$can_nam); #all candidate names to upper case
x$pur <- tolower(x$pur); # all "purpose" strings to lower case
x$exp_amo <- as.numeric(gsub(',', '',gsub('\\$','',x$exp_amo))) # all dollar amounts to numeric
x$exp_amo <- round(x$exp_amo) # round to nearest dollar
x$exp_amo <- abs(x$exp_amo) # exp_amo should be positive, a few entries have neg numbers (likely typos)
temp <- dim(x)[1]
x <- subset(x, !is.na(exp_amo)) # remove negative values of exp_amo (probably should count these as 
                                # negative but they are technically spending, just ultimately on nothing 
                                # since they're reimbursed)

if (verbose) { temp <- writeLines(paste("Removed", temp - dim(x)[1], "records due to negative expense amounts.")); }

# takes a list of candidates and ids and returns a cleaned version of x with unified candidate
# names and an ID assigned to every row. For example, "Donald Trump", "Trump, Donald", "Trump, Donald J."
# "TRUMP, DONALD", and "TRUMP, DONALD J" will all be changed to "TRUMP, DONALD" for easier manipulation
# later.
unifyCanNames <- function(candidates, ids, x) {
  for (i in 1:length(candidates)) {
    print(paste("Cleaning: ", candidates[i]))
    canNam <- word(candidates[i], sep = fixed(","));
    canNamReg <- paste(".*", canNam, ".*", sep="");
    x$can_nam <- sub(canNamReg, candidates[i], x$can_nam);
    x$can_id[x$can_nam == candidates[i]] <- ids[i];
  }
  return(x)
}

# checks that the number of entries with a given candidate name is the same as the number of 
# entries with the matching candidate ID, validating the data cleanup done previously. This 
# will catch candidate lastname misspellings or mismatch between candidate ID and name. Any errors
# here indicate that some manual cleanup is needed before running the script.
validateNames <- function(candidates, ids) {
  for (i in 1:length(candidates)) {
    result <- length(subset(x, can_nam == candidates[i], select=can_nam)) == 
      length(subset(x, can_id == ids[i], select=can_nam));
    writeLines(paste("Validating candidate: ", candidates[i], "...\n", result));
    if (!result) {
      writeLines("Error: A candidate last name may be misspelled or there may be a mismatch between 
                 candidate name and ID in the database. Some manual cleanup or modification to 
                 the cleanup function may be required.\n")
    }
  }
}

x <- unifyCanNames(candidatesUpper, candidateIds, x);
validateNames(candidatesUpper, candidateIds);

# From the FEC:
# This file contains transactions from new notices and amendments. Transactions from a new notice 
# are not removed when that notice is amended. You will find duplicate transactions in this dataset. 
# The need for speed in delivering these data will mean that some of the usual review and modification 
# that we routinely do won’t be happening for these files.  For example, if filers submit the same 
# material more than once (e.g. by amending a previous filing, or by submitting a regular report that 
# includes information already submitted on 24 or 48 hour notices) we won’t be adjusting the files to 
# include only one version of the entries.  This means that analysis of these data will require some 
# work so that totals are not inflated.  Generally this can be done by sorting the records by filer 
# and date and targeted candidate with an examination of amendment indicator columns or report type 
# codes when the same dates and amounts appear.

# The following code gets the table of amendment codes, iterates through them finding the matching 
# amended records and removing them from the database

x$duplicate <- FALSE;
amn_ind_vals <- levels(as.factor(x$amn_ind))
starting_num_records <- dim(x)[1]

for (i in 1:(length(amn_ind_vals)-1)) {
  s <- subset(x, amn_ind == paste("A", i, sep=""))
  writeLines(paste("Found ", length(s$amn_ind), " amendment records of type ", levels(as.factor(s$amn_ind))))
  l <- levels(as.factor(s$prev_file_num))
  s <- subset(x, file_num %in% l)
  for (i in 1:length(levels(as.factor(s$amn_ind)))) {
    writeLines(paste("Found ", dim(subset(s, amn_ind==levels(as.factor(s$amn_ind))[i]))[1],
                     " matching amended records of type ", levels(as.factor(s$amn_ind))[i]))
    
  }
  if (verbose) {
    writeLines("Amended file numbers: \n", sep = "")
    print(l)
    writeLines("Amended file records table v\n")
    print(s[,c("can_nam", "amn_ind", "file_num", "prev_file_num", "exp_amo")])
  }
  x[x$file_num %in% l,]$duplicate <- TRUE;
  x <- subset(x, duplicate == FALSE)
  for (i in 1:length(levels(as.factor(s$amn_ind)))) {
    writeLines(paste("Removed ", dim(subset(s, amn_ind == levels(as.factor(s$amn_ind))[i]))[1], 
                     " records of type ", levels(as.factor(s$amn_ind))[i]))
  }
  writeLines(paste(rep("-", 40), sep="", collapse=""))
}

ending_num_records <- dim(x)[1]
writeLines(paste("Removed ", starting_num_records - ending_num_records, " total records"))

# categorize spending into buckets
categories <- c("production", "tv advertising", "staff", "calls", "emails", "social", 
                "radio advertising", "paper mailings", "newspaper advertising", "online advertising",
                "uncategorized media", "media buys", "other")

# find keywords in the "purpose" field and replace whole field with one of the categories
x$pur <- gsub(".*production.*", "production", x$pur);
x$pur <- gsub(".*development.*", "production", x$pur);
x$pur <- gsub(".*agency.*", "production", x$pur);
x$pur <- gsub(".*television.*", "tv advertising", x$pur);
x$pur <- gsub(".*tv.*", "tv advertising", x$pur);
x$pur <- gsub(".*cable.*", "tv advertising", x$pur);
x$pur <- gsub(".*staff.*", "staff", x$pur);
x$pur <- gsub(".*salary.*", "staff", x$pur);
x$pur <- gsub(".*travel.*", "staff", x$pur);
x$pur <- gsub(".*payroll.*", "staff", x$pur);
x$pur <- gsub(".*transport.*", "staff", x$pur);
x$pur <- gsub(".*call.*", "calls", x$pur);
x$pur <- gsub(".*phone.*", "calls", x$pur);
x$pur <- gsub(".*email.*", "emails", x$pur);
x$pur <- gsub(".*social.*", "social", x$pur);
x$pur <- gsub(".*radio.*", "radio advertising", x$pur);
x$pur <- gsub(".* mail.*", "paper mailings", x$pur);
x$pur <- gsub(".*ship.*", "paper mailings", x$pur);
x$pur <- gsub(".*print.*", "paper mailings", x$pur);
x$pur <- gsub(".*post.*", "paper mailings", x$pur);
x$pur <- gsub(".*mailing.*", "paper mailings", x$pur);
x$pur <- gsub(".*new.*", "newspaper advertising", x$pur);
x$pur <- gsub(".*online.*", "online advertising", x$pur);
x$pur <- gsub(".*web.*", "online advertising", x$pur);
x$pur <- gsub(".*mobile.*", "online advertising", x$pur);
x$pur <- gsub(".*facebook.*", "online advertising", x$pur);
x$pur <- gsub(".*digital.*", "online advertising", x$pur);
x$pur <- gsub(".*internet.*", "online advertising", x$pur);

# last catchall for media / ads that weren't categorized previously
x$pur <- gsub(".*media.*", "media buys", x$pur);
x$pur <- gsub(".*advertise.*", "uncategorized media", x$pur);
x$pur <- gsub(".*ad .*", "uncategorized media", x$pur);
x$pur <- gsub(".*ads .*", "uncategorized media", x$pur);

# everything else
x$pur[!(x$pur %in% categories[1:(length(categories)-1)])] <- "other";
levels(as.factor(x$pur));

par(las=2)
par(mar=c(5,10,4,2)) # increase y-axis margin.
barplot(table(as.factor(x$pur)), horiz=TRUE) # plots number of filings by category (not $ amount)

# select candidate rows from summary file using IDs
candidate_summaries <- subset(z, can_id %in% candidateIds)
writeLines("Found the following candidates in the Candidate Summary file:")
print(candidate_summaries[,c("can_id", "can_nam")])
candidate_summaries <- unifyCanNames(candidatesUpper, candidateIds, candidate_summaries);
validateNames(candidatesUpper, candidateIds);
writeLines("Cleaned candidate entries in the Candidate Summary file:")
print(candidate_summaries[,c("can_id", "can_nam")])

# initialize variable specifying which columns represent sums of other columns
total_summary_cols <- c("tot_con", "tot_loa", "tot_rec", "tot_loa_rep", "tot_con_ref", "tot_dis", 
                        "net_con", "net_ope_exp", "ind_con", "deb_owe_to_com", "deb_owe_by_com")

summary_revenue_cols <- c("ind_ite_con", "ind_uni_con", "ind_con", "par_com_con", "oth_com_con", 
                          "can_con", "tot_con", "tra_fro_oth_aut_com", "can_loa", "oth_loa", 
                          "tot_loa", "off_to_ope_exp", "off_to_fun", "off_to_leg_acc", "oth_rec", 
                          "tot_rec", "cas_on_han_beg_of_per", "deb_owe_to_com")

summary_rev_cols_disp <- c("Contributions over $200",
                           "Contributions under $200",
                           "Independent contributions",
                           "Party PAC contribution",
                           "Other PAC contribution",
                           "Candidate contribution",
                           "Total contributions",
                           "Other PAC contribution",
                           "Candidate loans",
                           "Other loans",
                           "Total loans",
                           "Offset to operating cost",
                           "Offset to fundraising cost",
                           "Offset to legal and accounting cost",
                           "Other revenues",
                           "Total revenues",
                           "Starting cash on hand",
                           "Debt owed to candidate committee")

summary_expenditure_cols <- c("ope_exp", "exe_leg_acc_dis", "fun_dis", "tra_to_oth_aut_com", 
                              "can_loa_rep", "oth_loa_rep", "tot_loa_rep", "ind_ref", "par_com_ref", 
                              "oth_com_ref", "tot_con_ref", "oth_dis", "tot_dis", "cas_on_han_clo_of_per", 
                              "deb_owe_by_com")

summary_exp_cols_disp <- c("Operating expenses",
                           "Exempt legal and accounting",
                           "Fundraising expenses",
                           "Transfer to other PACs",
                           "Candidate loan repayments",
                           "Other loan repayments",
                           "Total loan repayments",
                           "Individual refunds",
                           "Party PAC refunds",
                           "Other PAC refunds",
                           "Total PAC refunds",
                           "Other expenses",
                           "Total expenses",
                           "Cash on hand",
                           "Debt owed by candidate committee")

# TODO check that non-total columns add to over-all totals (will also be obvious in diagram if wrong)

# extract data and construct csv for generating sankey diagram
# CSV must have lines of the form source, target, value, shortname
# where: SOURCE is a source of income or the candidate doing the spending
#        TARGET is the candidate for income, or the category of expense for spending
#        VALUE  is the amount being earned or spent
#        SHORTNAME  is a short version of the node name for the candidate involved in the transaction

# initialize file with header row
file <- file("sankeydata.csv", "w")
write("source,target,value,shortname", file=file)
# iterate through candidates in candidate_summaries
for (i in 1:length(candidates)) {
  candidate_data <- subset(candidate_summaries, can_nam==candidatesUpper[i])
  # for columns that are revenues, create link entries
  for (val in setdiff(summary_revenue_cols, total_summary_cols)) {
    if (!is.na(candidate_data[,val])) {
      index <- match(val, summary_revenue_cols)
      write(paste0("\"", summary_rev_cols_disp[index], "\",\"",candidatesDisplay[i], "\",", candidate_data[,val]), file=file)
    }
  }
  # for columns that are spending, create link entries
  for (val in setdiff(summary_expenditure_cols, total_summary_cols)) {
    if (!is.na(candidate_data[,val])) {
      index <- match(val, summary_expenditure_cols)
      write(paste0("\"",candidatesDisplay[i], "\",\"", summary_exp_cols_disp[index], "\",",candidate_data[,val]), file=file)
    }
  }
}
close(file)

# calculate total spending on themselves for each candidate 
# selected from z and put into candidate summaries
self_spend_candidates_ordered <- NULL;
for (i in 1:length(candidates)) {
  self_spend_candidates_ordered[i] <- subset(candidate_summaries, 
                                             can_id==candidateIds[i], 
                                             tot_dis)
}
self_spend_candidates_ordered <- unlist(self_spend_candidates_ordered)
self_spend_avg <- round(sum(as.numeric(self_spend_candidates_ordered)) / length(self_spend_candidates_ordered))

# calculates total independent spending by category combined for all candidates
totalsByCategory <- function(sub, categories) {
  result <- categories
  for (i in 1:length(categories)) {
    s <- subset(sub, pur == categories[i])
    result[i] <- sum(s$exp_amo)
  }
  return(as.numeric(result))
}

total_spending_by_category <- totalsByCategory(x, categories)
barplot(total_spending_by_category, horiz=TRUE, names.arg = categories)

total_by_category_support <- totalsByCategory(subset(x, sup_opp == "Support"), categories)
total_by_category_against <- totalsByCategory(subset(x, sup_opp == "Oppose"), categories)

l <- list()
for (i in c(1, 3:11, 13)) {
  l[[categories[i]]] <- list("support"=total_by_category_support[i], 
                             "oppose"=-total_by_category_against[i])
}
json <- toJSON(l)
write(json, file="totals_by_category_no_tv.json")

# everything in one category vs. the two tv advertising-related categories
l <- list()
l[["everything else"]] <- list("support"=sum(total_by_category_support[c(1, 3:11, 13)]), 
                               "oppose"=-sum(total_by_category_against[c(1, 3:11, 13)]))
l[["tv advertising"]] <- list("support"=sum(total_by_category_support[c(2, 12)]),
                              "oppose"=-sum(total_by_category_against[c(2, 12)]))
json <- toJSON(l)
write(json, file="totals_tv_vs_all.json")

# total spend for candidates
total_spend_for_candidate <- c(1:length(candidates));
for (i in 1:length(candidatesUpper)) {
  s <- subset(x, can_nam == candidatesUpper[i] & sup_opp == "Support");
  total_spend_for_candidate[i] <- sum(s$exp_amo);
}

# total spend against candidates
total_spend_against_candidate <- c(1:length(candidates));
for (i in 1:length(candidatesUpper)) {
  s <- subset(x, can_nam == candidatesUpper[i] & sup_opp == "Oppose");
  total_spend_against_candidate[i] <- sum(s$exp_amo);
}

# total for + against combined for each candidate
total_spend_candidate <- c(1:length(candidates));
for (i in 1:length(candidatesUpper)) {
  s <- subset(x, can_nam == candidatesUpper[i]);
  total_spend_candidate[i] <- sum(s$exp_amo);
}

# total and average for and against for all candidates combined
tot_opp <- sum(total_spend_against_candidate)
tot_for <- sum(total_spend_for_candidate)
avg_opp <- round(tot_opp / 6)
avg_for <- round(tot_for / 6)
barplot(c(avg_opp, avg_for))

# create data frame of totals for and against by candidate (plus average)
# and write to JSON file, also includes total spending of candidate official committees 
# for reference
summary_df <- data.frame(candidate=c("Average",candidates),
                         name=c("Average", candidates),
                         support=c(avg_for, total_spend_for_candidate), 
                         oppose=c(-avg_opp, -total_spend_against_candidate),
                         party=c("none", candidateParties),
                         selfspend=c(self_spend_avg, self_spend_candidates_ordered))
list <- list()
for (i in 1:length(summary_df$candidate)) {
  list[[as.vector(summary_df$candidate)[i]]] <- summary_df[i,2:dim(summary_df)[2]];
}
json <- toJSON(list)
write(json, file="totals_for_and_against_by_candidate.json")

# plot total spend for, against, and total
if (verbose) {
  barplot(total_spend_against_candidate, names.arg = candidates)
  barplot(total_spend_for_candidate, names.arg = candidates)
  barplot(total_spend_candidate, names.arg = candidates)
}

# The following section calculates total spending for / against
# each candidate by category, and outputs that information as a json file

tot_sup_by_cat_by_can <- data.frame(matrix(ncol = length(categories)+1, nrow = 0));
colnames(tot_sup_by_cat_by_can) <- c("candidate_name", categories)
for (i in 1:length(candidates)) {
  s <- subset(x, can_nam == candidatesUpper[i] & sup_opp == "Support");
  tot_sup_by_cat_by_can[i,] <- c(candidates[i], totalsByCategory(s, categories));
}

tot_sup_by_cat_by_can_list <- list()
for (i in 1:length(candidates)) {
  tot_sup_by_cat_by_can_list[[candidates[i]]] <- tot_sup_by_cat_by_can[i, 2:14];
}
json <- toJSON(tot_sup_by_cat_by_can_list)
write(json, file="total_spend_support_candidate_by_category.json")

tot_opp_by_cat_by_can <- data.frame(matrix(ncol = length(categories)+1, nrow = 0));
colnames(tot_opp_by_cat_by_can) <- c("candidate_name", categories)
for (i in 1:length(candidates)) {
  s <- subset(x, can_nam == candidatesUpper[i] & sup_opp == "Oppose");
  tot_opp_by_cat_by_can[i,] <- c(candidates[i], totalsByCategory(s, categories));
}

tot_opp_by_cat_by_can_list <- list()
for (i in 1:length(candidates)) {
  tot_opp_by_cat_by_can_list[[candidates[i]]] <- tot_opp_by_cat_by_can[i, 2:14];
}
json <- toJSON(tot_opp_by_cat_by_can_list)
write(json, file="total_spend_against_candidate_by_category.json")


# Future work:
# calculate top 5 donors for and against per candidate and list the total amount donated