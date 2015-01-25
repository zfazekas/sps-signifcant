## --------------------------------------------------------------------------##
## Title:     Replication script for "All Votes are Equal? Significant 
##            Legislation and Party Competition in the Danish Folketing"
## Journal:   Scandinavian Political Studies
## Authors:   Martin Ejnar Hansen & Zoltan Fazekas
## Contact:   martin.hansen@brunel.ac.uk; zoltan.fazekas@gmail.com
## Accepted:  7 January 2015
## R version: 3.1.0 (ran on Mac OsX, tested on Windows machine as well)
## Notes:     Session info at the end of the script
##            Loads data from ./data/ folder
##            Tables and figures saved in ./output/ folder
##            Materials in paper (original run) ./inculded/ (not modified here)
## --------------------------------------------------------------------------##
## rm(list = ls())
## Loading packages used
library("ggplot2")
library("reshape2")
library("car")
library("scales")
library("dplyr")
library("grid")
library("stargazer")
library("arm")
library("lme4")
## --------------------------------------------------------------------------##
## Additional helper functions
## Plotting them
plotTheme <- function(base_size = 10, base_family = "sans"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(colour = "grey30"), 
    axis.ticks = element_line(size = 0.2), 
    axis.ticks.length = unit(0.3,"cm"), 
    axis.title.x = element_text(vjust = -1.5, family = "sans", face = "bold"), 
    axis.title.y = element_text(vjust = 0.75, 
      hjust = 0.5, angle = 90, family = "sans", face = "bold"), 
    title = element_text(vjust = 2), plot.margin = unit(c(1,1,1,0.7), "cm")     )
}
## Standard error of the mean
se <- function(x) sqrt(var(x, na.rm = T)/length(x))
## --------------------------------------------------------------------------##
## Loading votes data
## type    = code for the source of legislation proposal (to be merged later)
## to_election = number of days between vote and upcomming election
## length  = length of debate in column inches
## reg2    = legislature unique code
## sd..uaf = party behavior (1 = voted for, 0 = voted against)
##         = NA - not available, no expressed vote (treated as NA),
##           missing code if the party was NOT in the legislature
## sd..uaf = variable names are party name abbreviations
## Detailed information on type in SI Table A6
## Detailed information on party abbreviations in SI Table A1
votes <- read.csv("./data/dk-votes.csv", header = TRUE) 
## 8622 bills, 2 NA for length
## 56 bills with no available information on type (which domain)
## Please note: running our analysis without domain related controls
## and such keepint these 56 bills does not change the results 
## (an earlier version of the submission did that)
## --------------------------------------------------------------------------##
## Government data
## leg        = legislature code, used to link with votes data
## gov1..gov4 = party names for government parties (gov1 = gives PM??)
## toTheRight = ideological most right wing party (authors' coding)
## toTheLeft  = ideological most right wing party (authors' coding)
govs <- read.csv("./data/dk-govs.csv", sep = ",",
                 stringsAsFactors = FALSE,
                 header = TRUE)
## --------------------------------------------------------------------------##
## Seats data
## leg      = legislature code, used to link with votes and gov data
## period   = start-end of legislature
## election = national election date concluding the legislature
## sd..df   = number of seats for parties, NA = not in legislature
seats <- read.csv("./data/dk-seats.csv", sep = ",",
                  header = TRUE)
## Merge government and legislature related variables
govs <- merge(govs, seats, by = "leg")
## --------------------------------------------------------------------------##
## Legislation type/source data
## type    = legislation type (proposed by) code for merging
## newcat  = collapsed categories (grouped)
## catdesc = category description
legtype <- read.csv("./data/dk-type-code.csv", 
                    stringsAsFactors = FALSE,
                    header = TRUE)
## --------------------------------------------------------------------------##
## Data transformations, merging preparation seats
seats.m <- melt(seats, 
                id = c("leg", "period", "election"))
seats.m$party_name <- as.character(seats.m$variable)
seats.m$party_name[as.character(seats.m$variable) == "z" & 
                seats.m$period == "1998-01"] <- "fri"
## party name update for merging
names(seats.m)[5] <- "seats"
seats.m$merge_id <- paste(seats.m$period, seats.m$party_name,
                          sep = "-")
## head(seats.m$merge_id)
## Get total number of seats in each legislature
seats.m <- seats.m %>%
           group_by(period) %>%
           mutate(seats_total = sum(seats, na.rm = TRUE))
## Calculate seat share as proportion
seats.m$seat_share <- seats.m$seats/seats.m$seats_total
## head(seats.m)
## --------------------------------------------------------------------------##
## Data transformations, merging preparation voting data
votes$reg2 <- factor(votes$reg2)
## Create dichotomous variable for significant legislation
## Rule: larger or equal in length to the 80th percentile
## in the particular legislature in case
votes <- votes %>%
		     group_by(reg2) %>%
         mutate(long = as.numeric(length >= 
                quantile(length, na.rm = TRUE, 0.8)))
## Dropping unique number of legislation (not needed) and 
## using a new data.frame for analysis (votes kept untouched) 
dyads <- dplyr::select(votes, length, long, reg2, type, to_election, 
                      sd:uaf)
dyads$reg2 <- factor(dyads$reg2)
dyads$voteID <- paste(dyads$reg2, rownames(dyads), sep = "-")
## Reformatting data: long format
dyads <- melt(dyads, id = c("voteID", "type", "length", 
              "long", "reg2", "to_election"))
names(dyads)[7:8] <- c("party", "vote")
## Removing bills/votes where the type of legislation is not known
## Please note: keeping the full data wi
dyads <- filter(dyads, type != "#NULL!")
## Adding information of larger category/domain of the bills
dyads <- merge(dyads, legtype, by = "type") 

## Dropping those parties that were not in a particular legislature
## Hereby, we differentiate between those parties that had some NAs in
## votes, and those that had all votes as NA (not present in the leg.)
## There were cross referenced with parliament composition information
m1 <- dyads %.%
         group_by(reg2, party) %.%
         mutate(toDrop = sum(as.numeric(is.na(vote) == T)) == length(vote))
m1 <- filter(m1, toDrop == F)
m1$toDrop <- NULL
## Checks:
## dim(m1) ## 68167 votes
## length(unique(m1$voteID)) ## 8566 bills with all information available
## --------------------------------------------------------------------------##
## Adding government related variables
names(govs)[1] <- "reg2"
m1 <- merge(m1, govs[, 1:9], by = "reg2") ## done here, because "period"
## needed for nicer display below
## --------------------------------------------------------------------------##
## Creating distribution plots for length
dist_plot <- m1 %>%
            group_by(voteID, period) %>%
            summarise(length = mean(length))
## Main text, Figure 1
ggplot(dist_plot, aes(x = length)) +
    geom_histogram(fill = "grey50") +
    plotTheme() + xlim(0, 500) +
    theme(axis.line = element_blank()) +
    geom_segment(x = 0, xend = 500,
                 y = -Inf, yend = -Inf, size = 0.1) +
    geom_segment(x = -Inf, xend = -Inf,
                 y = 0, yend = 6000, size = 0.05) +
    xlab("debate length (column inches)") +
    ylab("count")
ggsave("./output/main-fig-1.pdf", 
       height = 5, width = 5, units ='in')
## SI, Figure 1 - distribution, but for each legislature separately
ggplot(dist_plot, aes(x = length)) +
    geom_histogram(fill = "grey50") +
    facet_wrap(~period, scales = "free") +
    plotTheme() + 
    xlab("debate length (column inches)") +
    ylab("count")
ggsave("./output/si-fig-1.pdf", 
       height = 8, width = 8, units ='in')
## --------------------------------------------------------------------------##
## Adding dichotomous variable (at the vote x party format) 
## for major government party (1 = YES)
## Whether party in government
## Whether party is extreme right or extreme left
## Whether party is extreme (simple OR for the previous two)
m1$majorGov <- as.numeric(m1$party == m1$gov1)
m1$inGov <- as.numeric(m1$party == m1$gov1 | 
    m1$party == m1$gov2 | 
    m1$party == m1$gov3 | 
    m1$party == m1$gov4)
m1$extRight <- as.numeric(m1$party == m1$toTheRight)
m1$extLeft <- as.numeric(m1$party == m1$toTheLeft)
m1$isExtreme <- as.numeric(m1$extRight == 1 | m1$extLeft == 1)
## Adding vote specific variables
## Was the vote unanious?
## How large the majority for passing it
## Total number of parties in legislature
m1 <- as.data.frame(m1) %>% 
        group_by(voteID) %>%
        mutate(unanimous = sum(vote, na.rm = T) == 
                                sum(as.numeric(is.na(vote) == F)),
               majPassed = sum(vote == 1, na.rm = T),
               totalPart = n())
## --------------------------------------------------------------------------##
## Summary table: SI Table A3: Unanimous bills
## Stacked data format, so only one party used for descriptives, 
## one that was in all legislatures 
stable1 <- as.data.frame(m1) %>%
           filter(party == "sd") %>% 
           group_by(period) %>%
           summarise(bills = n(),
                     unanimous = round(100*mean(as.numeric(unanimous), 
                                                na.rm = T), 2))
unanimoustab <- as.data.frame(m1) %>%
                group_by(voteID) %>%
                summarise(period = unique(period),
                          unanimous = as.numeric(unique(unanimous)),
                          sig = unique(long))
unanimoustab <- as.data.frame(unanimoustab) %>%
                filter(!is.na(sig)) %>%
                group_by(period, sig) %>%
                summarise(unanimous = round(100*mean(unanimous), 2))

unanimoustab <- dcast(unanimoustab, period ~ sig, value.var = "unanimous")
stable1 <- merge(stable1, unanimoustab, by = "period")
write.csv(stable1, file = "./output/si-table-3.csv", row.names = F)
## --------------------------------------------------------------------------##
## Additional variables: number of parties voting as the major government party
## Which parties are in government (as string list)
m1 <- as.data.frame(m1) %>%
         group_by(voteID) %>%
         mutate(votedGov = as.numeric(vote == vote[majorGov == 1]),
                govParts = unique(paste(party[inGov == 1], 
                                  collapse = ", ")))
## Number of extreme parties voting with the government
## Number of non-government parties voting with the government
## Total number of parties not in government
m1 <- as.data.frame(m1) %>%
         group_by(voteID) %>%
         mutate(noExtVoted    = sum(isExtreme == 1 & votedGov == 1, 
                                    na.rm = TRUE),
                noNonGovVoted = sum(inGov == 0 & votedGov == 1, 
                                    na.rm = TRUE),
                noNonGov      = sum(inGov == 0, na.rm = TRUE))

## For each legislature, dropping > 99th percentile in length 
## such as Finance bill, for example
rc <- as.data.frame(m1) %>%
      group_by(period) %>%
      filter(length <= quantile(length, 0.99, na.rm = T))
rc$merge_id <- paste(rc$period, rc$party, sep = "-")
## Adding legislature-party related quantities to the data
rc <- merge(rc, seats.m[, c("merge_id", "party_name", 
                            "seats", "seats_total", "seat_share")],
                by = "merge_id")
## Adding government size related variables
rc <- rc %>%
        group_by(voteID) %>%
        mutate(gov_size = sum(seats[inGov == 1]))
rc$gov_share <- rc$gov_size/rc$seats_total

vote_data <- as.data.frame(rc) %>%
                group_by(voteID) %>%
                summarise(period = unique(period),
                  cycle          = unique(to_election),
                  length         = unique(length),
                  long           = unique(long),
                  gov_share      = unique(gov_share),
                  cat            = unique(catdesc),
                  unanimous      = unique(unanimous),
                  voted          = unique(majPassed),
                  total_parties  = unique(totalPart),
                  total_nogov    = unique(noNonGov),
                  nogov_voted    = unique(noNonGovVoted),
                  ext_voted      = unique(noExtVoted))
vote_data$total_ext <- 2 ## There are always 2 extreme parties (left and right)
## 0-1 government share variable (it does not vary within legislatures)
vote_data$gov_share_std <- (vote_data$gov_share - min(vote_data$gov_share))/
        (max(vote_data$gov_share) - min(vote_data$gov_share)) 
## Rescaling variables
vote_data <- data.frame(vote_data) %>%
    group_by(period) %>%
    mutate(length_std = (length - mean(length, na.rm = T)) / 
                        (2*sd(length, na.rm = T)),
           cycle_01   = (cycle - min(cycle, na.rm = T)) / 
                        (max(cycle, na.rm = T) - min(cycle, na.rm = T)) 
           )
## dim(vote_data)
## --------------------------------------------------------------------------##
## Multivariate models
table(vote_data$unanimous) ## total possible non-unanimous: 4485
## Note: > 0.99 already dropped
## Majority model: with continous (rescaled) significance predictor
majority_model <- glmer(voted / total_parties ~ 1 + length_std + 
                        cat + cycle_01 +
                        gov_share_std +
                        (1 + length_std | period),
                        weight = total_parties,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(majority_model)
## Majority model: with dichotomous significance predictor
majority_model01 <- glmer(voted / total_parties ~ long + cycle_01 + cat +
                        gov_share_std +
                        (1 + long| period),
                        weight = total_parties,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(majority_model01)
## Opposition models: proportion of non-government parties voting
## with the government
opp_model <- glmer(nogov_voted / total_nogov ~ length_std + cycle_01 + cat +
                        gov_share_std +
                        (1 + length_std | period),
                        weight = total_nogov,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(opp_model)
opp_model01 <- glmer(nogov_voted / total_nogov ~ long + cycle_01 + cat +
                        gov_share_std +
                        (1 + long | period),
                        weight = total_nogov,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(opp_model01)
## Extreme party models: proportion of non-government parties voting
## with the government
ext_model <- glmer(ext_voted / total_ext ~ length_std + cycle_01 + cat +
                        gov_share_std +
                        (1 + length_std | period),
                        weight = total_ext,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(ext_model)
ext_model01 <- glmer(ext_voted / total_ext ~ long + cycle_01 + cat +
                        gov_share_std +
                        (1 + long | period),
                        weight = total_ext,
                        family = binomial,
                        data = filter(vote_data, unanimous == FALSE))
summary(ext_model01)

## Main text, Table 2
## Labels need to be updated for final table in paper
stargazer(majority_model, majority_model01,
          opp_model, opp_model01, 
          ext_model, ext_model01, 
          out = "./output/main-table-2.htm")
## --------------------------------------------------------------------------##
## Additional figures for SI, with domain specific plots
## Data preparation
## head(rc)
sumtabletype <- as.data.frame(rc) %>%
        group_by(voteID) %>%
        summarise(period   = unique(period),
                 category  = unique(catdesc),
                 sig       = unique(long),
                 unanimous = unique(unanimous))
## dim(filter(sumtabletype, unanimous == F)) ## just checking
sumtabletype <- as.data.frame(sumtabletype) %>%
                filter(unanimous == FALSE) %>%
                group_by(period, category) %>%
                summarise(sig = mean(sig))
## --------------------------------------------------------------------------##
## SI Figure A4
ggplot(sumtabletype,
            aes(x = period, y = sig, group = 1)) + 
    geom_line() + geom_point() + plotTheme() +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Proportion of Significant bills") +
    xlab("") + geom_hline(yintercept = 0.5, alpha = 0.2) +
    facet_grid(category~.) +
    theme(strip.text.y = element_text(size = 8, angle = 360, vjust = 0.6, hjust = 0)) +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    geom_hline(yintercept = -Inf)
ggsave("./output/si-fig-4.pdf", 
       height = 6, width = 8, units ='in')
## --------------------------------------------------------------------------##
votemargin <- as.data.frame(rc) %>%
                filter(unanimous == FALSE) %>%
                group_by(period, long, catdesc) %>%
                summarise(passed = mean(majPassed/totalPart, na.rm = T),
                          passedSE = se(majPassed/totalPart),
                          nonGov = mean(noNonGovVoted/noNonGov, na.rm = T),
                          nonGovSE = se(noNonGovVoted/noNonGov),
                          extVote = mean(noExtVoted/2, na.rm = T),
                          extVoteSE = se(noExtVoted/2)
                    )
## --------------------------------------------------------------------------##
## SI Figure A5
ggplot(votemargin,
            aes(x = period, y = passed, ymin = passed - 1.96*passedSE,
                ymax = passed + 1.96*passedSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = passed - 1.96*passedSE,
                ymax = passed + 1.96*passedSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average majority/minority on votes") +
    xlab("") +
    facet_grid(catdesc~.) +
    theme(strip.text.y = element_text(size = 8, angle = 360, hjust = 0)) +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position = c(0.13, 0.51)) +
    theme(legend.background = element_rect(fill=alpha('white', 0.1))) +
    geom_hline(yintercept = -Inf)
ggsave("./output/si-fig-5.pdf", 
       height = 6, width = 8, units ='in')
## --------------------------------------------------------------------------##
## SI Figure A6
ggplot(votemargin,
            aes(x = period, y = nonGov, ymin = nonGov - 1.96*nonGovSE,
                ymax = nonGov + 1.96*nonGovSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = nonGov - 1.96*nonGovSE,
                ymax = nonGov + 1.96*nonGovSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average proportion of opposition parties voting with Government") +
    xlab("") +
    facet_grid(catdesc~.) +
    theme(strip.text.y = element_text(size = 8, angle = 360, hjust = 0)) +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position = c(0.16, 0.52)) +
    theme(legend.background = element_rect(fill=alpha('white', 0.1))) +
    geom_hline(yintercept = -Inf)
ggsave("./output/si-fig-6.pdf", 
       height = 6, width = 8, units ='in')
## --------------------------------------------------------------------------##
## SI Figure A7
ggplot(votemargin,
            aes(x = period, y = extVote, ymin = extVote - 1.96*extVoteSE,
                ymax = extVote + 1.96*extVoteSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = extVote - 1.96*extVoteSE,
                ymax = extVote + 1.96*extVoteSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average proportion of extreme parties voting with Government") +
    xlab("") +
    facet_grid(catdesc~.) +
    theme(strip.text.y = element_text(size = 8, angle = 360, hjust = 0)) +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position = c(0.15, 0.52)) +
    theme(legend.background = element_rect(fill=alpha('white', 0.1))) +
    geom_hline(yintercept = -Inf)
ggsave("./output/si-fig-7.pdf", 
       height = 6, width = 8, units ='in')
## --------------------------------------------------------------------------##
## SI Figure A3
ptype <- as.data.frame(rc) %>%
        filter(unanimous == FALSE & inGov == 0) %>%
        group_by(period, long, catdesc, party) %>%
        summarise(votedGov = mean(votedGov, na.rm = T))
parties <- unique(ptype$party)
for (i in 1:length(parties)){
    temp <- filter(ptype, party == parties[i])
    p <- ggplot(temp, aes(x = period, y = votedGov, 
                                        ymin = 0, ymax = votedGov, 
                                        group = as.factor(long), 
                                        colour = as.factor(long))) + 
        geom_pointrange(position = position_dodge(width = 0.25)) + 
        facet_grid(catdesc~.) +
        scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
        plotTheme() +
        theme(axis.text.x = element_text(angle = 90)) +
        ylab("Average proportion voted with Government") +
        xlab("") +
        facet_grid(catdesc~.) +
        theme(strip.text.y = element_text(size = 8, angle = 360, hjust = 0)) +
        theme(strip.background = element_rect(fill = "white", 
                                colour = "white")) +
        theme(legend.position = "bottom") +
        labs(title = paste("Party", parties[i], sep = ": "))
    ggsave(p, file = paste0("./output/si-fig-3-", parties[i], ".pdf"),
           height = 6, width = 7)
}
## --------------------------------------------------------------------------##
## Main text, Figures 2-4
votemargin <- as.data.frame(m1) %>%
                filter(unanimous == FALSE) %>%
                group_by(period, long) %>%
                summarise(passed = mean(majPassed/totalPart, na.rm = T),
                          passedSE = se(majPassed/totalPart),
                          nonGov = mean(noNonGovVoted/noNonGov, na.rm = T),
                          nonGovSE = se(noNonGovVoted/noNonGov),
                          extVote = mean(noExtVoted/2, na.rm = T),
                          extVoteSE = se(noExtVoted/2)
                    )
ggplot(votemargin,
            aes(x = period, y = passed, ymin = passed - 1.96*passedSE,
                ymax = passed + 1.96*passedSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = passed - 1.96*passedSE,
                ymax = passed + 1.96*passedSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average majority/minority on votes") +
    xlab("") +
    theme(legend.position = c(0.9, 0.15))
ggsave("./output/main-fig-2.pdf", 
       height = 6, width = 8, units ='in')
ggplot(votemargin,
            aes(x = period, y = nonGov, ymin = nonGov - 1.96*nonGovSE,
                ymax = nonGov + 1.96*nonGovSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = nonGov - 1.96*nonGovSE,
                ymax = nonGov + 1.96*nonGovSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average proportion of opposition parties voting with Government") +
    xlab("") +
    theme(legend.position = c(0.9, 0.15))
ggsave("./output/main-fig-3.pdf", 
       height = 6, width = 8, units ='in')
ggplot(votemargin,
            aes(x = period, y = extVote, ymin = extVote - 1.96*extVoteSE,
                ymax = extVote + 1.96*extVoteSE, group = as.factor(long), colour = as.factor(long))) + 
    geom_line() + geom_point() + geom_smooth(data = votemargin, aes(x = period, ymin = extVote - 1.96*extVoteSE,
                ymax = extVote + 1.96*extVoteSE), stat = "identity", 
                alpha = 0.5) + plotTheme() +
    scale_colour_manual("", values = c("0" = "darkblue", "1" = "red"),
                            labels = c("0" = "Not significant", "1" = "Significant")) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Average proportion of extreme parties voting with Government") +
    xlab("") +
    theme(legend.position = c(0.9, 0.15))
ggsave("./output/main-fig-4.pdf", 
       height = 6, width = 8, units ='in')
## --------------------------------------------------------------------------##
## SI Figure A2:
m2 <- as.data.frame(m1) %>%
      filter(unanimous == FALSE) %>%
      filter(inGov == 0) %>%
      group_by(period, party, long) %>%
      summarise(votedGov = mean(votedGov, na.rm = T), isExtreme = unique(isExtreme))

legs <- unique(m2$period)
for (i in 1:length(legs)){
    p <- ggplot(subset(m2, period == legs[i]), 
                aes(x = party, y = votedGov, 
                    shape = as.factor(isExtreme), 
                    colour = as.factor(long))) + 
            geom_point(position = position_dodge(width = 0.25)) + 
            plotTheme() + 
            xlab("") + ylab("Proportion voted with the government") + 
            scale_colour_manual("", values = c("0" = "black", 
                                                "1" = "red"), 
                                    labels = c("0" = "Not significant", 
                                                "1" = "Significant")) + 
            scale_shape_manual("", values = c("0" = 1, "1" = 2), 
                                   labels = c("0" = "Opposition party", 
                                        "1" = "Extreme opposition party")) + 
            ##theme(legend.position = "bottom") + 
            labs(title = paste("Legislature", legs[i], sep = ": "))
         ggsave(p, file = paste0("./output/si-fig-2-", legs[i], ".pdf"),
           height = 6, width = 7)
}
## --------------------------------------------------------------------------##
## SI Tables A4-A5 data preparation
## Data preparation for Figure 5, Main text
m1d <- dcast(m2, period + party + isExtreme ~ long, value.var = "votedGov")
names(m1d)[4:5] <- c("nSig", "sig")
m1d <- m1d %>%
       group_by(period) %>%
       summarise(pMoreOver = round(sum(sig - nSig > 0)/length(sig), 2),
        moreOver = paste(party[sig - nSig > 0], collapse = ", "),
        lessOver = paste(party[sig - nSig < 0], collapse = ", "),
        closestNSig = paste(party[nSig == max(nSig)], collapse = ","),
        closestSig = paste(party[sig == max(sig)], collapse = ","),
        extremes = sum(sig - nSig > 0 & isExtreme == 1))

govs$npart <- rowSums(govs[, 2:5] != "")
legdata <- merge(govs, seats.m, by = "period")
legdata$ingov <- 0
legdata$ingov[legdata$gov1 == legdata$variable |
    legdata$gov2 == legdata$variable |
    legdata$gov3 == legdata$variable |
    legdata$gov4 == legdata$variable] <- 1
legdata <- as.data.frame(legdata) %>%
         group_by(period) %>%
         summarise(govSize  = sum(seats[ingov == 1], na.rm = T)/
                              sum(seats, na.rm = T),
                   nGovPart = mean(npart))

votemargin <- merge(votemargin, legdata, by = "period")
votemargin.m <- melt(votemargin[, c(1, 2, 5, 7, 9)], 
                     id = c("period", "long", "govSize"))

votemargin.m$longS <- "Not significant"
votemargin.m$longS[votemargin.m$long == 1] <- "Significant"
votemargin.m$variableS <- "Opposition parties voting with government"
votemargin.m$variableS[votemargin.m$variable == "extVote"] <- "Extreme parties voting with government"
## --------------------------------------------------------------------------##
## Main text, Figure 5
ggplot(votemargin.m, aes(x = govSize, y = value, group = as.factor(longS), colour = as.factor(longS))) + geom_point() + plotTheme() + facet_wrap(~variableS) + stat_smooth(aes(fill = as.factor(longS)), alpha = 0.25, method = "lm") + xlab("Government seat share") + ylab("Proportion") +
    scale_colour_manual("", values = c("Not significant" = "darkblue", "Significant" = "darkred")) +
    scale_fill_manual("", values = c("Not significant" = "darkblue", "Significant" = "darkred"))
ggsave("./output/main-fig-5.pdf", 
       height = 5, width = 7, units ='in')
## --------------------------------------------------------------------------##
m1d <- merge(m1d, legdata[, 1:2], by = "period")
m1d$govSize <- round(m1d$govSize, 2)
govs$govParts <- apply(govs[, 2:5], 1, function (x) paste(x, collapse = ", "))
m1d <- merge(m1d, govs[, c(8,27)], by = "period")

## --------------------------------------------------------------------------##
## SI Table A4
write.csv(dplyr::select(m1d, period, govParts, closestNSig, closestSig),
          file = "./output/si-tab-4.csv",
          row.names = FALSE)
## --------------------------------------------------------------------------##
## SI Table A5
## Note, pMoreOver moved after lessOver in the final table
write.csv(dplyr::select(m1d, period:lessOver, govSize, govParts),
          file = "./output/si-tab-5.csv",
          row.names = FALSE)
## --------------------------------------------------------------------------##
## SI Table A2

si_a2 <- 
votes %>%
  group_by(reg2) %>%
  summarise(mu_length = mean(length, na.rm = TRUE),
            sd_length = sd(length, na.rm = TRUE),
            long_thr  = quantile(length, 0.8, na.rm = TRUE),
            non_sig   = sum(length < 
                            quantile(length, 0.8, na.rm = TRUE), 
                            na.rm = TRUE),
            sig       = sum(length >= 
                            quantile(length, 0.8, na.rm = TRUE),
                            na.rm = TRUE)
            )
names(si_a2)[1] <- "leg"
seats$npart <- rowSums(!is.na(seats[, 4:19])) - 
               rowSums(seats[, 4:19] == 0, na.rm = TRUE)
write.csv(merge(si_a2, seats[, c("leg", "period", "npart")], by = "leg"),
      file = "./output/si-table-2.csv", row.names = FALSE)

# > sessionInfo()
# R version 3.1.0 (2014-04-10)
# Platform: x86_64-apple-darwin13.1.0 (64-bit)

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#  [1] arm_1.7-03    lme4_1.1-7    Rcpp_0.11.3   Matrix_1.1-4  MASS_7.3-33   stargazer_5.1 dplyr_0.2    
#  [8] scales_0.2.4  car_2.0-20    reshape2_1.4  ggplot2_1.0.0

# loaded via a namespace (and not attached):
#  [1] abind_1.4-0      assertthat_0.1   coda_0.16-1      colorspace_1.2-4 digest_0.6.4     gtable_0.1.2    
#  [7] labeling_0.2     lattice_0.20-29  magrittr_1.0.1   minqa_1.2.3      munsell_0.4.2    nlme_3.1-117    
# [13] nloptr_1.0.0     nnet_7.3-8       parallel_3.1.0   plyr_1.8.1       proto_0.3-10     splines_3.1.0   
# [19] stringr_0.6.2    tools_3.1.0   