# Analysis of Kansas Judicial Retention Elections and Constitutional Amendment, Nov. 2016
# efg, 10 Nov 2016
# Adapted from script from 10 Nov 2010.

# Read file 2016-Kansas-General-Election-YYYY-MM-DD-HHMM.txt for analysis.

library(maps)

basedir <- "E:/2016/R/ScreenScrape-KS-SOS/"                                              #####

################################################################################
### Process screen-scraped data

d <- read.delim(paste0(basedir, "2016-Kansas-General-Election-2016-11-10-2040.txt"), as.is=TRUE)  #####

get.candidate <- function(YesCandidate, NoCandidate, ShortName)
{
  Yes <- d[d$Candidate == YesCandidate, ][,c(1,4)]
  colnames(Yes) <- c("County", paste0(ShortName, ".Yes"))
  Yes[,2] <- as.numeric(Yes[,2])

  No  <- d[d$Candidate == NoCandidate, ][,c(1,4)]
  colnames(No) <- c("County", paste0(ShortName, ".No"))
  No[,2] <- as.numeric(No[,2])

  Both <- merge(Yes, No)

  Both$PercentYes <- round(100 * Both[,2] / (Both[,2] + Both[,3]), 1)
  colnames(Both)[4] <- paste0(ShortName, ".PercentYes")

  Both
}

################################################################################
### Kansas Supreme Court

# Strings observed in file from screen scraping.  Will change every election.
# Note:  Carefully observe space inconsistencies in the online SOS page.

Carol.Beier   <- get.candidate("Carol A. Beier -  YES",
                               "Carol A. Beier - NO",   "Beier")
Dan.Biles     <- get.candidate("Dan Biles -  YES",
                               "Dan Biles - NO",        "Biles")
Lawton.Nuss   <- get.candidate("Lawton R. Nuss -  YES",
                               "Lawton R. Nuss - NO",   "Nuss")
Marla.Luckert <- get.candidate("Marla Luckert -  YES",
                               "Marla Luckert - NO",    "Luckert")
Caleb.Stegall <- get.candidate("Caleb Stegall -  YES",
                               "Caleb Stegall - NO",    "Stegall")

# Merge all into single data.frame
Supreme.Court <- merge(Carol.Beier, merge(Dan.Biles, merge(Lawton.Nuss, merge(Marla.Luckert, Caleb.Stegall))))

# Summary table showing all Supreme Court justices
write.csv(Supreme.Court, file=paste(basedir, "Table-Kansas-Supreme-Court-Retention-Election-2016.csv"), row.names=FALSE) #####

# Counties voting "No"
Carol.Beier[Carol.Beier$Beier.Yes <= Carol.Beier$Beier.No, ]
Dan.Biles[Dan.Biles$Biles.Yes < Dan.Biles$Biles.No, ]
Lawton.Nuss[Lawton.Nuss$Nuss.Yes < Lawton.Nuss$Nuss.No, ]
Marla.Luckert[Marla.Luckert$Luckert.Yes < Marla.Luckert$Luckert.No, ]
Caleb.Stegall[Caleb.Stegall$Stegall.Yes < Caleb.Stegall$Stegall.No, ]

# Write files with county results (sorted by lowest to highest percent yes)
write.csv(Carol.Beier[order(Carol.Beier$Beier.PercentYes), ],
          file=paste0(basedir, "Table-Carol-Beier-2016.csv"), row.names=FALSE)
write.csv(Dan.Biles[order(Dan.Biles$Biles.PercentYes), ],
          file=paste0(basedir, "Table-Dan-Biles-2016.csv"), row.names=FALSE)
write.csv(Lawton.Nuss[order(Lawton.Nuss$Nuss.PercentYes), ],
          file=paste0(basedir, "Table-Lawton-Nuss-2016.csv"), row.names=FALSE)
write.csv(Marla.Luckert[order(Marla.Luckert$Luckert.PercentYes), ],
          file=paste0(basedir, "Table-Marla-Luckert-2016.csv"), row.names=FALSE)
write.csv(Caleb.Stegall[order(Caleb.Stegall$Stegall.PercentYes), ],
          file=paste0(basedir, "Table-Caleb-Stegall-2016.csv"), row.names=FALSE)

################################################################################
### Kansas Constitutional Amendment

Amendment <- get.candidate("Constitutional Amendment -  YES",
                           "Constitutional Amendment - NO", "Amendment2016")

Amendment <- Amendment[order(Amendment$Amendment2016.PercentYes),]
write.csv(Amendment, file=paste(basedir, "Kansas-Constitutional-Amendment-2016.csv"), row.names=FALSE)  #####

################################################################################
### Maps

CapWords <- function(s, strict = FALSE)
{
    cap <- function(s) paste(toupper(substring(s,1,1)),
                  {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Online map showing county names and abbreviations

county.abbr <- c(
  "AL","AN","AT","BA","BT","BB","BR","BU","CS","CQ","CK","CN","CA","CY",
  "CD","CF","CM","CL","CR","DC","DK","DP","DG","ED","EK","EL","EW","FI",
  "FO","FR","GE","GO","GH","GT","GY","GL","GW","HM","HP","HV","HS","HG",
  "JA","JF","JW","JO","KE","KM","KW","LB","LE","LV","LC","LN","LG","LY",
  "MP","MN","MS","ME","MI","MC","MG","MR","MT","NM","NO","NS","NT","OS",
  "OB","OT","PN","PL","PT","PR","RA","RN","RP","RC","RL","RO","RH","RS",
  "SA","SC","SG","SW","SN","SD","SH","SM","SF","ST","SV","SU","TH","TR",
  "WB","WA","WS","WH","WL","WO","WY")

m <- map('county', 'kansas', plot=FALSE)
county.names <- CapWords(sub("kansas,","", m$names))
county.names[57] <- "McPherson"   # Force this fix

rownames(Supreme.Court) <- Supreme.Court$County

# Put in FIPS County order ("McPherson" comes before "Marion")
Supreme.Court <- Supreme.Court[county.names,]

plot.NO.counties <- function (ShortName, LongName, YesVotes, NoVotes,
                              MapTitle="Counties Voting NOT to Retain")
{
  Won <- (YesVotes > NoVotes)
  Color <- c("blue", "white")[1+Won]   # 1=Won, 2=Lost
  County <- county.abbr
  County[Won] <- ""

  png(paste(basedir, "Map-", ShortName, ".png", sep=""), width=540, height=360)

  m <- map('county', 'kansas', fill=T, col=Color)
  map.text("county","kansas",
    labels=County, cex=1.15, add=TRUE, col="white")
  mtext(paste(105-sum(Won), MapTitle, LongName), cex=1.5)

  mtext("Source:  Kansas Secretary of State, Online Election Results, 11/8/2016",    #####
    BOTTOM<-1, adj=0, line=0.5, cex=0.75)
  mtext(expression(italic("Kansas Meadowlark")), 1, adj=1, line=0.5, col="blue")

  dev.off()
}

plot.NO.counties("Beier",  "Carol Beier",   Supreme.Court$Beier.Yes,   Supreme.Court$Beier.No)
plot.NO.counties("Biles",  "Dan Biles",     Supreme.Court$Biles.Yes,   Supreme.Court$Biles.No)
plot.NO.counties("Nuss",   "Lawton Nuss",   Supreme.Court$Nuss.Yes,    Supreme.Court$Nuss.No)
plot.NO.counties("Luckert","Marla Luckert", Supreme.Court$Luckert.Yes, Supreme.Court$Luckert.No)
plot.NO.counties("Stegall","Caleb Stegall", Supreme.Court$Stegall.Yes, Supreme.Court$Stegall.No)

plot.NO.counties("Amendment","Constitutional Amendment",
                 Amendment$Amendment2016.Yes, Amendment$Amendment2016.No,
                 MapTitle="Counties Voting Against")

