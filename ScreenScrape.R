# Screen scrape election results from Kansas Secretary of State's site.
# efg, 9 Nov 2016
# Adapted from script from 9 Nov 2010.

# Screen scrape the 105 county pages and extract election results.
# Write file 2016-Kansas-General-Election-YYYY-MM-DD-HHMM.txt for analysis.

library(gdata)   # trim
library(XML)     # htmlTreeParse

Kansas.Counties <- c(      # Names of 105 Kansas counties
  "Allen",      "Anderson",     "Atchison",    "Barber",    "Barton",
  "Bourbon",    "Brown",        "Butler",      "Chase",     "Chautauqua",
  "Cherokee",   "Cheyenne",     "Clark",       "Clay",      "Cloud",
  "Coffey",     "Comanche",     "Cowley",      "Crawford",  "Decatur",
  "Dickinson",  "Doniphan",     "Douglas",     "Edwards",   "Elk",
  "Ellis",      "Ellsworth",    "Finney",      "Ford",      "Franklin",
  "Geary",      "Gove",         "Graham",      "Grant",     "Gray",
  "Greeley",    "Greenwood",    "Hamilton",    "Harper",    "Harvey",
  "Haskell",    "Hodgeman",     "Jackson",     "Jefferson", "Jewell",
  "Johnson",    "Kearny",       "Kingman",     "Kiowa",     "Labette",
  "Lane",       "Leavenworth",  "Lincoln",     "Linn",      "Logan",
  "Lyon",       "Marion",       "Marshall",    "McPherson", "Meade",
  "Miami",      "Mitchell",     "Montgomery",  "Morris",    "Morton",
  "Nemaha",     "Neosho",       "Ness",        "Norton",    "Osage",
  "Osborne",    "Ottawa",       "Pawnee",      "Phillips",  "Pottawatomie",
  "Pratt",      "Rawlins",      "Reno",        "Republic",  "Rice",
  "Riley",      "Rooks",        "Rush",        "Russell",   "Saline",
  "Scott",      "Sedgwick",     "Seward",      "Shawnee",   "Sheridan",
  "Sherman",    "Smith",        "Stafford",    "Stanton",   "Stevens",
  "Sumner",     "Thomas",       "Trego",       "Wabaunsee", "Wallace",
  "Washington", "Wichita",      "Wilson",      "Woodson",   "Wyandotte")

basedir <- "E:/2016/R/ScreenScrape-KS-SOS/"                               #####

scrape.county <- function (county)
{
  url <- paste0("http://www.sos.ks.gov/ent/", county, ".html")            #####
  xml <- htmlTreeParse(url, useInternal=TRUE)

  x <- unlist(xpathApply(xml, "//table[@width='500']/tr/td/table/tr", xmlValue))

  # Cleanup
  x <- gsub("\r\n", "|", x)
  x <- gsub("\\|\\|View Map", "", x)
  x <- gsub("\\| \\|", "", x)
  x <- gsub("\\|\\| ", "", x)
  x <- gsub("\\&nbsp\\&nbsp", "", x)
  x <- gsub("\\\"", "", x)

  contest <- ""
  for (i in 1:length(x))
  {
    if (length(grep("County Precincts Reporting:", x[i])) > 0)
    {
      candidate <-  unlist(strsplit(x[i], "County Precincts Reporting:"))[1]
    } else {
        raw <- trim(unlist(strsplit(x[i], "\\|")))
        raw <- gsub(",", "", raw)  # remove commas from numbers
        raw <- gsub("%", "", raw)  # remove percent sign
        line <- c(county, candidate, raw)

        # Separate fields with tab
        cat( paste(line, collapse="\t"), "\n", file=outfile)
    }
  }
}

outfile <- file(paste0(basedir, "2016-Kansas-General-Election-",          #####
                       format(Sys.time(),"%Y-%m-%d-%H%M"), ".txt"), "w")

# Header for tab-delimited file
line <- "County\tContest\tCandidate\tCountyVotes\tCountyPercent\tStateVotes\tStatePercent"
cat( line, "\n", file=outfile)

format(Sys.time(),"%Y-%m-%d-%H%M")

for (i in 1:length(Kansas.Counties))
{
  county <- Kansas.Counties[i]
  cat(county, "\n")
  flush.console()
  scrape.county(county)
}

format(Sys.time(),"%Y-%m-%d-%H%M")

close(outfile)
