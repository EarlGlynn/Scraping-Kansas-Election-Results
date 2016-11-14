# Scraping-Kansas-Election-Results
Screen scraping Kansas statewide election results to produce maps not provided by Kansas Secretary of State

The Kansas Secretary of State publishes election results online but for some reason does not produce statewide summaries of judicial retention or constitutional amendment contests.

The KS SOS election results web page can be scraped, and summaries, including maps, can be created relatively easily using R scripts.

## Background info

See [Powerpoint slides](2011-04-09-Computer-Assisted-Reporting.pdf) from 2011 describing the problem and how it was solved for the 2010 election.  The same approach was applied to the 2016 elections.

## R Scripts

The **ScreenScrape.R** script was updated for the 2016 election.  The script "visits" each of the 105 county pages and writes election results to a tab-delimited file.

The **StatewideStats.R** script reads the election results and extracts information for the judicial retention and constitutional amendment questions.  Summary tables and maps are then created.

This process will no longer be necessary when the Kansas Secretary of State's Office enables "View Map" options online for the judicial retention contests like already available in other statewide races.

The tables and maps show that 4 of the 5 justices up for retention elections this year lost in ~60 of the 105 counties, even though all won the statewide contest.

The 2016 results for Chief Justice Lawton Nuss:

![Lawton Nuss Retention Map](Map-Map-Nuss.png)

![Lawton Nuss Retention Table](Table-Lawton-Nuss-2016.csv)

