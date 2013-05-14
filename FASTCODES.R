# This script allows the user to run all of the commands "in CODES.R" and "CODES.txt" in a few easy steps.

# To run this script you need to do the following two steps:

# 1. Make sure the Rcurl package is installed.
# To install Click "Packages", then click "Install Package(s)",
# click "UK (Bristol)" and press OK, select "RCurl" and press OK.

# 2. Ensure that you have downloaded your
# football data from www.football-data.co.uk and saved it as "Data.csv".
# This data file needs to be in your R working directory:
# To set your working directory click "File", click "Change dir..." and 
# select the folder that your "Data.csv" file is in. 

# Copy and paste the following commands into R:
library(RCurl)
require(RCurl)
d = "https://raw.github.com/maunderb/Betting-Strategy/master/CODES.R"
script = getURL(d, ssl.verifypeer = FALSE)
eval(parse(text = script))

# You can now bring up your list of parameters from Model 1 or Model 2
# by typing "R1" or "R2" respectively.

# You can also make use of the "STRATEGY1" function:

# The following function can be used to decide whether to bet on particular 
# outcomes on a single football match or not. The variables p and q are:
# p = (ALPHA estimate for team i * BETA estimate for team j) and
# q = (k^2 * ALPHA estimate for team j * BETA estimate for team i),
# h, d, a need to be EU odds for home win, draw or away
# win respectively and r is the discrepency level, we recommend setting r = 1:1.
# Where the R output shows a 0 no bet should be placed, where the output displays
# a 1 a bet should be placed on this outcome.
