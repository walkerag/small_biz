# small_biz

An analysis of small business influence using US census bureau business panel data and results from the 2016 election

Blog post: arandomwalker.com/2017/4/8/smallbizanalysis

## Data Sources

Census Data: 
https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml   
Go to the Download Center, then select "Business Patterns" in second step. 2004-2014 are currently available.   
Overview: https://www.census.gov/programs-surveys/cbp/about.html   
I used both the county and entire US level aggregations   
Make sure to be careful as multiple rollups will be contained in the same download file   

County Shape Files:
https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html    
Very helpful R mapping tutorial: https://www.datascienceriot.com/mapping-us-counties-in-r-with-fips/kris/    

County Election Results:
https://github.com/tonmcg/County_Level_Election_Results_12-16    

## Code Overview

01_cbp_all_usa.R: Pulls in country-level census data file, creates overview barcharts/scatterplots    
02_elections_plot.R: Makes the pretty-ish map of county level US election results    
03_cbp_county.R: Pulls in county-level census data file, merges to election data, then analyzes small business influence in GOP vs DEM counties    




