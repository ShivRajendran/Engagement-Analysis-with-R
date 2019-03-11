# Engagement-Analysis-with-R
Utilizing R to analyze how employees have been responding to engagement survey questions since 2015

#SAhistory.R

Multiple Data sets for 2015-2018. Each csv file contained responses to that years engagement survey. Questions were asked with answers ranging from 1-5 (disagree-agree). Survey questions were broken up into seperate sections: Accountability, Communication, Organizational effectiveness, and etc. For each question Historical average response was recorded and plotted using ggplot. Plots were compiled to see how employee moral and opinion has changed over time. 
ggplot was used to create all plots, while ggsave saved all plot images as pngs for use in reports.

#WrittenResponseWC.R

In engagement surveys there are 3 to 4 written response questions. For each Question for each year wordclouds were made utilizing R's 
wordcloud package and textmining package(tm package). All question responses were stored and words that occured most were scraped while common place words were removed. The tm_map remove word function was used to remove other commonly occuring words that providing no real analysis. Wordclouds were snipped and saved for use in reports regarding engagement.

