# Project 2: Open Data App - an RShiny app development project

## Project Title: NYC High Schools
Term: Fall 2017

+ Group 2
+ **NYC High Schools**: + Team members
	+ Xin Luo
	+ Qingyun Lu
	+ Yijia Li
	+ Saaya Yasuda
	+ Enrique Olivo

+ **Project summary**: We developed this app using R Shiny to help parents decide where to send their children to high school in New York City. In this app, users can compare high schools with one another and can, thus, find the best school on the basis of what they value the most, including school location, teacher performance, student achievement, and other factors. Our app makes it easier for users to understand the relative strengths and weaknesses of each school through data visualization (in the form of maps, scatter plots, bar charts, pie charts, and radar charts). We used a specific dataset (“2014-2015 School Quality Reports Results for High Schools”) from data.gov. The dataset includes information pertaining to categories like enrollment, diversity, graduation rate, post-secondary enrollment status, SAT scores, ACT scores, regents scores, and school framework survey results for a total of 847 high schools in New York City from 2014 to 2015.

+ **App link**: 
https://xinluohaha.shinyapps.io/shiny_app_final_version/
+ **Project screenshots**: 

+ Page 1. Search for your ideal school.
+ Tab 1. Find schools on the map on the basis of zip code and total student number, and see your selected school’s detailed information regarding, for example, diversity, post-secondary enrollment status, and SAT scores.
+ Tab 2. Select a school you are interested in and see its location on the map and its detailed information.
+ Tab 3. Select a school you are interested in and see the school’s overall framework survey performance compared to other schools in its borough or in the city as a whole.
+ Page 2. Use radar charts to compare any two schools’ relative strengths and weaknesses.
+ Page 3. Rank the schools on the basis of factors you care about the most and see the top 10 results.
+ Page 4. Select the three factors most important to you and then calculate (from 1 to 10) the importance of each factor in order to identify the corresponding top five schools.
+ Page 5. Look into how a school’s framework survey performance is related to the school’s graduation rate and college-enrollment rate, select the features you like, and see whether they are significantly correlated.


+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members desgined the app. Xin Luo built the framework of shiny app, finished all works ralated to the map, made the summary plots/boxes for each school, and did the UI design for most tabs based on the works of all group members. Qingyun Lu worked on the Intro page and the comparison page and did the UI and Server for the two parts. Organized and analyzed the datasets related and designed the radar chart for school comparison. Yijia Li worked on the UI and Server for School Calculator Page, processed data and made pie charts for each school on School Search Page, and wrote readme file. Enrique Olivo created and designed the Ranking page. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

