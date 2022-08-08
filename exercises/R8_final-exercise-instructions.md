# MSIA Boot Camp - Final R exercise

You've learned quite a lot about R in a short time. Congratulations! This exercise is designed to give you some additional practice on the material we have discussed this week while the lectures are still fresh in your mind, and to integrate different tools and skills that you have learned.

## Instructions

#### Task 1: Import your data 

Read the data files `nys_schools.csv` and `nys_acs.csv` into R. These data come from two different sources: one is data on *schools* in New York state from the [New York State Department of Education](http://data.nysed.gov/downloads.php), and the other is data on *counties* from the American Communities Survey from the US Census Bureau. Review the codebook file so that you know what each variable name means in each dataset. 

#### Task 2: Explore your data

Getting to know your data is a critical part of data analysis. Take the time to explore the structure of the two dataframes you have imported. What types of variables are there? Is there any missing data? How can you tell? What else do you notice about the data?

#### Task 3: Recoding and variable manipulation

1. Deal with missing values, which are currently coded as `-99`.
2. Create a categorical variable that groups counties into "high", "medium", and "low" poverty groups. Decide how you want to split up the groups and briefly explain your decision. 
3. The tests that the NYS Department of Education administers changes from time to time, so scale scores are not directly comparable year-to-year. Create a new variable that is the standardized z-score for math and English Language Arts (ELA) for each year (hint: group by year and use the `scale()` function)

#### Task 4: Merge datasets

Create a county-level dataset that merges variables from the schools dataset and the ACS dataset. Remember that you have learned multiple approaches on how to do this, and that you will have to decide how to summarize data when moving from the school to the county level.

#### Task 5: Create summary tables

Generate tables showing the following:

1. For each county: total enrollment, percent of students qualifying for free or reduced price lunch, and percent of population in poverty.
2. For the counties with the top 5 and bottom 5 poverty rate: percent of population in poverty, percent of students qualifying for free or reduced price lunch, mean reading score, and mean math score.

#### Task 6: Data visualization

Using `ggplot2`, visualize the following:

1. The relationship between access to free/reduced price lunch and test performance, at the *school* level.
2. Average test performance across *counties* with high, low, and medium poverty.

#### Task 7: Answering questions

Using the skills you have learned in the past three days, tackle the following question: 

> What can the data tell us about the relationship between poverty and test performance in New York public schools? Has this relationship changed over time? Is this relationship at all moderated by access to free/reduced price lunch?

You may use summary tables, statistical models, and/or data visualization in pursuing an answer to this question. Feel free to build on the tables and plots you generated above in Tasks 5 and 6.

Given the short time period, any answer will of course prove incomplete. The goal of this task is to give you some room to play around with the skills you've just learned. Don't hesitate to try something even if you don't feel comfortable with it yet. Do as much as you can in the time allotted.

## Github submission

When you have completed the exercise, save your Markdown file in the `submissions` folder of your forked repo using this naming convention: `FinalRExercise_LastnameFirstname.Rmd`. Commit changes periodically, and push commits when you are done.

You can optionally create a pull request to submit this file (and other exercise files from the bootcamp sessions) to the base repo that lives in the MSiA organization. If you would like to do this, make sure that all new files you have created are in the `submissions` folder, and then create a pull request that asks to merge changes from your forked repo to the base repo. 

## Reminders

- Remember to **load necessary packages**.
- Remember to **comment extensively** in your code. Since you will be working in an RMarkdown file, you can describe your workflow in the text section. But you should also comment within all of your code chunks.
- Attempt to knit your Markdown file into HTML format before committing it to Github. Troubleshoot any errors with the knit process by checking the lines referred to in the error messages.
