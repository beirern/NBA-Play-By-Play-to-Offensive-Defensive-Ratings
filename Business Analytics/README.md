# 2) Business Question

We have provided a sample dataset of 1,000 (real) Instagram posts by @nba since October 1, 2017 (211 individual photos, 109 photo albums, and 680 videos) for which your task is to predict total “engagements.” Note that these “engagements” are *not* real – i.e., we’ve artificially generated the Engagements column such that there’s no (intentional) correlation with the real-life engagement totals belonging to posts by @nba. To assist with your model, we have provided an identical dataset of 7,766 (real) Instagram posts.

Using these inputs, predict Engagements for each of the 1,000 posts in the holdout set. You will be graded on **Mean Absolute Percentage Error (MAPE)** on Total Viewers. We selected this metric due to scaling in the “engagements” response variable. Further details on each source file and on the MAPE evaluation metric are provided in the prompt PDF made available here.

#### Please submit a file named holdout_set_[Individual_or_Team_Name].csv with the Engagements column filled in with your response variable. Please note that each question is permitted a maximum of two file attachments. Please save your answer in a .csv file and save your code, spreadsheets, and/or other work in a zip file. Accepted file types: zip, csv.
