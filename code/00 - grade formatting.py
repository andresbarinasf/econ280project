# %%
import pandas as pd 
import numpy as np
import re
import seaborn as sns
import matplotlib.pyplot as plt
import os

# %%
os.chdir('/Users/zhangww/Dropbox (MIT)/research_projects/ChatGPT/Analysis - Replication Package') #input correct filepath

# %%
data = pd.read_csv('raw/grades.csv', header = 0)

# %%
data = data.dropna(how = 'all', axis = 1) 

# %%
ratings = data.filter(regex='(occupation|sheetnum|grader_id|overall|rating.*|prolific_pid.*|time(\d[AB]+)_Page Submit|responseid|StartDate|justify|retry.*)', axis = 1)

ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_1',r'writing\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_2',r'content\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_3',r'originality\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('time(\d[AB]+)_Page Submit',r'time\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('prolific_pid.(\d[AB]+)',r'prolific_pid\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('retry.(\d[AB]+)',r'retry\1',x))

# %%
# convert to wide
ratings_long = pd.wide_to_long(ratings, ('overall', 'writing', 'content', 'originality', 'time', 'prolific_pid', 'retry', 'justify'), 'responseid', 'essaynum', suffix=r'\w+',).reset_index()
ratings_long[['overall', 'writing', 'content', 'originality', 'time']] = ratings_long[['overall', 'writing', 'content', 'originality', 'time']].apply(pd.to_numeric)
ratings_long.StartDate = pd.to_datetime(ratings_long.StartDate)
ratings_long.sort_values(by='StartDate', inplace = True)
ratings_long['essaynum'] = ratings_long.essaynum.str[1]


# %%
# remove duplicates 
# because completed task twice, keep first instance only 
# or because is id - as placeholder
ratings_long = ratings_long.drop_duplicates(subset=['responseid', 'grader_id', 'prolific_pid', 'essaynum'], keep='first', inplace=False, ignore_index=False)
ratings_long = ratings_long.loc[ratings_long.prolific_pid != '-']

# also drop the missing ones or sheetnum 0 because those are not final grades
ratings_long = ratings_long.loc[(ratings_long.prolific_pid.notna()) & (ratings_long.sheetnum != '0')]

# %%
ratings_long.to_csv('raw/grading_long_complete.csv')

# %%
ratingsA = ratings_long.loc[ratings_long.essaynum == 'A',]
ratingsB = ratings_long.loc[ratings_long.essaynum == 'B',]

# %%
ratings_wide = ratingsA.merge(ratingsB, on = ['responseid', 'grader_id', 'prolific_pid', 'occupation'], suffixes = ('_a', '_b'), validate = '1:1') 

# fix error where the wrong essay got posted 
ratings_wide.loc[~((ratings_wide.sheetnum_a == 3) & (ratings_wide.occupation == "HR professional") & (id == "ChatGPT"))]
ratings_wide.loc[~((ratings_wide.sheetnum_a == "L13") & (ratings_wide.occupation == "manager") & (id == "ChatGPT"))]
ratings_wide.loc[(ratings_wide.retry_a != 1) & (ratings_wide.retry_b != 1)]

ratings_wide.to_csv('raw/grading_wide_complete.csv')



# %%
