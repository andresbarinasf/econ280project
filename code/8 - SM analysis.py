# %%
import pandas as pd 
import numpy as np
import re
import seaborn as sns
import matplotlib.pyplot as plt
import scipy
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import os 
import nltk

# %%
os.chdir('/Users/zhangww/Dropbox (MIT)/research_projects/ChatGPT/REPLICATION PACKAGE FINAL') #input correct filepath

# %%
data = pd.read_csv('intermediate/for_gptgrades_analysis.csv',  encoding = "ISO-8859-1")

## Make plot of grade distribution depending on ChatGPT Usage + pre-post

# split into groups 
data['grade_post'] = np.where(data.a_first == 0, data.overall_a, data.overall_b)
data['grade_pre'] = np.where(data.a_first == 0, data.overall_b, data.overall_a)
chatgpt = data.loc[data.prolific_pid == 'ChatGPT',]
basechatonly = data.loc[(data.treatment == 1) & (data.usedgpt == 1) & (data.used == '2')]
basechat = data.loc[(data.treatment == 1) & (data.usedgpt == 1) & (data.used.str.contains('2'))]
editchat = data.loc[(data.treatment == 1)& (data.usedgpt == 1) & (data.used.str.contains('3'))]
usedgpt = data.loc[(data.treatment == 1) & (data.usedgpt == 1)]
treatment = data.loc[(data.treatment == 1)]
control = data.loc[(data.treatment == 0)]

chatgpt['group'] = 'chatgpt'
basechatonly['group'] = 'basechatonly'
basechat['group'] = 'basechat'
editchat['group'] = 'editchat'
usedgpt['group'] = 'usedgpt'
treatment['group'] = 'treatment'
control['group'] = 'control'

allgroups = [control, treatment, usedgpt, editchat, basechat, basechatonly, chatgpt]

cdf = pd.concat(allgroups)[['prolific_pid', 'grade_post', 'group', 'grade_pre']]  

post_df = cdf[['prolific_pid', 'grade_post', 'group']].rename(columns={'grade_post':'grade'})
post_df['time'] = 'post'
pre_df = cdf[['prolific_pid', 'grade_pre', 'group']].rename(columns={'grade_pre':'grade'})
pre_df['time'] = 'pre'
long_cdf = pd.concat([pre_df, post_df])
long_cdf.time = np.where(long_cdf.prolific_pid == 'ChatGPT', 'post', long_cdf.time)

# violin plot 
# sns.set(rc={'figure.figsize':(15,7)})
sns.set_style('darkgrid')
plt.figure(figsize=(15,7))
ax = sns.violinplot(x="group", y="grade", hue="time", split = True, inner = None, data=long_cdf)
ax2 = sns.pointplot(x="group", y="grade", hue="time",
                    data=long_cdf, dodge=0.2, join=False, palette=['white'], ax = ax)

ax2.set_xticklabels(["Control", "Treatment", "Used ChatGPT", "ChatGPT, Edit", "ChatGPT, Submit +", "ChatGPT, Submit", "Researcher ChatGPT"])
ax2.set_xlabel("")
ax2.set_ylabel("Grade")

legend_elements = [
    Patch(facecolor=sns.color_palette()[0], edgecolor='black', label='Pre-Treatment Period'),
    Patch(facecolor=sns.color_palette()[1], edgecolor='black', label='Post-Treatment Period'),
    Line2D([], [], marker='o', color='white', markerfacecolor='white', markersize=10, label='Mean Grade')
]

plt.legend(handles=legend_elements, bbox_to_anchor=(1, 1), loc=2, borderaxespad=0.)
plt.savefig('output/gpt_grade_chg.png', dpi=800, bbox_inches='tight')
plt.clf()

# %%
# Look at grader characteristics 
grader_screener = pd.read_csv('raw/grading_screener.csv')

grader_characteristics = grader_screener.loc[(grader_screener.grader_id.isin(data.grader_id)) & (grader_screener.salary.notna()),].drop_duplicates('grader_id')

# plot salaries 
bar_plot = sns.barplot(x = grader_characteristics.salary.unique(),
             y = grader_characteristics.salary.value_counts(), 
             order = ['< $20,000',  '$20,000-39,999', '$40,000-59,999','$60,000-79,999','$80,000-99,999', '$100,000-119,999', '$120,000+'],
             color = 'grey')
bar_plot.set_xlabel("Salary")
bar_plot.set_ylabel("Count")
plt.xticks(rotation=45)
plt.savefig('output/grader_salary.png', dpi=800, bbox_inches='tight')
plt.clf()

# %%
grader_balance = data.merge(grader_characteristics, how = 'left', on = 'grader_id', suffixes = ('_t', '_g'))
ax = sns.histplot(data=grader_balance[(grader_balance.retry_a.isna()) & (grader_balance.retry_b.isna())], x="salary_g", hue="treatment", multiple="dodge", stat = "density")
plt.xticks(rotation=45)
ax.set_xlabel("Salary")
plt.legend(title = "Group", labels = ["Control", "Treatment"])
plt.savefig('output/grader_salary_group.png', dpi=800, bbox_inches='tight')
plt.clf()

grader_balance.tenure_g = grader_balance.tenure_g.astype(float)
print(grader_balance.groupby('treatment').tenure_g.describe())

# %%
# edit distance
data = pd.read_csv('raw/fullsurvey_fulltext.csv')
subsample = pd.read_csv('intermediate/levenshtein.csv', encoding = "ISO-8859-1")
subsample = subsample.merge(data, how = 'left', on = ['prolific_pid', 'responseid'])#.drop_duplicates('prolific_pid')
subsample = subsample.filter(regex = '^(prolific_pid|task_a|task_b|essaycontent.*|firstpaste|treatment|a_first)$', axis = 1)
subsample["treatment_task"] = np.where(subsample.a_first == 1, "b", "a")
subsample["treatment_essay"] = np.where(subsample.a_first == 1, subsample.task_b, subsample.task_a)
def select_value(row): 
    column_name = f"essaycontent_{row['treatment_task']}_{row['firstpaste']}"
    return row[column_name]

subsample["paste_essay"] = subsample.apply(select_value, axis = 1)

regex = re.compile(r'\W+')

def split_words(s):
    return [word.lower() for word in regex.split(s) if word != '']

subsample["paste_essay_list"] = subsample.paste_essay.apply(split_words)
subsample["treatment_essay_list"] = subsample.treatment_essay.apply(split_words)
subsample["paste_essay_len"] = subsample.paste_essay_list.apply(len)
subsample["treatment_essay_len"] = subsample.treatment_essay_list.apply(len)

subsample["edit_dist"] = subsample.apply(lambda row: nltk.edit_distance(row['paste_essay_list'], row['treatment_essay_list']), axis=1)

# drop the ones that we think are pasting in more chatgpt 
sus = ['090908b9758d55f27cb259b72fce7917ba478d5a07b70c0fd9a98e778ad3aa23', 
       '256a93c634a183da7748803c60463ddd3459368a860021c516a0171d964388c4',
       '321a85bd85f3dd6014299195be807c384fc70d7d844e9c010ca74a1f71abef55',
       'c66ed9f333aed70682d43cf25be5929393fdda7ed4bd54ee8ffd314374c68600',
       'd5047e8615ea4ba9edfc6ddc3d49e263dc676a79e7016e681f570d9146a05788']

subsample = subsample.loc[~subsample.prolific_pid.isin(sus)]
print((subsample.edit_dist/subsample.treatment_essay_len).describe())

# %%
# ChatGPT essays
data = pd.read_csv('raw/grades.csv', header = 0)

# subset on graders
relevant_graders = pd.read_csv('intermediate/for_gptgrades_analysis.csv',  encoding = "ISO-8859-1").grader_id
data = data.loc[data.grader_id.isin(relevant_graders)]

# produce table of pure ChatGPT stuff 
data = data.dropna(how = 'all', axis = 1) 

ratings = data.filter(regex='(occupation|sheetnum|grader_id|overall|rating.*|prolific_pid.*|time(\d[AB]+)_Page Submit|responseid|StartDate|justify|retry.*|essay.*)', axis = 1)

ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_1',r'writing\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_2',r'content\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('rating(\d[AB]+)_3',r'originality\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('time(\d[AB]+)_Page Submit',r'time\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('prolific_pid.(\d[AB]+)',r'prolific_pid\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('retry.(\d[AB]+)',r'retry\1',x))
ratings = ratings.rename(columns=lambda x: re.sub('essay.(\d[AB]+)',r'essay\1',x))

ratings_long = pd.wide_to_long(ratings, ('overall', 'writing', 'content', 'originality', 'time', 'prolific_pid', 'essay', 'justify'), 'responseid', 'essaynum', suffix=r'\w+',).reset_index()
ratings_long[['overall', 'writing', 'content', 'originality', 'time']] = ratings_long[['overall', 'writing', 'content', 'originality', 'time']].apply(pd.to_numeric)
ratings_long.StartDate = pd.to_datetime(ratings_long.StartDate)
ratings_long.sort_values(by='StartDate', inplace = True)
ratings_long['essaynum'] = ratings_long.essaynum.str[1]

grouped = ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].groupby('occupation').agg(
    num_unique_essays=('essay', 'nunique'),
    num_grades = ('overall', 'count'),
    avg_grade=('overall', 'mean'),
    sd_grade=('overall', 'std'),
    num_graders=('grader_id', 'nunique')
).reset_index()

total = pd.DataFrame({'occupation':'total', 
                      'num_unique_essays':ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].essay.nunique(),
                      'num_grades':ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].overall.count(),
                      'avg_grade': ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].overall.mean(),
                      'sd_grade': ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].overall.std(),
                      'num_graders': ratings_long.loc[ratings_long.prolific_pid == 'ChatGPT'].grader_id.nunique()}, 
                      index = [0])

chatgpt_table = pd.concat([grouped,total]).rename(columns = {'occupation':'Occupation', 
                                                             'num_unique_essays':'# Unique Essays',
                                                             'num_grades':'# Grades',
                                                             'avg_grade':'Mean Grade',
                                                             'sd_grade':'SD Grade',
                                                             'num_graders':'# Unique Graders'})

chatgpt_table['Mean Human Grade'] = pd.concat([usedgpt.groupby('occupation').agg(avg_grade = ('grade_post', 'mean')).reset_index().avg_grade, pd.Series(usedgpt.grade_post.mean())])

print(chatgpt_table.round(2).to_latex(column_format = 'l | cccccc', index = False))

