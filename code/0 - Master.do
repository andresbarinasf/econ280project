clear

global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"


global code "$path/code"
global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"


do "$code/1 - employment data.do"
do "$code/2 - clean.do"
do "$code/3 - grade cleaning.do"
do "$code/4 - main text figures.do"
do "$code/5 - appendix figures.do"
do "$code/6 - followup.do"
do "$code/7 - second followup.do"





