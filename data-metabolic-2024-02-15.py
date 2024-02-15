import pandas
metabolic_df = pandas.read_pickle("data-metabolic-2024-02-15.pkl")
metabolic_df.iloc[:,1:25]#features
metabolic_df.iloc[:,25]#label
metabolic_df.iloc[:,26]#width of 95% CI
metabolic_df.iloc[:,27]#type of filtering
metabolic_df.iloc[:,28]#label in a different domain
