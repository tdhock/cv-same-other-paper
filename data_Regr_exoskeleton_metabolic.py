import pandas
import re
import numpy as np
metabolic_df = pandas.read_pickle("data_exoskeleton_metabolic/first_data_for_Toby.pkl")
metabolic_df.columns
# I think the primary way you would want to stratify this data is with the impairment level.  It is split into binary columns for each impairment type. Cerebral Palsy (CP) has 4 categories (CP_4 may never be used), then we also have unimpaired, elderly and stroke victims. This should give you 6 distinct groups unless you want to pool all the CP together.
group_list = ["CP_"+str(i+1) for i in range(4)]+["stroke","elderly","unimpaired"]
metabolic_df["group"] = ""
for col_name in group_list:
    group_val = re.sub("_.*", "", col_name)
    metabolic_df["group"] = np.where(
        metabolic_df[col_name]==1,
        group_val,
        metabolic_df["group"])
    print(pandas.crosstab(metabolic_df["group"],metabolic_df[col_name]))
    print("")
metabolic_df.group.value_counts()
metabolic_df.stroke.value_counts()
metabolic_df.iloc[:,1:25]#features
metabolic_df.iloc[:,25]#label
metabolic_df.iloc[:,26]#width of 95% CI
metabolic_df.iloc[:,27]#type of filtering
metabolic_df.iloc[:,28]#label in a different domain
