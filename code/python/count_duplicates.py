import pandas as pd
import os

# Change directory
cwd = os.path.dirname(os.path.abspath(__file__))
os.chdir(cwd)

# Import list of papers found
df_adip_size = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="adipocyte_diameter")
df_adip_v = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="adipose_vessel_size")
df_tumor_vsize = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="tumor_vessel_size")
df_tumor_vdensity = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="tumor_vessel_density")
df_cbm_mice = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="CBM_mice")
df_cbm_rats = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="CBM_rats")
df_vegfr12 = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="Kd_for_VEGFR1_and_VEGFR2")
df_nrp1 = pd.read_excel("../../data/papers/paper_list_datamining.xlsx", sheet_name="Kd_for_NRP1")

# Merge dataframes
df_tumor_v = pd.concat([df_tumor_vsize, df_tumor_vdensity])
df_cbm = pd.concat([df_cbm_mice, df_cbm_rats])
df_kd = pd.concat([df_vegfr12, df_nrp1])

# Count the total number of papers
df = pd.concat([df_adip_size, df_adip_v, df_tumor_v, df_cbm, df_kd])

print("The number of papers (including duplicates): ", len(df))

# Check the number of papers is same as the sum of dataframes' length
print("Is the number of rows in `df` is same as the sum of rows of all dataframes?", (len(df) == (len(df_adip_size)+len(df_adip_v)+len(df_tumor_v)+len(df_cbm)+len(df_kd))))

# Count the number of duplicates
print("The number of duplicates in papers list: ", len(df) - len(df.drop_duplicates()))

print("The number of papers (excluding duplicates): ", len(df.drop_duplicates()))

# Count the number of papers included in analysis
df_no_dup = df.drop_duplicates()
print("The number of papers included in the analysis: ", len(df_no_dup.loc[df_no_dup["Included"]=="Yes"]))