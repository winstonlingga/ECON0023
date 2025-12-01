import os
import pandas as pd
import re

# === SET YOUR FOLDER PATH ===
folder_path = "/Users/beatricesoresina/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Academic/UCL/Year 4/Trade/HPI/"

df_list = []

print("\n=== FILES FOUND ===")
all_files = [
    os.path.join(folder_path, f)
    for f in os.listdir(folder_path)
    if f.endswith(".csv")
]
print(all_files)
print("====================\n")


# === PROCESS EACH FILE ===
for file in all_files:
    print(f"\nProcessing: {file}")

    # READ FILE CORRECTLY
    try:
        df = pd.read_csv(
            file,
            sep=";",           
            skiprows=1,        
            engine="python",
            encoding="latin-1"
        )
    except Exception as e:
        print(f"Error reading {file}: {e}\n")
        continue

    df = df.drop(columns=[""], errors="ignore")

    # Required columns
    required_cols = ["Regione", "Descr_Tipologia", "Compr_min", "Compr_max"]
    if not all(col in df.columns for col in required_cols):
        missing = [col for col in required_cols if col not in df.columns]
        print(f"Skipping — missing: {missing}")
        continue

    # Convert decimals
    for col in ["Compr_min", "Compr_max"]:
        df[col] = (
            df[col]
            .astype(str)
            .str.replace(",", ".", regex=False)
            .str.replace(" ", "", regex=False)
        )
        df[col] = pd.to_numeric(df[col], errors="coerce")

    # === FILTER only Abitazioni civili ===
    df = df[df["Descr_Tipologia"].str.strip() == "Abitazioni civili"]

    # === EXTRACT DATE FROM FILENAME ===
    match = re.search(r"(\d{5})_VALORI", file)
    if match:
        raw = match.group(1)  # e.g. 20111
        year = raw[:4]
        sem = raw[4]
        df["date"] = f"Semester {year}/{sem}"
    else:
        print("Could not extract semester — skipping file.")
        continue

    # === KEEP ONLY NECESSARY COLUMNS ===
    df = df[["date", "Regione", "Compr_min", "Compr_max"]]

    df_list.append(df)
    print(f"✓ Added cleaned rows from {file}")


# === CONCAT ALL ===
if len(df_list) == 0:
    print("\nERROR: No valid dataframes created — nothing to concatenate.")
    exit()

final_df = pd.concat(df_list, ignore_index=True)

print("\n=== FINAL CLEAN DATAFRAME ===")
print(final_df.head())

# Save raw cleaned output
final_csv = os.path.join(folder_path, "combined_output.csv")
final_df.to_csv(final_csv, index=False)


# === COMPUTE REGIONAL HPI AVERAGE PER SEMESTER ===
final_df["HPI_average"] = (final_df["Compr_min"] + final_df["Compr_max"]) / 2

hpi_df = (
    final_df.groupby(["date", "Regione"], as_index=False)
            .agg({"HPI_average": "mean"})
)

# Save aggregated dataset
hpi_csv = os.path.join(folder_path, "HPI_per_region_semester.csv")
hpi_df.to_csv(hpi_csv, index=False)

print("\n=== HPI AVERAGE PER REGION PER SEMESTER ===")
print(hpi_df.head())

print(f"\nSaved regional HPI dataset to:\n{hpi_csv}")
# Save raw cleaned output
final_csv = os.path.join(folder_path, "combined_output.csv")
final_df.to_csv(final_csv, index=False)
print(f"\nSaved combined cleaned dataset to:\n{final_csv}\n")

# Reload the dataset just created (safer)
hpi_df = pd.read_csv(hpi_csv)

# Extract YEAR from "Semester YYYY/S"
# "Semester 2011/1" → 2011
hpi_df["Year"] = hpi_df["date"].str.extract(r"(\d{4})").astype(int)

# Compute annual HPI as the mean of the two semesters
hpi_year_df = (
    hpi_df.groupby(["Year", "Regione"], as_index=False)
          .agg({"HPI_average": "mean"})
)

# Save annual dataset
annual_csv = os.path.join(folder_path, "HPI_per_region_year.csv")
hpi_year_df.to_csv(annual_csv, index=False)

print("\n=== ANNUAL HPI PER REGION ===")
print(hpi_year_df.head())

print(f"\nSaved annual HPI dataset to:\n{annual_csv}")