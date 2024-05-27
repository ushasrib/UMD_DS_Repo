import pandas as pd
from sqlalchemy import create_engine
import psycopg2

# Load the Excel file into a pandas DataFrame
excel_file_path = "C:\\Users\\bhoga\\Desktop\\Desktop\\UMD Spring'24\\INFM737\\Capstone\\Morocco_Survey_files\\survey_questions_Morocco.xlsx"  # Replace with the path to your Excel file
df = pd.read_excel(excel_file_path)

scenario_data = []

# Iterate over each scenario column
for column in df.columns:
    if column.startswith("Scenario"):  # Assuming scenario columns start with "Scenario"
        scenario_name = column
        
        # Add the scenario name to the scenario data list
        scenario_data.append({"scenario_name": scenario_name})

# Create a DataFrame from the scenario data
scenario_df = pd.DataFrame(scenario_data, columns=["scenario_name"])

# Add auto-incrementing scenario_id to the scenario DataFrame
scenario_df["scenario_id"] = range(1, len(scenario_df) + 1)

# Display the scenario DataFrame
print("Scenario DataFrame:")
print(scenario_df)

# Create an empty DataFrame to store scenario-q_id relationship data
scenario_qid = []

# Iterate over each scenario column
for column in df.columns:
    if column.startswith("Scenario"):  # Assuming scenario columns start with "Scenario"
        scenario_name = column
        
        # Filter rows where the checkbox in the scenario column is checked
        filtered_df = df[df[scenario_name] == 1]
        filtered_df = filtered_df[['source_name','q_id', 'q_text', scenario_name]]
        print(filtered_df)
        # Iterate over the filtered rows and extract q_ids
        for index, row in filtered_df.iterrows():
            # Assuming q_ids are separated by comma in the "q_id" column
                                            
            scenario_qid.append({"scenario_id": scenario_df.loc[scenario_df["scenario_name"] == scenario_name, "scenario_id"].iloc[0],
                                                       "source_name": row["source_name"],
                                                       "q_id": row["q_id"],
                                                       "q_text": row["q_text"]})
                       

# Display the scenario-q_id DataFrame
scenario_qid_df = pd.DataFrame(scenario_qid)
print("\nScenario-q_id DataFrame:")
print(scenario_qid_df)

# Establish a connection to the PostgreSQL database

connection_str = 'postgresql://postgres:Capstone@localhost/ics_capstone'
engine = create_engine(connection_str)

# Push scenario data to PostgreSQL
scenario_df.to_sql(name='scenarios', con=engine, if_exists='replace', index=False)

# Push scenario-q_id relationship data to PostgreSQL
scenario_qid_df.to_sql(name='scenario_qids', con=engine, if_exists='replace', index=False)

print("Data pushed to PostgreSQL successfully.")

