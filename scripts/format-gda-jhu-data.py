import argparse
import json
import re
from os import listdir
from os.path import isfile, join

import pandas as pd

parser = argparse.ArgumentParser(description="Convert GDA/JHU CSV to Donu JSON")
parser.add_argument(
    "--path", dest="data_dir", type=str, nargs=1, help="path to data", required=True
)

gda_datasource_by_state = {
    "GA": "GDA",
    "FL": "GDA",
}

args = parser.parse_args()
data_dir = args.data_dir[0]

datafiles = [
    join(data_dir, f)
    for f in listdir(data_dir)
    if isfile(join(data_dir, f)) and re.match(r"ASKE-E_GDA_DATA-([A-Z]{2}).csv", f)
]

for file_path in datafiles:
    with open(file_path) as csvfile:
        df = pd.read_csv(csvfile)
        pd.set_option("display.max_rows", df.shape[0] + 1)

        state = file_path.split("-")[-1].split(".")[0]
        org = gda_datasource_by_state.get(state, "JHU")
        data = {
            "name": f"{org} Infection Data",
            "description": f"Infection data for {state}",
            "columns": [],
        }
        for label, content in df.items():
            data["columns"].append(
                {"name": label, "description": label, "values": content.to_list()}
            )

        with open(
            join("./dataRepo/", f"{org}-Infections-{state}.json"), "w"
        ) as jsonfile:
            jsonfile.write(json.dumps(data))
