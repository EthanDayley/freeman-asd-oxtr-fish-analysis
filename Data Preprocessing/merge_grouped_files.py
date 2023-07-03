import os

IMPORT_FILEPATH = os.path.join('..', 'Preprocessed Statistics Files')
STATISTICS_FILES = [
    {
        'import_filename': 'nbm_oxtr_collocation_grouped.csv',
        'column_prefix': 'nbm'
    },
    {
        'import_filename': 'vp_oxtr_grouped.csv',
        'column_prefix': 'vp'
    }
]

EXPORT_FILEPATH = os.path.join('..', 'Preprocessed Statistics Files')
EXPORT_FILENAME = 'nbm_and_vp_stats_merged.csv'

all_sample_ids = []
all_column_headers = ['sample_id']

values_by_sample_id = {}

for file in STATISTICS_FILES:
    # read file
    with open(os.path.join(IMPORT_FILEPATH, file['import_filename']), 'r') as f:
        raw_file = f.read()

    # initial processing
    rows = [i.split(',') for i in raw_file.splitlines()]
    columns = rows.pop(0)
    columns.pop(0)  # remove sample_id to prevent duplication
    columns = [file['column_prefix'] + '_' + i for i in columns]
    all_column_headers += columns

    # now add values to values_by_sample_id
    for row in rows:
        sample_id = row.pop(0)
        if sample_id not in all_sample_ids:
            all_sample_ids.append(sample_id)
        for i in range(len(columns)):
            col = columns[i]
            val = row[i]
            if sample_id not in values_by_sample_id:
                values_by_sample_id[sample_id] = {}     # make sure that sub-dict is initialized
            values_by_sample_id[sample_id][col] = val   # assign value

# sort sample ids
all_sample_ids.sort()

# generate matrix with so, so much list comprehension
export_matrix = [all_column_headers] + [[sample_id] + ['' for i in range(len(all_column_headers)-1)] for sample_id in all_sample_ids]

# fill matrix with values
for row_index in range(1, len(export_matrix)):
    sample_id = export_matrix[row_index][0]
    for col_index in range(1, len(export_matrix[0])):
        column_header = export_matrix[0][col_index]
        try: export_matrix[row_index][col_index] = values_by_sample_id[sample_id][column_header]
        except: pass    # remember that matrix already initialized

# join export matrix into csv format. I know there's a module, but I don't need it.
export_csv = '\n'.join([','.join(row) for row in export_matrix])

with open(os.path.join(EXPORT_FILEPATH, EXPORT_FILENAME), 'w') as f:
    f.write(export_csv)