import os

class StatsTable:
    '''
    This class represents data from a raw statistics file.
    It contains functionality to load the relevant data and
    average across sample images to provide data in a usable format.
    '''

    filename = ''
    raw_file = ''
    raw_rows = []
    raw_columns = []
    columns = []
    processed_data = []

    def __mean(self, l): return sum([float(i) for i in l]) / len(l)

    def __get_column_indices(self):
        indexed_columns = {}
        for i in self.columns:
            indexed_columns[i['current_name']] = self.raw_columns.index(i['current_name'])
        return indexed_columns

    def load(self):
        with open(self.filename, 'r') as f:
            self.raw_file = f.read()
        file_rows = self.raw_file.splitlines()[5:]
        self.raw_columns = file_rows.pop(0).split(',')
        self.raw_rows = [i.split(',') for i in file_rows]

    def export_stats(self):
        export_columns = ['sample_id'] + [i['output_name'] for i in self.columns]
        column_names = [i['current_name'] for i in self.columns]
        export_rows = []
        indexed_cols = self.__get_column_indices()
        current_sample_id = ''
        values_by_column = {i: [] for i in column_names}
        for curr_row in self.raw_rows:
            if current_sample_id != '' and current_sample_id != curr_row[0].split()[0]:
                # new sample id, average across collected values and add row to table
                export_rows.append([current_sample_id] + [self.__mean(values_by_column[col]) for col in column_names])
                current_sample_id = curr_row[0].split()[0]
                values_by_column = {i: [] for i in values_by_column}
            elif current_sample_id == '': current_sample_id = curr_row[0].split()[0]
            for col in column_names:
                values_by_column[col].append(curr_row[indexed_cols[col]])

        return {'column_headers': export_columns, 'rows': export_rows}

    def __init__(self, filename, columns):
        self.filename = filename
        self.columns = columns



IMPORT_FILEPATH = os.path.join('..', 'Raw Statistics Files')
EXPORT_FILEPATH = os.path.join('..', 'Preprocessed Statistics Files')

STATS_FILES = [
    {
        'import_filename': 'NBM_OXTR_CollStatistics.csv',
        'export_filename': 'nbm_oxtr_collocation_grouped.csv',
        'columns': [
            {
                'current_name': 'Area (1st integration)',
                'output_name': 'area'
            },
            {
                'current_name': 'Area ratio (1st)',
                'output_name': 'area_ratio'
            },
            {
                'current_name': 'Brightness (1st integration)',
                'output_name': 'brightness'
            }
            
        ]
    },
    {
        'import_filename': 'VP_OXTRStatistics.csv',
        'export_filename': 'vp_oxtr_grouped.csv',
        'columns': [
            {
                'current_name': 'Area (integration)',
                'output_name': 'area'
            },
            {
                'current_name': 'Brightness (integration)',
                'output_name': 'brightness'
            }
        ]
    }
]


for file in STATS_FILES:
    filepath = os.path.join(IMPORT_FILEPATH, file['import_filename'])
    stats = StatsTable(filepath, file['columns'])
    stats.load()
    grouped = stats.export_stats()
    table = [grouped['column_headers']] + grouped['rows']
    csv = '\n'.join([','.join([str(j) for j in i]) for i in table])
    print(csv)
    with open(os.path.join(EXPORT_FILEPATH, file['export_filename']), 'w') as f:
        f.write(csv)