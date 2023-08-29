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
            if i['type'] == 'mean':
                indexed_columns[i['current_name']] = self.raw_columns.index(i['current_name'])
            elif i['type'] == 'mean_ratio':
                indexed_columns[i['output_name']] = {}
                indexed_columns[i['numerator']] = self.raw_columns.index(i['numerator'])
                indexed_columns[i['denominator']] = self.raw_columns.index(i['denominator'])
            else: raise RuntimeError("Unknown column type \"" + str(i['type']) + "\"")
        return indexed_columns

    def load(self):
        with open(self.filename, 'r') as f:
            self.raw_file = f.read()
        file_rows = self.raw_file.splitlines()[5:]
        self.raw_columns = file_rows.pop(0).split(',')
        self.raw_rows = [i.split(',') for i in file_rows]

    def export_stats(self):
        export_columns = ['sample_id'] + [i['output_name'] for i in self.columns]
        column_names = [i['output_name'] for i in self.columns]
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
            for col in self.columns:
                if col['type'] == 'mean':
                    values_by_column[col['output_name']].append(curr_row[indexed_cols[col['current_name']]])
                elif col['type'] == 'mean_ratio':
                    val = float(curr_row[indexed_cols[col['numerator']]]) / float(curr_row[indexed_cols[col['denominator']]])
                    values_by_column[col['output_name']].append(val)
                else: raise RuntimeError("Unknown column type \"" + str(col['type']) + "\"")
        
        # final sample id, average across collected values and add row to table
        export_rows.append([current_sample_id] + [self.__mean(values_by_column[col]) for col in column_names])
        current_sample_id = curr_row[0].split()[0]
        values_by_column = {i: [] for i in values_by_column}

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
                'type': 'mean',
                'current_name': 'Area ratio (1st)',
                'output_name': 'area_ratio'
            },
            {
                'type': 'mean_ratio',
                'numerator': 'Brightness (1st integration)',
                'denominator': 'Area (target area)',
                'output_name': 'brightness_ratio'
            }
            
        ]
    },
    {
        'import_filename': 'NBM_OXTR_Coll_InvStatistics.csv',
        'export_filename': 'nbm_oxtr_collocation_inverse_grouped.csv',
        'columns': [
            {
                'type': 'mean',
                'current_name': 'Area ratio (1st)',
                'output_name': 'inv_area_ratio'
            },
            {
                'type': 'mean_ratio',
                'numerator': 'Brightness (1st integration)',
                'denominator': 'Area (target area)',
                'output_name': 'inv_brightness_ratio'
            }
            
        ]
    },
    {
        'import_filename': 'NBM_OXTRStatistics.csv',
        'export_filename': 'nbm_oxtr_grouped.csv',
        'columns': [
            {
                'type': 'mean',
                'current_name': 'Area (integration)',
                'output_name': 'area'
            },
            {
                'type': 'mean',
                'current_name': 'Brightness (integration)',
                'output_name': 'brightness'
            }
        ]
    },
    {
        'import_filename': 'VP_OXTRStatistics.csv',
        'export_filename': 'vp_oxtr_grouped.csv',
        'columns': [
            {
                'type': 'mean',
                'current_name': 'Area (integration)',
                'output_name': 'area'
            },
            {
                'type': 'mean',
                'current_name': 'Brightness (integration)',
                'output_name': 'brightness'
            }
        ]
    }
]


for file in STATS_FILES:
    filepath = os.path.join(IMPORT_FILEPATH, file['import_filename'])
    print("grouping:\t", filepath)
    stats = StatsTable(filepath, file['columns'])
    stats.load()
    grouped = stats.export_stats()
    table = [grouped['column_headers']] + grouped['rows']
    csv = '\n'.join([','.join([str(j) for j in i]) for i in table])
    export_filename = os.path.join(EXPORT_FILEPATH, file['export_filename'])
    print("exporting to:\t", export_filename)
    print()
    with open(export_filename, 'w') as f:
        f.write(csv)