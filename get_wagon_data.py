import json
import urllib2
import pandas as pd
import os
import csv

get_over = lambda x: int(x.split('.')[0]) + 1
get_ball = lambda x: int(x.split('.')[1])
convert_to_int = lambda x: int(x)
unicode_encode = lambda x: x.encode('utf-8')

os.chdir('/Users/sambeet/Data Science/cricket_ipl_analysis/')
def get_wagon_data_and_parse_as_dataframe(match_id = 335982):
    all_data = pd.DataFrame()
    for inning in range(1,3):
        url_to_get = 'http://www.espncricinfo.com/ci/engine/match/gfx/' + str(match_id) + '.json?inning=' + str(inning) + ';template=wagon'
        url_request = urllib2.urlopen(url_to_get)
        json_data = json.load(url_request)
        inning_data = pd.DataFrame(data=json_data['runs'])
        if inning_data.shape[0] != 0:
            inning_data['over'] = inning_data.o_u.apply(get_over)
            inning_data['ball'] = inning_data.o_u.apply(get_ball)
            inning_data = inning_data[['bat','bowl','comm','over','ball','x','y','z']]
            inning_data.columns = ['batsman_id','bowling_id','commentary','over','ball','wagon_x','wagon_y','wagon_zone']
            inning_data['innings_no'] = int(inning)
            inning_data['cricinfo_match_id'] = int(match_id)
            inning_data['wagon_x'] = inning_data.wagon_x.apply(convert_to_int)
            inning_data['wagon_y'] = inning_data.wagon_y.apply(convert_to_int)
            inning_data['wagon_zone'] = inning_data.wagon_zone.apply(convert_to_int)
            all_data = pd.concat([all_data,inning_data])
        else:
            continue
    return all_data

with open('match_ids.csv','rU') as match_id_file:
    rows = csv.DictReader(match_id_file)
    all_wagon_data_rem = pd.DataFrame()
    for row in rows:
        match_id = row['x']
        print match_id
        match_wagon_data = get_wagon_data_and_parse_as_dataframe(match_id=match_id)
        all_wagon_data_rem = pd.concat([all_wagon_data_rem,match_wagon_data])
#all_wagon_data = pd.concat([all_wagon_data,all_wagon_data_rem])
#all_wagon_data[['cricinfo_match_id','innings_no','batsman_id','bowling_id','over','ball','wagon_x','wagon_y','wagon_zone']].to_csv('all_wagon_data_num.csv')
all_wagon_data_rem['commentary'] = all_wagon_data_rem.commentary.apply(unicode_encode)
all_wagon_data_rem.to_csv('all_wagon_data.csv')
