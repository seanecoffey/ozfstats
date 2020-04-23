import psycopg2
import requests
import time
from collections import namedtuple
from tqdm import tqdm_notebook
import pandas

import psycopg2
import requests
import time
from collections import namedtuple
from tqdm import tqdm_notebook
import pandas

#Read Season and Import CSV
season = input('Season to import: ')
div = input('Division to import: ')
string = 'tf2- + div, +' S' + season + '.csv'
df = pandas.read_csv(string)

allMatchIds = []
i=0
while i<len(df):
    allMatchIds.append(df['Map_1_Log'][i])
    allMatchIds.append(df['Map_2_Log'][i])
    allMatchIds.append(df['Map_3_Log'][i])
    allMatchIds.append(df['Map_4_Log'][i])
    allMatchIds.append(df['Map_5_Log'][i])
    i+=1

allMatchIds = [int(x) for x in allMatchIds if str(x) != 'nan']

matchLogData = namedtuple('matchLogData',[
    'season',
    'week',
    'homeTeam',
    'awayTeam',
    'map1LogId',
    'map2LogId',
    'map1GCLogId',
    'map2GCLogId',
])

r = requests.get('http://logs.tf/api/v1/log/{}'.format(allMatchIds[0]))

#Steam ID Converter
steamid64ident = 76561197960265728
def usteamid_to_commid(usteamid):
    for ch in ['[', ']']:
        if ch in usteamid:
            usteamid = usteamid.replace(ch, '')
            usteamid_split = usteamid.split(':')
            commid = int(usteamid_split[2]) + steamid64ident
            return commid

#
MapData = namedtuple('MapData',[
    'map_name',
    'logstf_match_id',
    'blu_score',
    'red_score',
])

def extract_maps(match_id, log):
    return [
        MapData(
            log['info']['map'],
            match_id,
            log['teams']['Blue']['score'],
            log['teams']['Red']['score']
        )
    ]

log_map_data = []
print('Reading all logs in CSV')
for match_id in tqdm_notebook(allMatchIds):

    while True:
        r = requests.get('http://logs.tf/api/v1/log/{}'.format(match_id))
        #print(r)
        if r.status_code == 429:
            print('Too many requests. Sleeping...')
            time.sleep(5)
            continue
        break

    log = r.json()
    log_map_data.extend(extract_maps(int(match_id), log))

print('done')

#Check Database For Missing Logs
match_ids=[]

conn = psycopg2.connect(
##MASKED
)

cur = conn.cursor()


try:
    for md in log_map_data:
        cur.execute("""select * from public.maps m where m.log_id = %(str)s;
        """,
        {'str': md.logstf_match_id})
        result = cur.fetchone() is not None
        if not result:
            match_ids.append(md.logstf_match_id)

finally:
    cur.close()
    conn.close()
    print(match_ids)
    print('done')

#Only process data that is not already in database.
log_map_data = []
for match_id in tqdm_notebook(match_ids):

    while True:

        r = requests.get('http://logs.tf/api/v1/log/{}'.format(match_id))
        print(r)
        if r.status_code == 429:
            print('Too many requests. Sleeping...')
            time.sleep(5)
            continue
        break

    log = r.json()
    log_map_data.extend(extract_maps(int(match_id), log))

print('done')

##incomplete, need to check privacy.
