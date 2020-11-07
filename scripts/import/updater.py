# Google Auth
from oauth2client.service_account import ServiceAccountCredentials

# Imports etc.
import psycopg2
import requests
import time
from collections import namedtuple
from tqdm import tqdm
import pandas
import gspread
import os
from dotenv import load_dotenv
#load env
load_dotenv()

#Load Database settings
db_host = os.getenv("DATABASE_HOST")
db_port = os.getenv("DATABASE_PORT")
db_user = os.getenv("DATABASE_USER")
db_password = os.getenv("DATABASE_PASSWORD")
db_name = os.getenv("DATABASE_NAME")

# OAuth Config
scopes = [
    'https://www.googleapis.com/auth/spreadsheets',
    'https://www.googleapis.com/auth/drive'
]
credentials_file = os.getenv("GOOGLE_CREDENTIALS")
google_credentials = ServiceAccountCredentials.from_json_keyfile_name(credentials_file, scopes)

#clear terminal
def clear():
    if os.name == 'nt':
        _ = os.system('cls')
    else:
        _ = os.system('clear')

# Main Options Menu
def menu():
    clear()
    print("\t**********************************************")
    print("\t***  ozfstats updater - v0.0  ***")
    print("\t**********************************************")
    print("\n 1. Import logs into database \n 2. ozfstats update scripts (R) \n")
    choice = input("select script \n")

    if choice == "1":
        sheet_fn()
        menu()

    elif choice == "2":
        print("Use R CMD for now!")
        menu()

    else:
        print("Not a valid option")
        menu()

# Main Import Script
def sheet_fn():
    # Take inputs to see what data to import
    import_season = int(input("Season to import: \n"))
    import_div = input("Choose Division to import: (use 'prem', 'high', 'inter')\n")
    if import_div == "prem":
        wsname = "Prem"
    elif import_div == "high":
        wsname = "High"
    elif import_div == "inter":
        wsname = "Inter"
    else:
        print("not a valid division\n")
        return

    print("Searching for new logs from Season " + str(import_season) + " - " + wsname + "...")

    # Read google sheet and pull into a dataframe
    gc = gspread.authorize(google_credentials)
    sheet = gc.open("S" + str(import_season) + "-logs")
    ws = sheet.worksheet(wsname)
    data = ws.get_all_values()
    headers = data.pop(0)
    df = pandas.DataFrame(data, columns=headers)

    match_ids = get_match_ids(df) #Get list of logs.tf match ids.
    refined = refine_match_ids(match_ids) #Reduce down to IDs not already in database.
    print(refined)
    if len(refined) == 0:
        print("No new logs to add\n")
        time.sleep(5)
        return
    log_data = pull_from_logstf(refined, import_div) #Pull logs.tf data from new match Ids
    add_match_to_db(pull_map_data(log_data)) #Convert log_data into general map data and push to public.maps
    check_steam_ids(log_data) #Check if there are new players not in the database and add.
    do_push_pull(log_data) #Repetitve pushing to database

    print("\n Done updating fn.. sleeping for 5 seconds...\n")
    time.sleep(5)

# Extract list of logs.tf ids from dataframe
def get_match_ids(df):
    match_ids = []
    i=0
    while i<len(df):
        match_ids.append(df['Map_1_Log'][i])
        match_ids.append(df['Map_2_Log'][i])
        match_ids.append(df['Map_3_Log'][i])
        match_ids.append(df['Map_4_Log'][i])
        match_ids.append(df['Map_5_Log'][i])
        i+=1

    match_ids = [int(x) for x in match_ids if str(x) != '']
    return match_ids

# Convert usteam to community ID
def usteamid_to_commid(usteamid):
    steamid64ident = 76561197960265728
    for ch in ['[', ']']:
        if ch in usteamid:
            usteamid = usteamid.replace(ch, '')

    usteamid_split = usteamid.split(':')
    commid = int(usteamid_split[2]) + steamid64ident

    return commid

def get_steam_ids(log):
    steam_ids=[]

    for steam_id, player_data in log['players'].items():
        steam_ids.append(steam_id)

    return steam_ids

#Extract from log into namedtuple data.
LogData = namedtuple('LogData',[
    'logstf_match_id',
    'div',
    'log'
])

def extract_logs(match_id, log, div):
    return [
        LogData(
            match_id,
            div,
            log
        )
    ]

# Pull logs.tf JSON into log tuple.
def pull_from_logstf(match_ids, division):
    log_data = []
    for match_id in tqdm(match_ids):

        while True:

            r = requests.get("http://logs.tf/api/v1/log/{}".format(match_id))
            print(r)
            if r.status_code == 429:
                print("Too many requests. Sleeping...")
                time.sleep(5)
                continue
            break

        log = r.json()
        log_data.extend(extract_logs(int(match_id), log, division))

    print('Finished pulling from logs.tf \n')
    return log_data

# Connect to database - hide these variables!
def connect_db():
    conn = psycopg2.connect(
        host=db_host,
        port=db_port,
        user=db_user,
        password=db_password,
        database=db_name
    )
    print('Connected to database...')
    return conn

# Reduce match Ids by checking database for existing data
def refine_match_ids(all_match_ids):
    conn = connect_db()
    cur = conn.cursor()
    match_ids=[]
    try:
        for logid in all_match_ids:
            cur.execute("""select * from public.maps m where m.log_id = %(str)s;
            """,
            {'str': logid})
            result = cur.fetchone() is not None
            if not result:
                match_ids.append(logid)
    finally:
        cur.close()
        conn.close()
        print('Checked database, closing database connection...')
        return(match_ids)

# Refine data for maps table
MapData = namedtuple('MapData',[
    'map_name',
    'logstf_match_id',
    'blu_score',
    'red_score',
    'div'
])

# takes log_data tuple
def extract_maps(log_data):
    return [
        MapData(
            log_data.log['info']['map'],
            log_data.logstf_match_id,
            log_data.log['teams']['Blue']['score'],
            log_data.log['teams']['Red']['score'],
            log_data.div
        )
    ]

def pull_map_data(log_data):
    map_data = []
    for ld in log_data:
        map_data.extend(extract_maps(ld))
    return (map_data)

#Push map data to database.
def add_match_to_db(match_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for md in match_data:
            cur.execute(
            """
            INSERT INTO public.maps(map, log_id, blu_score, red_score, div)
            VALUES (%s, %s, %s, %s, %s) ON CONFLICT DO NOTHING;
            """,
            tuple(md)
            )
        conn.commit()
        print('Successfully commmitted match data to database')
    finally:
        cur.close()
        conn.close()

# Player Data
PlayerData = namedtuple('PlayerData',[
    'steam_id',
    'nickname',
])

def check_steam_ids(log_data):
    steam_id_list = []

    for ld in log_data:
        steam_id_list.extend(get_steam_ids(ld.log))

    steam_id_list = list(dict.fromkeys(steam_id_list))

    conn = connect_db()
    cur = conn.cursor()
    log_player_data = []
    try:
        for steam_id in steam_id_list:
            cur.execute("""select * from public.players p where p.steam_id = %(str)s;
            """,
            {'str': steam_id})
            result = cur.fetchone() is not None
            if not result:
                print('nickname for \nhttp://warzone.ozfortress.com/users/steam_id/{}'.format(usteamid_to_commid(steam_id)))
                nick = get_warzone_nick(usteamid_to_commid(steam_id))
                print("nickname from warzone: \n" + nick)
                log_player_data.extend(PlayerData(steam_id, nick))
    finally:
        cur.close()
        conn.close()
        add_new_players(log_player_data)
        print("finished checking steam ids")
        time.sleep(5)

# Warzone requests via api.
def get_warzone_nick(steamid):
    r = requests.get("http://node1.seanecoffey.com:3030/api/{}".format(steamid))

    return r.json()

def add_new_players(log_player_data):
    new_players = {}
    new_steamids = []
    new_nicknames = []

    i=0
    while i<len(log_player_data):
        new_steamids.append(log_player_data[i])
        i+=1
        new_nicknames.append(log_player_data[i])
        i+=1

    j=0
    while j<(len(log_player_data)/2):
        new_players[new_steamids[j]]=new_nicknames[j]
        j+=1

    print("Proposed additions to player database: \n")
    print(new_players)
    confirmation = input("Confirm push to database? (Y/N)\n\n")
    if confirmation == ("y" or "Y"):
        push_players_to_db(log_player_data, new_players, new_steamids)
    if confirmation == ("n" or "N"):
        print("Not pushed")

def push_players_to_db(log_player_data, new_players, new_steamids):
    conn = connect_db()
    cur = conn.cursor()

    try:
        i=0
        while i<(len(log_player_data)/2):
            cur.execute(
            """
            INSERT INTO public.players(steam_id, nickname)
            VALUES ('{}', '{}') ON CONFLICT DO NOTHING;
            """.format(new_steamids[i],new_players[new_steamids[i]]),
            )
            i+=1
            conn.commit()
            print("Successfully pushed new player to database")
    finally:
        cur.close()
        conn.close()

# Team Map Data.
TeamMapData = namedtuple('TeamMapData',[
    'logid',
    'teamColour',
    'score',
    'kills',
    'damage',
    'deaths',
    'charges',
    'drops',
    'caps',
    'firstcaps',
    'div'
])

def extract_team_map_data(log_data):
    return [
        TeamMapData(
            log_data.logstf_match_id,
            teamColour,
            team_data['score'],
            team_data['kills'],
            team_data['dmg'],
            team_data['deaths'],
            team_data['charges'],
            team_data['drops'],
            team_data['caps'],
            team_data['firstcaps'],
            log_data.div
        ) for teamColour, team_data in log_data.log['teams'].items()
    ]

def pull_team_map_data(log_data):
    team_map_data = []
    for ld in log_data:
        team_map_data.extend(extract_team_map_data(ld))
    return team_map_data

def push_team_map_data(team_map_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for md in team_map_data:
            cur.execute(
            """
            INSERT INTO public.map_results(log_id, team_colour, score, kills, damage, deaths, charges, drops, caps, mids, div)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            """,
            tuple(md)
            )
        conn.commit()
        print('successfully pushed new team map data to database')
    finally:
        cur.close()
        conn.close()

##Player Map Data
PlayerMapData = namedtuple('PlayerMapData',[
    'steamid',
    'log_id',
    'class_primary',
    'mins_primary',
    'kills',
    'assists',
    'deaths',
    'damage',
    'damage_taken',
    'heals_received',
    'caps',
    'healing_done',
    'ubers',
    'drops',
    'div'
])

def extract_player_map_data(log_data):
    return [
        PlayerMapData(
            player_id,
            log_data.logstf_match_id,
            player_data['class_stats'][0]['type'],
            player_data['class_stats'][0]['total_time'],
            player_data['kills'],
            player_data['assists'],
            player_data['deaths'],
            player_data['dmg'],
            player_data['dt'],
            player_data['hr'],
            player_data['cpc'],
            player_data['heal'],
            player_data['ubers'],
            player_data['drops'],
            log_data.div
        ) for player_id, player_data in log_data.log['players'].items()
    ]

def pull_player_map_data(log_data):
    player_map_data = []
    for ld in log_data:
        player_map_data.extend(extract_player_map_data(ld))
    return player_map_data

def push_player_map_data(player_map_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for pmd in player_map_data:
            cur.execute(
            """
            INSERT INTO public.player_map_results(steam_id, log_id, class_primary, mins_primary, kills, assists, deaths, damage, damage_taken, heals_received, caps, healing_done, ubers, drops, div)
            VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            """,
            tuple(pmd)
            )
        conn.commit()
        print('successfully pushed new player map data to database')
    finally:
        cur.close()
        conn.close()

#Check Log time
def count_total_time(x):
    count = 0
    mins_total = 0

    for y in x:
        mins_total += x[count]['total_time']
        count +=1

    return mins_total

PlayerMapData2 = namedtuple('PlayerMapData',[
    'team_col',
    'steamid',
    'log_id',
])

def extract_player_total_time_data(log_data):
    return [
        PlayerMapData2(
            count_total_time(player_data['class_stats']),
            player_id,
            log_data.logstf_match_id
        ) for player_id, player_data in log_data.log['players'].items()
    ]

def pull_player_total_time(log_data):
    player_map_time_data = []
    for ld in log_data:
        player_map_time_data.extend(extract_player_total_time_data(ld))
    return player_map_time_data

def update_player_time(player_map_time_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for pmd in player_map_time_data:
            cur.execute(
            """
            UPDATE public.player_map_results
            SET mins_total=%s
            WHERE steam_id=%s AND log_id=%s
            """,
            tuple(pmd)
            )
        conn.commit()
        print('successfully updated player time data')
    finally:
        cur.close()
        conn.close()

#Team map length check
def check_map_length (log_data):
    total_round_time = 0
    total_map_time = log_data.log['length']
    count=0
    for rnd in log_data.log['rounds']:
        total_round_time+=log_data.log['rounds'][count]['length']
        count +=1
    if total_map_time != total_round_time:
        return [log_data.logstf_match_id, total_map_time == total_round_time, total_map_time, total_round_time]
    else:
        pass

mapLength = namedtuple('mapLength',[
    'log_id',
    'map_length',
    'map_round_length'
])

def pull_total_map_time(log_data):
    total_map_time_data = []
    for ld in log_data:
        if(check_map_length(ld)):
            total_map_time_data.extend(check_map_length(ld))
    return total_map_time_data

def count_round_length (log):
    total_round_time = 0
    count=0
    for rnd in log['rounds']:
        total_round_time+=log['rounds'][count]['length']
        count +=1
    return total_round_time

#Team/player colour data - I don't know why this is separate but I don't want to break things - it should be in player_map_data pull/push
def extract_player_col(log_data):
    return [
        PlayerMapData2(
            player_data['team'],
            player_id,
            log_data.logstf_match_id
        ) for player_id, player_data in log_data.log['players'].items()
    ]

def pull_col_data(log_data):
    player_col_data = []
    for ld in log_data:
        player_col_data.extend(extract_player_col(ld))
    return player_col_data

def push_col_data(player_col_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for pmd in player_col_data:
            cur.execute(
            """
            UPDATE public.player_map_results
            SET team_colour=%s
            WHERE steam_id=%s AND log_id=%s
            """,
            tuple(pmd)
            )
        conn.commit()
        print('Updated team colour data...')
    finally:
        cur.close()
        conn.close()

def push_map_time_data(total_map_time_data):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for pmd in total_map_time_data:
            cur.execute(
            """
            UPDATE public.player_map_results
            SET mins_total=%s
            WHERE steam_id=%s AND log_id=%s
            """,
            tuple(pmd)
            )
        conn.commit()
        print('Updated map time...')
    finally:
        cur.close()
        conn.close()

TeamMapData2 = namedtuple('TeamMapData2',[
    'mapTime',
    'logid',
    'teamColour'
])

def update_team_map_data(log_data):
    return [
        TeamMapData2(
            count_round_length(log_data.log),
            log_data.logstf_match_id,
            teamColour
        ) for teamColour, team_data in log_data.log['teams'].items()
    ]

def pull_update_team_map_data(log_data):
    update_tmd = []
    for ld in log_data:
        update_tmd.extend(update_team_map_data(ld))
    return(update_tmd)

def push_update_team_map_data(update_tmd):
    conn = connect_db()
    cur = conn.cursor()

    try:
        for pmd in update_tmd:
            cur.execute(
            """
            UPDATE public.map_results
            SET map_time=%s
            WHERE log_id=%s AND team_colour=%s
            """,
            tuple(pmd)
            )
        conn.commit()
        print('Successfully updated team map data...')
    finally:
        cur.close()
        conn.close()

def do_push_pull(log_data):
    push_team_map_data(pull_team_map_data(log_data))
    push_player_map_data(pull_player_map_data(log_data))
    update_player_time(pull_player_total_time(log_data))
    #push_map_time_data(pull_total_map_time(log_data))
    push_col_data(pull_col_data(log_data))
    push_update_team_map_data(pull_update_team_map_data(log_data))
    print("\nfinished tasks...\n")


menu()
