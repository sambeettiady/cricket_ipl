from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
# WAIT FOR ELEMENTS
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
#CATCH EXCEPTIONS
#from selenium.common.exceptions import TimeoutException,ElementNotVisibleException
import re
import csv

file_to_save = 'ipl_players_2.csv'

#Function to extract attributes
def extract_attribute(pattern_to_check,string_to_check):
    if string_to_check.find(pattern_to_check) == -1:
        starting_index = string_to_check.find(pattern_to_check)
    else:
        starting_index = string_to_check.find(pattern_to_check) + len(pattern_to_check) + 1
    ending_index = string_to_check.find('\n', starting_index)
    if starting_index == -1:
        attribute = ' '
    elif starting_index != -1 and ending_index == -1:
        attribute = string_to_check[starting_index:]
    else:
        attribute = string_to_check[starting_index:ending_index]
    return attribute

#Configure browser
path_to_chromedriver = '/Users/sambeet/ipl_scraper/chromedriver' # change path as needed
browser = webdriver.Chrome(executable_path = path_to_chromedriver)
browser.wait = WebDriverWait(browser, 5)

#Crawl 1st page
url = 'http://www.iplt20.com/teams'
browser.get(url)

#Get team names and urls
team_list_path = "#content > div > ul"
team_list = browser.wait.until(EC.presence_of_element_located(
    (By.CSS_SELECTOR, team_list_path)))
team_name = list()
team_urls = team_list.find_elements_by_css_selector('li > a')

for i in range(0, len(team_urls)):
    team_name.append(re.sub(pattern='\\n', repl=' ', string=team_urls[i].find_element_by_css_selector('div > div > div.team-card__info--inner > h3').text))
    team_urls[i] = team_urls[i].get_attribute('href')

#Open file
with open(file_to_save, 'wb') as csvfile:
    filewriter = csv.writer(csvfile)
    filewriter.writerow(['Team','Player','Role','Batting Style','Bowling Style','Foreign Player','Is Captain'])

#Start Crawling team pages
    for i in range(0,len(team_urls)):
        print team_name[i]
        browser.get(team_urls[i])
        team_players_list = browser.wait.until(EC.presence_of_element_located(
            (By.CSS_SELECTOR,
            "#content > div.row.team-overview-columns > div.columns.large-3.medium-12.small-12.col-1 > div > div > div > ul")))

#Get players list and urls
        players = team_players_list.text.split('\n')
        players_url = team_players_list.find_elements_by_css_selector('li > a')
        for k in range(0, len(players_url)):
            players_url[k] = players_url[k].get_attribute('href')

        #Browse player page and write attributes to file
        for k in range(0, len(players_url)):
            print 'Crawling URL!!'
            browser.get(players_url[k])
            print 'Extracting Attributes!!!'
            player_attribute = browser.wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"#content > div.row.team-overview-columns > div.columns.large-9.medium-12.small-12.col-2 > section.player-info > div > div.columns.large-6.medium-6.player-info__main > table"))).text
            player_role = extract_attribute(pattern_to_check='Role', string_to_check=player_attribute)
            batting_style = extract_attribute(pattern_to_check='Batting Style', string_to_check=player_attribute)
            bowling_style = extract_attribute(pattern_to_check='Bowls', string_to_check=player_attribute)
            nationality = extract_attribute(pattern_to_check='Nationality', string_to_check=player_attribute)
            if nationality == 'Indian':
                foreign_player = 0
            else:
                foreign_player = 1

            if k == 0:
                captain = browser.wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "#content > div > div > div > div > dl > dd:nth-child(6)"))).text

            if players[k] == captain:
                is_captain = 1
            else:
                is_captain = 0

            filewriter.writerow([team_name[i], players[k], player_role, batting_style, bowling_style, foreign_player, is_captain])
