#all these packages must be install before the script can run
from bs4 import BeautifulSoup
from urllib.request import urlopen
import pandas as pd
import time
import os
import sys
import csv
import re
import requests
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


#using selenium is good for dealing with javascript which becomes a factor with the sypder section
#there is also a application that needs to be install and placed in the small folder as the script in order to open the webdriver
#a different driver is required depending on the webbrowser 
###This will be useful to create the sypder part of the code
def get_links(): 
    datelist = pd.date_range('2014-03-22', periods=191).tolist()
    dates=[datetime.date(d).strftime('%Y-%m-%d') for d in datelist]
    
    url_list = []
    root_url='https://www.fangraphs.com/'
    for date in dates:    
        try:
            page = urlopen(root_url+'scoreboard.aspx?date='+date).read()
            soup1 = BeautifulSoup(page,'html.parser')
            href_list=soup1.find_all('a',href=re.compile('plays.aspx+'))
            for link in href_list:
                url_list.append(root_url + link['href'])
        except:
            pass
    return url_list
        
    #this section  does the scraping for now only for url but is easily changed to a loop for a url_list
def scrape(url_list):
    browser = webdriver.Firefox()
    ftdf=pd.DataFrame()
    fddf=pd.DataFrame()
    df=pd.DataFrame()
    for url in url_list:
        team=re.search('team=(.*)&dh', url).group(1)
        date=re.search('date=(.*)&team', url).group(1)
        time.sleep(10)
        browser.get(url)
        element= WebDriverWait(browser,20).until(
            EC.presence_of_element_located((By.XPATH,'/html/body/form/div[3]/div[2]/table[2]/tbody/tr/td[1]/div[1]/div/div/div/div[1]/table')))
        table=browser.find_element_by_xpath('/html/body/form/div[3]/div[2]/table[2]/tbody/tr/td[1]/div[1]/div/div/div/div[1]/table')       
        raw_html = table.get_attribute('innerHTML')
        soup = BeautifulSoup(raw_html, 'html.parser')
        
        rows=[]#these loops parse the tables and but the data into a dataframe
        tcol=[]
        dcol=[]
        for row in soup.find_all('tr'):
            cols = row.find_all('td')
            tcol.append(team)
            dcol.append(date)
            row=[]
            for col in cols:
                row.append(col.text)
            rows.append(row)

        h_df=pd.DataFrame(rows)
        df=df.append(h_df,ignore_index=True)
        tdf=pd.DataFrame(tcol)
        ftdf=ftdf.append(tdf,ignore_index=True)
        ddf=pd.DataFrame(dcol)
        fddf=fddf.append(ddf,ignore_index=True)

        fdf=pd.concat([ftdf.reset_index(drop=True),df], axis=1)
        ffdf=pd.concat([fddf.reset_index(drop=True),fdf], axis=1)
    return ffdf #ends the function by returning the roster, penalty, goal and goalie dataframes

if __name__ == '__main__':
    #this actually runs functions 
    urls = get_links()
    df=scrape(urls) #specific url but will be changed to urls
    df.to_csv(r'C:\Users\peter\OneDrive\Documents\baseball-databases\fangraphs\data\2014_leverage_pbp.csv',index=False)
    
