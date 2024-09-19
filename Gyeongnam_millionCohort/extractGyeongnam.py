
# IMPORT BASIC
import os
import pandas as pd
# import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
from matplotlib import font_manager, rc
import platform
from tqdm import tqdm
import sklearn
from sklearn import linear_model
import scipy.stats as stats
from sklearn.preprocessing import StandardScaler
# import statsmodels.api as sm
# import statsmodels.formula.api as smf
# from statsmodels.stats.outliers_influence import variance_inflation_factor
# from patsy import dmatrices
from sklearn.model_selection import train_test_split
import pickle
import chardet

# TIME
import datetime

# CRAWLING
# import lxml.html
import sqlite3
from pandas.io import sql
# from bs4 import BeautifulSoup
# import xmltodict
import json

# Korean font settings
if platform.system() == 'Windows':
# If Windows: 
    font_name = font_manager.FontProperties(fname="c:/Windows/Fonts/malgun.ttf").get_name()
    rc('font', family=font_name)


def load_save_data(dataName: str, dataKeyword: str): 
    print(f"Start reading {dataName} data and concating ...")
    
    path = "D:\\SNUlab\\0. data\\100만압축\\"
    data_whole = pd.DataFrame()

    # Make list of files that contains {dataKeyword} data
    files = [filename for filename in os.listdir(path) if os.path.isfile(os.path.join(path,filename)) and dataKeyword in filename]
    
    # Load each files and concat into one
    for file in tqdm(files): 
        print(f"Start loading and concating {file}")
        # # detect encoding
        # with open(path+file, 'rb') as f: 
        #     encoding = chardet.detect(f.read())['encoding']
        try: 
            data = pd.read_sas(path+file, format='sas7bdat', encoding = 'euc-kr')
        except UnicodeDecodeError: 
            print(f"{file} encoding does not matches others! Skip file")
            continue
        data_whole = pd.concat([data_whole, data], axis = 0)

    data_whole.to_pickle(f"D:\\SNUlab\\백만코호트 경남\\{dataKeyword}_whole.pkl")
    print(f"Concating {dataName} from 2002 to 2013 is done!")


def jk_gyeongnam(): 
    path = "D:\\SNUlab\\백만코호트 경남\\"
    data = pd.read_csv(path+'nhid_jk_whole.csv', encoding = 'euc-kr')
    # extract gyeongnam people by sido code
    data = data.loc[data['SIDO'] == 48].reset_index(drop = True)
    data.to_csv(path+'nhid_jk_gyeongnam.csv', encoding = 'euc-kr', index = False)

def gj_gyeongnam(): 
    path = "D:\\SNUlab\\백만코호트 경남\\"
    data_jk = pd.read_csv(path+'nhid_jk_gyeongnam.csv', encoding = 'euc-kr')
    data_gj = pd.read_csv(path+'nhid_gj_whole.csv', encoding = 'euc-kr')

    gyeongnam_ppl = data_jk['PERSON_ID'].unique().tolist() # 92,846
    data_gj_gn = data_gj.loc[data_gj['PERSON_ID'].isin(gyeongnam_ppl) == True].reset_index(drop = True)
    len(data_gj_gn['PERSON_ID'].unique().tolist()) # 50,873
    data_gj_gn.to_csv(path+'nhid_gj_gyeongnam.csv', encoding = 'euc-kr', index = False)

def t120_gyeongnam(): 
    path_original = "D:\\SNUlab\\0. data\\100만압축\\"
    path = "D:\\SNUlab\\백만코호트 경남\\"
    data_jk = pd.read_csv(path+'nhid_jk_gyeongnam.csv', encoding = 'euc-kr')
    gyeongnam_ppl = data_jk['PERSON_ID'].unique().tolist() # 92,846

    # Make list of files that contains {dataKeyword} data
    files = [filename for filename in os.listdir(path_original) if os.path.isfile(os.path.join(path_original,filename)) and "nhid_gy20_t1" in filename]
    
    # Load each files and concat into one
    for file in tqdm(files): 
        print(f"Start loading {file}")
        try: 
            data = pd.read_sas(path_original+file, format='sas7bdat', encoding = 'euc-kr')
            data_gn = data.loc[data['PERSON_ID'].isin(gyeongnam_ppl) == True].reset_index(drop = True)
            data_gn.to_csv(path+f'{file}_gyeongnam.csv', encoding = 'euc-kr', index = False)
        except UnicodeDecodeError: 
            print(f"{file} encoding does not matches others! Skip file")
            continue

def t130_gyeongnam(): 
    path_original = "D:\\SNUlab\\0. data\\100만압축\\"
    path = "D:\\SNUlab\\백만코호트 경남\\"

    # years = ['2002', '2003', '2004', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013']
    years = ['2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013']
    # Load each files and concat into one
    for year in tqdm(years): 
        print(f"Start loading nhid_gy30_t1_{year}.sas7bdat")
        try: 
            data = pd.read_sas(path_original+f"nhid_gy30_t1_{year}.sas7bdat", format='sas7bdat', encoding = 'euc-kr')
            data_t120_gn = pd.read_csv(path+f"nhid_gy20_t1_{year}_gyeongnam.csv", encoding = 'euc-kr')
            gyeongnam_keyseq = data_t120_gn['KEY_SEQ'].unique().tolist() 
            gyeongnam_keyseq_str = map(str, gyeongnam_keyseq) 
            data_gn = data.loc[data['KEY_SEQ'].isin(gyeongnam_keyseq_str) == True].reset_index(drop = True)
            data_gn.to_csv(path+f'nhid_gy30_t1_{year}_gyeongnam.csv', encoding = 'euc-kr', index = False)

        except UnicodeDecodeError: 
            print(f"nhid_gy30_t1_{year} encoding does not matches others! Skip file")
            continue
    


def main(): 
    load_save_data("자격", "nhid_jk")
    load_save_data("검진", "nhid_gj")
    load_save_data("T1_20", "nhid_gy20_t1")
    load_save_data("T1_30", "nhid_gy30_t1")



path = "D:\\SNUlab\\백만코호트 경남\\"
path_original = "D:\\SNUlab\\0. data\\100만압축\\"
data_t120_gn = pd.read_csv(path+f"nhid_gy20_t1_2002_gyeongnam.csv", encoding = 'euc-kr')
gyeongnam_keyseq = data_t120_gn['KEY_SEQ'].unique().tolist() 
len(gyeongnam_keyseq)
data = pd.read_sas(path_original+f"nhid_gy30_t1_2002.sas7bdat", format='sas7bdat', encoding = 'euc-kr')
gyeongnam_keyseq_str = map(str, gyeongnam_keyseq) 
data_gn = data.loc[data['KEY_SEQ'].isin(gyeongnam_keyseq_str) == True].reset_index(drop = True)
data['KEY_SEQ'].dtype
data_t120_gn['KEY_SEQ'].dtype