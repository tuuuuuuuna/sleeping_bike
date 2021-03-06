import requests
import pandas as pd
import time
from bs4 import BeautifulSoup as bs
from pandas import DataFrame as df
import requests

# 대여소 정보를 담을 List
locations=[]

def get_url(page_num): # 접속할 url 주소를 리턴
    base_url = f'https://www.bikeseoul.com/app/station/moveStationSearchView.do?currentPageNo={page_num:02d}&stationGrpSeq=12'
    # stationGrpSeq = 12 -> 마포구의 지역번호
    return base_url

# 크롤링 함수 선언
def crawling_bike():
    for i in range(1,17):
        page = requests.get(get_url(i))
        features = "html.parser"
        soup = bs(page.text) # 데이터가 있는 table 태그에 접근
        loc_table = soup.select('table.psboard1 > tbody')[0].find_all('tr')

        for row in loc_table:
            loc_info = []

        # <번호. 대여소명>에서 번호와 대여소명 각각 처리
            loc_name = row.select('td.pl10')[0].get_text(strip=True)
            loc_info.append(time.strftime('%c', time.localtime(time.time())))
            if '.' in loc_name:
                loc_info.append(int(loc_name.split('.')[0])) # 번호 추출
                loc_info.append(loc_name.split('.')[-1].lstrip()) # 대여소명 추출
            else:
                loc_info.append(0) # 번호가 없는 경우 0으로 처리
                # loc_info.append(loc_name)
# 대여가능 수
            loc_info.append(int(row.select('td.tr')[1].get_text(strip=True)))

# 주소
#             loc_info.append(row.select('td.mhid')[0].get_text(strip=True))

# 위도, 경도 좌표
            loc_geo = row.find('a')['param-data'].split(',')
            loc_info.append(float(loc_geo[0]))
            loc_info.append(float(loc_geo[1]))
# 리스트에 location 추가
            locations.append(loc_info)
#            print(loc_info)
# 트래픽 속도 조절을 위한 random time 설정
#     time.sleep(30)

# 시작과 마지막 페이지를 arguments로 입력
for i in range(800):
    crawling_bike()
    time.sleep(109)

# List를 데이터프레임으로 변환
header = ['관측시간','번호','대여소명','대여가능 수','위도','경도']

df = pd.DataFrame.from_records(locations, columns = header)
# csv파일로 추출
df.to_csv('seoulbike.csv', index=False)