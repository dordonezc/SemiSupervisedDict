import requests
from bs4 import BeautifulSoup

def easy_parser(url):
    data = requests.get(url)
    soup = BeautifulSoup(data.content, 'html.parser')
    parsed = soup.select("#news-content")[0]
    return parsed.text

def link_parser(url):
    data = requests.get(url)
    soup = BeautifulSoup(data.content, 'html.parser')
    articles = soup.select(".categoryArticle")
    save_list = []
    for a in articles:
        local_div = a.select("div")[0]
        aux = [local_div.a['href'], local_div.a.getText(), local_div.p.getText()]
        save_list.append(aux)
    return save_list


##---------------------------------------------------------------------------------##
## Parsing links

# page_num = range(6, 180)

# all_links = []
# for pag in page_num:
#     print("Parsing " + str(pag))
#     url = "https://oilprice.com/Latest-Energy-News/World-News/Page-" + str(pag) + ".html"
#     all_links.append(link_parser(url))

#data = pd.DataFrame([val for elem in all_links for val in elem])
#data.to_csv("OilTest.csv")
# import pandas as pd 
# data = pd.read_csv("OilTest.csv").drop('Unnamed: 0', axis = 1)
# i = 0 
# save_list = []
# for link in data[0]:
#     i+=1
#     print("Iteration " + str(i))
#     try:
#         aux = easy_parser(link)
#     except:
#         print("Failing")
#         aux = None
#     save_list.append(aux)

# store = pd.concat([pd.Series(save_list), data[0]],axis=1)
# store.to_csv("OilNews.csv")
