import urllib
import json

response = urllib.urlopen("http://search.twitter.com/search.json?q=microsoft")
pg0 = json.load(response)
results_num = pg0['results_per_page']

pages=([pg0])
for i in range(10):
    response = urllib.urlopen("http://search.twitter.com/search.json" + pages[i]['next_page'])
    pages.append(json.load(response))
    for j in range(results_num - 1):
       print pages[i]['results'][j]['text']