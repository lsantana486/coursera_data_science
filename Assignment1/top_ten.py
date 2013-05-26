import sys
import json
import operator
import decimal

top_hash={}

def lines(wrd):
    global top_hash
    key=''
    for i in range(len(wrd)):
        key=wrd[i]['text'].encode('utf-8')
        if top_hash.has_key(key)==1:
            top_hash[key] = top_hash[key] + 1
        else:
            top_hash[key] = 1
    
   

def main():
    global top_hash
    
    tweet_file = open(sys.argv[1])
    
    
    for line in tweet_file:
        tweet=json.loads(line)
        if tweet.has_key('entities')==1:
            if tweet['entities']['hashtags'] != []:
                lines(tweet['entities']['hashtags'])

    
    topten = sorted(top_hash.items(), key=operator.itemgetter(1),reverse=True)
    
    for i in range(len(topten)):
        if i == 10:
            break
        hashtag,count = topten[i]
        print hashtag + ' ' + str(round(decimal.Decimal(count),1))
    
    
if __name__ == '__main__':
    main()