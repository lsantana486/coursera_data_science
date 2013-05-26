import sys
import json
import decimal

cntrs={}

def counters(wrd):
    global cntrs
    for i in range(len(wrd)):
        key=wrd[i].encode('utf-8').translate(None, '():";,.!?-').lower()
        if key.find('@') == -1 and key.find('#') == -1 and key.find('/') == -1:
            #if key not in ('they','i','we','you','she','he','it','rt','a','an','to','at','the','in','on','me','his','hers','ours','theirs','its','and','or','that'):
            if cntrs.has_key(key)==1:
                cntrs[key] = cntrs[key] + 1
            else:
                cntrs[key] = 1


def main():
    global cntrs
    tweet_file = open(sys.argv[1])
    for line in tweet_file:
        tweet=json.loads(line)
        if tweet.has_key('text')==1:
            txt=tweet['text'].split()
            counters(txt)
    
    total_values= sum(cntrs.values())
    
    for key in cntrs:
        print key + ' ' + str(round(decimal.Decimal(cntrs[key])/decimal.Decimal(total_values),4))
    
if __name__ == '__main__':
    main()