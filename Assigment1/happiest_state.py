import sys
import json
import operator

states={}

def hw(sf):
    scores = {}
    for line in sf:
        term, score  = line.split("\t")
        scores[term] = int(score) 

    return scores

def lines(wrd,sent,city_code):
    global states
    sent_value = 0
    
    for i in range(len(wrd)):
        key=wrd[i].encode('utf-8')
        if sent.has_key(key)==1:
            sent_value = sent_value + sent[key]
        else:
            sent_value = sent_value
    
    if states.has_key(city_code)==1:
        states[city_code] = states[city_code] + sent_value
    else:
        states[city_code] = sent_value
    

def main():
    global states
    
    sent_file = open(sys.argv[1])
    tweet_file = open(sys.argv[2])
    
    sent_dict=hw(sent_file)
    
    for line in tweet_file:
        tweet=json.loads(line)
        if tweet.has_key('place')==1 and tweet.has_key('text')==1:
            if tweet['place'] is not None:
                if tweet['place']['country_code'] == 'US' and tweet['place']['place_type']== 'city':
                    cc = str(tweet['place']['full_name']).split(',')[1]
                    cc = cc.strip()
                    txt=tweet['text'].split()
                    lines(txt,sent_dict,cc)
    
    h_state = max(states.iteritems(), key=operator.itemgetter(1))[0]

    print h_state
    
    
if __name__ == '__main__':
    main()