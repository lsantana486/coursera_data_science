import sys
import json

def hw(sf):
    scores = {}
    for line in sf:
        term, score  = line.split("\t")
        scores[term] = int(score) 

    return scores

def lines(wrd,sent):
    sent_value = 0
    for i in range(len(wrd)-1):
        key=wrd[i].encode('utf-8')
        if sent.has_key(key)==1:
            sent_value = sent_value + sent[key]
        else:
            sent_value = sent_value
    print sent_value

def main():
    sent_file = open(sys.argv[1])
    tweet_file = open(sys.argv[2])
    
    sent_dict=hw(sent_file)
    
    for line in tweet_file:
        tweet=json.loads(line)
        if tweet.has_key('text')==1:
            txt=tweet['text'].split()
            lines(txt,sent_dict)
    
    
if __name__ == '__main__':
    main()