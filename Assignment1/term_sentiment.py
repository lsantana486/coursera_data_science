import sys
import json

def hw(sf):
    scores = {}
    for line in sf:
        term, score  = line.split("\t")
        scores[term] = int(score) 

    return scores


def lines(wrd,sent):
    key_p=0
    key_n=0
    key_u=[]
    key=''
    sent_value = 0
    for i in range(len(wrd)-1):
        key=wrd[i].encode('utf-8').translate(None, '():";,.!?').lower()
        if key.find('@') == -1 and key.find('#') == -1 and key.find('/') == -1:
            if key in ('they','i','we','you','she','he','it','rt','a','an','to','at','the','in','on','me','his','hers','ours','theirs','its','and','or','that'):
                print key + ' 0'
            else:
                if sent.has_key(key)==1:
                    if sent[key] >= 0:
                        key_p=key_p + abs(sent[key])
                    else:
                        key_n=key_n + abs(sent[key])                    
                else:
                    key_u.append(key)
                    
    results(key_u,key_p,key_n)
    
def results(kr,kp,kn):
    if kp < kn:
        sign='-'
    else:
        sign=''
    
    if kn == 0:
        key_w=kp
    elif kp == 0:
        key_w=kn
    else:
        key_w=kp/kn
    
    for i in range(len(kr)):
        print kr[i] + ' ' + sign + str(key_w)
    


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