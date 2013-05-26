import MapReduce
import sys


"""
Inverted Index Example in the Simple Python MapReduce Framework
"""

mr = MapReduce.MapReduce()

# =============================
# Do not modify above this line

def mapper(record):
    # key: document identifier
    # value: document contents
    counts = {}    
    key = record[0]
    value = record[1]
    words = value.split()
    #counting words
    for w in words:
        if counts.has_key(w)==1:
            counts[w] = counts[w] + 1
        else:
            counts[w] = 1
    #sending not duplicates
    for wrd in counts:
        mr.emit_intermediate(wrd,key)


def reducer(key,values):
    #key: word within the text
    #values: list of documents identifiers
    mr.emit((key,values))

# Do not modify below this line
# =============================
if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)