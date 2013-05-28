import MapReduce
import sys

"""
Asymmetric friendship Example in the Simple Python MapReduce Framework
"""

mr = MapReduce.MapReduce()

# =============================
# Do not modify above this line

def mapper(record):
    # key: document identifier
    # value: document contents
    key = record[0]
    value = record[1]
    mr.emit_intermediate(key, record)
    mr.emit_intermediate(value, record)

def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
    counts={}
    for value in list_of_values:
        A=value[0]
        B=value[1]
        if A==key:
            if counts.has_key(B)==1:
                counts[B] = counts[B] + 1
            else:
                counts[B]=1
        elif B==key:
            if counts.has_key(A)==1:
                counts[A] = counts[A] + 1
            else:
                counts[A]=1 

    for fr in counts:
        if counts[fr]==1:
            mr.emit((key,fr))

# Do not modify below this line
# =============================
if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)
