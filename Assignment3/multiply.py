import MapReduce
import sys

"""
Trim DNA Example in the Simple Python MapReduce Framework
"""

mr = MapReduce.MapReduce()

# =============================
# Do not modify above this line

def mapper(record):
    # key: document identifier
    # value: document contents
    matrix=record[0]
    i=record[1]
    j=record[2]
    value=record[3]
    if matrix=='a':
        k=0
        while k <= 4:
            mr.emit_intermediate((i,k),[j,value])
            k+=1            
    if matrix=='b':
        k=0
        while k <= 4:
            mr.emit_intermediate((k,j),[i,value])
            k+=1
    

def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
    mult={}
    for val in list_of_values:
        if mult.has_key(val[0])==1:
            mult[val[0]]=[mult[val[0]][0]*val[1],1]
        else:
            mult[val[0]]=[val[1],0]
    sum=0
    for el in mult:
        sum = mult[el][0]*mult[el][1] + sum
        
    mr.emit((key[0],key[1],sum))

# Do not modify below this line
# =============================
if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)
