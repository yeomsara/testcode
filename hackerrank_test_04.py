# arr value permutation calculate 
# find min / max


import itertools 

arr = [1,3,5,7,9]

result = []

for i in range(0,len(arr)):
    print(i)
    sum_arr = arr[:i] + arr[i+1:]
    print(arr[:i])
    print(arr[i+1:])
    result.append(sum(sum_arr))
    print(result)
    print(min(result),max(result))
    
print(min(result),max(result))
   

    
