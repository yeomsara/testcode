import pandas as pd
import numpy as np
import random

arr1 = [9,20,28,18,11]
arr2 = [30,1,21,17,28]
n = 5
# arr1 = [46,33,32,22,31,50]
# arr2 = [27,56,19,14,14,10]
# n = 6

def cal_map(arr):
    result_list = []
    for xx in arr:
        aa = str(bin(xx)[2:])
        if len(aa) < n :
            result_list.append(int(aa.zfill(5)))
        else :
            result_list.append(int(aa))
    return result_list

    
result_1 = cal_map(arr1) 
result_2 = cal_map(arr2)

map_solution = list(str(x+y) for x, y in list(zip(result_1, result_2)))

solution = [x.replace('1','#').replace('0',' ').replace('2','#') for x in map_solution]
print(solution)
