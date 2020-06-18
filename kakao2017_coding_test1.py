#  [ 2017 신입공채 코딩테스트 ] 
# (1). 비밀 지도(난이도: 하)
# 네오는 평소 프로도가 비상금을 숨겨놓는 장소를 알려줄 비밀지도를 손에 넣었다. 
# 그런데 이 비밀지도는 숫자로 암호화되어 있어 위치를 확인하기 위해서는 암호를 해독해야 한다. 
# 다행히 지도 암호를 해독할 방법을 적어놓은 메모도 함께 발견했다.
# 
# 1. 지도는 한 변의 길이가 n인 정사각형 배열 형태로,
#    각 칸은 “공백”(“ “) 또는 “벽”(“#”) 두 종류로 이루어져 있다.
# 2. 전체 지도는 두 장의 지도를 겹쳐서 얻을 수 있다. 각각 “지도 1”과 “지도 2”라고 하자.
#    지도 1 또는 지도 2 중 어느 하나라도 벽인 부분은 전체 지도에서도 벽이다. 
#    지도 1과 지도 2에서 모두 공백인 부분은 전체 지도에서도 공백이다.
# 3. “지도 1”과 “지도 2”는 각각 정수 배열로 암호화되어 있다.
# 4. 암호화된 배열은 지도의 각 가로줄에서 벽 부분을 1, 공백 
#    부분을 0으로 부호화했을 때 얻어지는 이진수에 해당하는 값의 배열이다.

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
