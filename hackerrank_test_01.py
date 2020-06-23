## 양말의 개수와 컬러를 파악한 뒤 짝이맞는 양말의 pair는 몇개인지 찾아라
#n = 양말의개수
#ar = 컬러
import math
import os
import random
import re
import sys
from collections import Counter

# Complete the sockMerchant function below.
def sockMerchant(n, ar):
    num = n
    color = ar
    pair_list = []
    cnt = Counter(ar).values()
    for i in cnt :
        if i > 1:
            values = math.trunc(i/2)
            pair_list.append(values)
        else:
            pass
    return sum(pair_list)
    
if __name__ == '__main__':
    n = 9    
    ar = [10 ,20, 20, 10, 10, 30, 50, 10,20]
    result = sockMerchant(n, ar)
    print(result)
