# 계곡의 밸리의 개수를 찾는문제
# U는 Uphill D는 Downhill
# Valley는 연속적으로 Down되는 sequence를 뜻함 
def counting_valley(n, paths):
    level = cnt_valley = 0
    for i in paths:
        if i == 'U':
            level += 1
        else :
            level -= 1
        if(level == 0 ) & (i == 'U'):
            cnt_valley += 1
    return cnt_valley


n = 8
steps = 'UDDDUDUU'
# steps = 'DDUUUUDD'
print(steps)
counting_valley(n,steps)
