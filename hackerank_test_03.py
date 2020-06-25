# 이렇게 쉬운문제를...
# 0 : 밟을 수 있는 구름 / 1: 피해야하는 천둥번개 
# input 배열은 [0,0,1,0,0,1,0,0] 이런식으로 0과 1로 이루어진 데이터
# 천둥번개는 밟지않으면서 점프를 최소화하는 최적의 jump횟수를 구하는 로직


def jumpingOnClouds(c):  
    # 시작시점
    idx = 0
    # 점프한 위치를 찍기위한 배열로 c의 길이만큼 만들어줌(초기화작업)
    zero = [0]*len(c)
    
    # 점프간격은 +2씩 감으로 idx값이 len(c)-2보다 작을때까지만 돌게끔 설계
    while  idx < len(c)-2:
        # 만약 현재위치의 두 단계 점프를 뛰는 곳이 천둥번개라면 앞에 구름으로 점프
        if c[idx+2] == 1:
            idx = idx+1
            zero[idx] = 1
        # 만약 현재위치의 두 단계 점프 뛰는곳이 구름이라면 한단계 거치고 바로 다음으로 2단계 점프 
        else:
            idx = idx+2
            zero[idx] = 1
    # 마지막 점프위치에 왔으면 도착해야하니 무조건 점프 
    else:
    
        zero[len(c)-1] = 1
    #최종 점프횟수 sum 한후 결과 return
    return sum(zero)