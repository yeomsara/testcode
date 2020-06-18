# 카카오 2017 신입공채 문제 2번 
#카카오톡 게임별의 하반기 신규 서비스로 다트 게임을 출시하기로 했다.
#다트 게임은 다트판에 다트를 세 차례 던져 그 점수의 합계로 실력을 겨루는 게임으로, 모두가 간단히 즐길 수 있다.
#갓 입사한 무지는 코딩 실력을 인정받아 게임의 핵심 부분인 점수 계산 로직을 맡게 되었다. 
#다트 게임의 점수 계산 로직은 아래와 같다.

#1. 다트 게임은 총 3번의 기회로 구성된다.
#2. 각 기회마다 얻을 수 있는 점수는 0점에서 10점까지이다.
#3. 점수와 함께 Single(S), Double(D), Triple(T) 영역이 존재하고 각 영역 당첨 시 
#   점수에서 1제곱, 2제곱, 3제곱 (점수^1 , 점수^2 , 점수^3 )으로 계산된다.

#4. 옵션으로 스타상(*) , 아차상(#)이 존재하며 스타상(*) 당첨 시 
#   해당 점수와 바로 전에 얻은 점수를 각 2배로 만든다. 아차상(#) 당첨 시 해당 점수는 마이너스된다.
#5. 스타상(*)은 첫 번째 기회에서도 나올 수 있다. 이 경우 첫 번째 스타상(*)의 점수만 2배가 된다. (예제 4번 참고)
#6. 스타상(*)의 효과는 다른 스타상(*)의 효과와 중첩될 수 있다. 이 경우 중첩된 스타상(*) 점수는 4배가 된다. (예제 4번 참고)
#7. 스타상(*)의 효과는 아차상(#)의 효과와 중첩될 수 있다. 이 경우 중첩된 아차상(#)의 점수는 -2배가 된다. (예제 5번 참고)
#8. Single(S), Double(D), Triple(T)은 점수마다 하나씩 존재한다.9 스타상(*), 아차상(#)은 점수마다 둘 중 하나만 존재할 수 있으며, 존재하지 않을 수도 있다.
#9. 0~10의 정수와 문자 S, D, T, *, #로 구성된 문자열이 입력될 시 총점수를 반환하는 함수를 작성하라.

def find_option_index(opt_index_list,score_index,score_list):
    opt_index = []
    if len(opt_index_list) > 1:
        for i in opt_index_list :
            if score_index[0][1] < i < score_index[1][0]:
                opt_index.append(0)
            elif  score_index[1][1] < i < score_index[2][0]:
                opt_index.append(1)
            elif i == len(score_list)-1:
                opt_index.append(2)
    elif len(opt_index_list) == 1:
        if score_index[0][1] < opt_index_list[0] < score_index[1][0]:
            opt_index.append(0)
        elif  score_index[1][1] < opt_index_list[0] < score_index[2][0]:
            opt_index.append(1)
        elif opt_index_list[0] == len(score_list)-1:
            opt_index.append(2)
    else :
        pass

    return opt_index

def calc_option(score_list):
    score_index   = [[m.start(),m.end()] for m in re.finditer(r'[0-9]+', score_list)]
    score         = [int(m[0]) for m in re.finditer(r'[0-9]+', score_list)]
    ## score의 카테고리를 찾아 변환
    score_cat     = [m[0] for m in re.finditer(r'[a-zA-Z]+', score_list)]
    cat_dic       = {'S':1,'D':2,'T':3}
    score_cat_pow = [cat_dic.get(n,n) for n in score_cat]
    ## score option점수 계산 
    star_index    = [m.start() for m in re.finditer('\*', score_list)]
    miss_index    = [m.start() for m in re.finditer('\#', score_list)]
    option_dic    = {'*':2,'#':-1}
    option_index  = star_index+miss_index
    option_list   = [score_list[i] for i in option_index]
    ## * 이면 2 #이면 -1을 보너스점수로 
    option_calc   = [option_dic.get(n,n) for n in option_list]

    comp_option_index = find_option_index(option_index,score_index,score_list)
    calc_score = [pow(score[i],score_cat_pow[i]) for i in range(0,len(score))]

    result_score = 0
    if(len(option_index)) >= 1:
        for i in range(0,len(comp_option_index)):
            opt_loc = int(comp_option_index[i])
            # 옵션이 있는 위치에서 -1을 뺏을때 -1일경우 첫번째 점수에 붙은 옵션이라는 의미
            # * 옵션의경우 2번째 점수에서 부터 중첩이 되고 #은 아니기에 *만 예외처리
            if (option_list[i] == '*') &(comp_option_index[i]-1 != -1):
                calc_score[opt_loc-1] = calc_score[opt_loc-1]*option_calc[i]
                calc_score[opt_loc] = calc_score[opt_loc]*option_calc[i]
            else:
                calc_score[opt_loc] = calc_score[opt_loc]*option_calc[i]
                
    else:
        pass
    
    print('======================================')
    print('☆ score_list   : ',score_list)
    print('=========== default score ============')
    print('SCORE        : ',score)
    print('score_cat    : ',score_cat)
    print('score_pow    : ',score_cat_pow)
    print('Default score : %s^%s+%s^%s+%s^%s'%(score[0],score_cat_pow[0],score[1],score_cat_pow[1],score[2],score_cat_pow[2]))
    print('calc_original_score   : ',calc_score)
    print('============= option cal =============')
    print('option_list  : ',option_list)
    print('option_index : ',option_index)
    print('option_calc  : ',option_calc)
    print('( 0 : first game / 1 : second game / 2 : last game )')
    print('comp_option_index : ',comp_option_index)
    result_score = sum(calc_score)
    
    return result_score
    

score_list    = ['1S2D*3T','1D2S#10S','1D2S0T','1S*2T*3S','1D#2S*3S','1T2D3D#','1D2S3T*','10S8D*1S','10S*10D#10T*']

for i in score_list:
    result_score = calc_option(i)
    print("★ result score : %s"%result_score)
    print('======================================\n')
    
    
    
