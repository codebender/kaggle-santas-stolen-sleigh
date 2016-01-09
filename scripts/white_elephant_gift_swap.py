# A Brute Approach & Optimizer
## Formerly, Swap and Give Greedy Optimizer

from cHaversine import haversine
import pandas as pd
import numpy as np
import math
import time
import itertools

# The optimization functions
##  - The Magic happens here

north_pole = (90,0)
weight_limit = 1000.0

# def bb_sort(ll):
#     s_limit = 100
#     optimal = False
#     ll = [[0,north_pole,10]] + ll[:] + [[0,north_pole,10]]
#     while not optimal:
#         optimal = True
#         for i in range(1,len(ll) - 2):
#             lcopy = ll[:]
#             lcopy[i], lcopy[i+1] = ll[i+1][:], ll[i][:]
#             if path_opt_test(ll[1:-1])[0] > path_opt_test(lcopy[1:-1])[0]:
#                 print("Sort Swap")
#                 ll = lcopy[:]
#                 optimal = False
#                 s_limit -= 1
#                 if s_limit < 0:
#                     optimal = True
#                     break
#     return ll[1:-1]

def bb_sort2(ll):
    s_limit = 7000
    optimal = False
    ll = [[0,north_pole,10]] + ll[:] + [[0,north_pole,10]]
    while not optimal:
        optimal = True
        for i in range(1,len(ll) - 4):
            lcopy=[[] for _ in range(15)]   # [0] not used
            b=list(itertools.permutations(([0,1,2,3])))

            for k in range(1,15):
                lcopy[k]=ll[:]
                lcopy[k][i], lcopy[k][i+1], lcopy[k][i+2],lcopy[k][i+3] = ll[i+b[k][0]][:], ll[i+b[k][1]][:],ll[i+b[k][2]][:],ll[i+b[k][3]][:]

            minDist=path_opt_test(ll[1:-1])
            kmn=0
            for k in range(1,15):
                tmp=path_opt_test(lcopy[k][1:-1])
                if tmp<minDist:
                    kmn=k
                    minDist=tmp
            if kmn>0:
                print("Sort2 swap")
                ll = lcopy[kmn][:]
                optimal = False
                s_limit -= 1
                if s_limit < 0:
                    optimal = True
                    break
                continue

    return ll[1:-1]

def prev_path_opt(curr,prev):
    curr = [[0,north_pole,10]] + curr[:] + [[0,north_pole,10]]
    prev = [[0,north_pole,10]] + prev[:] + [[0,north_pole,10]]
    for curr_ in range(1,len(curr) - 2):
        for prev_ in range(1,len(prev) - 2):
            lcopy_curr = curr[:]
            lcopy_prev = prev[:]
            lcopy_curr[curr_], lcopy_prev[prev_] = lcopy_prev[prev_][:], lcopy_curr[curr_][:]
            if ((path_opt_test(lcopy_curr[1:-1])[0] + path_opt_test(lcopy_prev[1:-1])[0]) < (path_opt_test(curr[1:-1])[0] + path_opt_test(prev[1:-1])[0])) and path_opt_test(lcopy_curr[1:-1])[2] <=1000 and path_opt_test(lcopy_prev[1:-1])[2] <= 1000:
                print("Trip Swap")
                curr = lcopy_curr[:]
                prev = lcopy_prev[:]
    return [curr[1:-1], prev[1:-1]]

def prev_path_opt_s1(curr,prev):
    curr = [[0,north_pole,10]] + curr[:] + [[0,north_pole,10]]
    prev = [[0,north_pole,10]] + prev[:] + [[0,north_pole,10]]
    for curr_ in range(1,len(curr) - 1):
        for prev_ in range(1,len(prev) - 1):
            lcopy_curr = curr[:]
            lcopy_prev = prev[:]
            if len(lcopy_prev)-1 <= prev_:
                break
            lcopy_curr = lcopy_curr[:curr_+1][:] + [lcopy_prev[prev_]] + lcopy_curr[curr_+1:][:]
            lcopy_prev.pop(prev_)
            if ((path_opt_test(lcopy_curr[1:-1])[0] + path_opt_test(lcopy_prev[1:-1])[0]) <= (path_opt_test(curr[1:-1])[0] + path_opt_test(prev[1:-1])[0])) and path_opt_test(lcopy_curr[1:-1])[2] <=1000 and path_opt_test(lcopy_prev[1:-1])[2] <= 1000:
                print("Trip Swap - Give to current")
                curr = lcopy_curr[:]
                prev = lcopy_prev[:]
    return [curr[1:-1], prev[1:-1]]

def split_trips(curr):
    prev = []
    curr = [[0,north_pole,10]] + curr[:] + [[0,north_pole,10]]
    for curr_ in range(1,len(curr) - 1):
        lcopy_curr = curr[:]
        if len(lcopy_curr)-1 <=curr_:
            break
        lcopy_prev = [[0,north_pole,10]] + [lcopy_curr[curr_]] + [[0,north_pole,10]]
        lcopy_curr.pop(curr_)
        if ((path_opt_test(lcopy_curr[1:-1])[0] + path_opt_test(lcopy_prev[1:-1])[0]) < (path_opt_test(curr[1:-1])[0])):
            print("Trip Split")
            curr = lcopy_curr[:]
            prev = lcopy_prev[:]
    return [curr[1:-1], prev[1:-1]]

def path_opt_test(llo):
    f_ = 0.0
    d_ = 0.0
    we_ = 0.0
    l_ = north_pole
    for i in range(len(llo)):
        d_ += haversine(l_, llo[i][1])
        we_ += llo[i][2]
        f_ += d_ * llo[i][2]
        l_ = llo[i][1]
    d_ += haversine(l_, north_pole)
    f_ += d_ * 10
    return [f_,d_,we_]

gifts = pd.read_csv("../input/gifts.csv").fillna(" ")

for n in range(15):
    Submission = pd.read_csv("../submissions/clusters_" + str(n) +".csv").fillna(" ")
    Submission = pd.merge(Submission, gifts, how='left', on=['GiftId'])

    ou_ = open("../submissions/clusters_" + str(n+1) + ".csv","w")
    ou_.write("TripId,GiftId\n")
    d = {}
    previous_trip = []
    Submission['colFromIndex'] = Submission.index
    Submission = Submission.sort_index(by=['TripId', 'colFromIndex'], ascending=[True, True])
    uniq_trips = Submission.TripId.unique()

    for s_ in range(len(uniq_trips)):
        trip = Submission[(Submission['TripId']==(uniq_trips[s_]))].copy()
        trip = trip.reset_index()

        b = []
        for x_ in range(len(trip.GiftId)):
            b.append([trip.GiftId[x_],(trip.Latitude[x_],trip.Longitude[x_]),trip.Weight[x_]])
        d[str(s_+1)] = [path_opt_test(b)[0],b[:]]

    for s_ in range(len(uniq_trips)):
        key = str(s_+1)
        if d[key][0] >= 1:
            for i in range(-(n+1),(n+2),1):
                r_ = int(key)+i
                if r_ <= 0:
                    r_ = len(uniq_trips) + i
                if r_ > len(uniq_trips):
                    r_ = i
                if r_ != int(key):
                    print(r_,key,i)
                    previous_trip=d[str(r_)][1][:]
                    b = d[key][1][:]
                    previous_trip, b = prev_path_opt(previous_trip, b)
                    previous_trip, b = prev_path_opt_s1(previous_trip, b)
                    #previous_trip, b = split_trips(b) - Need to increment trip Ids by one to keep sequence for further optimization
                    #previous_trip = bb_sort(previous_trip)
                    #b = bb_sort(b)
                    previous_trip = bb_sort2(previous_trip)
                    b = bb_sort2(b)
                    d[str(r_)][1]=previous_trip[:]
                    d[key][1]=b[:]
    for key in d:
        b = d[key][1][:]
        for x_ in range(len(b)):
            ou_.write(str(key)+","+str(b[x_][0])+"\n")
    ou_.close()
