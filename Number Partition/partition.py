import bisect
import random
import numpy as np
import math
from optparse import OptionParser
import sys
import os

def residue(seq):
    seq_res = seq.copy()
    seq_res = sorted(seq_res)
    while len(seq_res) > 1:
        diff = seq_res.pop() - seq_res.pop()
        bisect.insort(seq_res, diff)
    return seq_res[0]

def std_res(signs):
    residue = 0
    for i in range(len_seq):
        residue += seq[i] * signs[i]
    return abs(residue)

def part_res(part):
    seq_prime = [0] * len_seq
    for i, item in enumerate(part):
        seq_prime[item] += seq[i]
    return residue(seq_prime)



def rand_res(sol, alg):
    if alg in [1, 2, 3]:
        return std_res(sol)
    else:
        return part_res(sol)
    
def rand_seq(alg):
    if alg in [1, 2, 3]:
        rand_seq = np.random.choice([-1, 1], len_seq).tolist()
    else:
        rand_seq = np.random.choice(len_seq, len_seq).tolist()
    return rand_seq


def rand_indices():
    return np.random.choice(len_seq, 2).tolist()

def rand_neighbor(sol, alg):
    if alg in [1, 2, 3]:
        sol_neigh = sol.copy()
        rand_indxs = rand_indices()
        # print("before $i:" + str(isol_neigh[rand_indxs[0]]))
        # print("before $j:" + str(isol_neigh[rand_indxs[0]]))
        sol_neigh[rand_indxs[0]] = sol_neigh[rand_indxs[0]] * (-1)
        sol_neigh[rand_indxs[1]] = np.random.choice([sol_neigh[rand_indxs[1]] * (-1), sol_neigh[rand_indxs[1]]], 1, p = [0.5, 0.5]).tolist()[0]
        # print("after $i:" + str(isol_neigh[rand_indxs[0]]))
        # print("after $j:" + str(isol_neigh[rand_indxs[0]]))
        return sol_neigh
    else:
        sol_neigh = sol.copy()
        rand_indxs = rand_indices()
        while sol_neigh[rand_indxs[0]] == rand_indxs[1]:
            rand_indxs = rand_indices()
        sol_neigh[rand_indxs[0]] = rand_indxs[1]
        return sol_neigh

def rep_rand(alg):
    rand_sol = rand_seq(alg)
    for i in range(25000):
        rand_sol_prime = rand_seq(alg)
        # print("rand_sol: " + str(rand_sol))
        # print("rand_sol: " + str(rand_sol_prime))
        r1 = rand_res(rand_sol_prime, alg) 
        r2 = rand_res(rand_sol, alg)

        # print("rand_res: " + str(r1))
        # print("rand_res_prime: " + str(r2))

        if r1 < r2:
            rand_sol = rand_sol_prime
    return rand_res(rand_sol, alg)

def hill_climb(alg):
    rand_sol = rand_seq(alg)
    for i in range(25000):
        # print("rand_sol: " + str(rand_sol))
        # print("rand_sol: " + str(rand_sol_prime))
        rand_sol_prime = rand_neighbor(rand_sol, alg)

        r1 = rand_res(rand_sol_prime, alg) 
        r2 = rand_res(rand_sol, alg)

        # print("rand_res: " + str(r1))
        # print("rand_res_prime: " + str(r2))

        if r1 < r2:
            rand_sol = rand_sol_prime
    return rand_res(rand_sol, alg)

def sim_anneal(alg):
    rand_sol = rand_seq(alg)
    rand_sol_2prime = rand_sol

    for i in range(25000):
        rand_sol_prime = rand_neighbor(rand_sol, alg)
        
        # print("rand_sol: " + str(rand_sol))
        # print("rand_sol: " + str(rand_sol_prime))
        # print("rand_sol: " + str(rand_sol_2prime))

        rand_residue = rand_res(rand_sol, alg)
        rand_residue_prime = rand_res(rand_sol_prime, alg)

        if rand_residue_prime < rand_residue:
            rand_sol = rand_sol_prime
        else:
            schedule = (10 ** 10)*((0.8) ** (math.floor(i/300)))
            prob = (math.e) ** ((rand_residue - rand_residue_prime)/schedule)
            rand_bit = np.random.choice([0, 1], 1, p = [prob, 1-prob]).tolist()[0]

            if rand_bit == 0:
                rand_sol = rand_sol_prime

        if rand_res(rand_sol, alg) < rand_res(rand_sol_2prime, alg):
            rand_sol_2prime = rand_sol   
    return rand_res(rand_sol_2prime, alg)


if __name__ == "__main__":

    flag = int(sys.argv[1])
    alg = int(sys.argv[2])
    inputfile = sys.argv[3]

    

    # f = open(inputfile, "a")

    # for i in range(0, 100):
    #     entry = random.randint(0, 10**12)
    #     f.write(str(entry) + "\n")

    # f.close()


    lines = open(inputfile).read().splitlines()
    seq = list(map(lambda x: int(x), lines))
    len_seq = len(seq)

    result = 0

    if alg == 0: 
        result = residue(seq)
    elif alg == 1: 
        result = rep_rand(1)
    elif alg == 2: 
        result = hill_climb(2)
    elif alg == 3: 
        result = sim_anneal(3)
    elif alg == 11: 
        result = rep_rand(11) 
    elif alg == 12: 
        result = hill_climb(12)
    else: 
        result = sim_anneal(13)

    print(result)