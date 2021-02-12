import random
import math

def count_indices_uniformity(ls):
    dist = ls[-1] - ls[0]
    answer = 0
    for x, y in zip(ls, ls[1:]):
        p = float(y - x) / dist
        answer -= math.log(p)
    return answer

def count_uniformity(ls):
    uniq = set(ls)
    answer = 0
    for x in uniq:
        indices = [-1]
        for i, y in enumerate(ls):
            if x == y:
                indices.append(i)
        indices.append(len(ls))
        answer += count_indices_uniformity(indices)
    return answer

def explode(ls):
    result = []
    for (i, x) in enumerate(ls):
        for _ in range(x):
            result.append(i)
    return result

def print_score(ls):
    score = count_uniformity(ls)
    print(f'{ls} = {score}')

def optimize(ls, tries=100):
    score = count_uniformity(ls)
    answer = ls[:]
    for _ in range(tries):
        random.shuffle(ls)
        new_score = count_uniformity(ls)
        if new_score < score:
            score = new_score
            answer = ls[:]
    return answer

if __name__ == '__main__':
    ls = explode([2, 4, 3, 1])
    print_score(optimize(ls))
