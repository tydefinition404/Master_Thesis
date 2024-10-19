import numpy as np
import math

# functions to execute dominant sets clustering algorithm
# http://homepage.tudelft.nl/3e2t5/HungKrose_ICMI2011.pdf

def symmetrize_A(A, op):
	if op == 'max':
		W = np.maximum( A, A.transpose() )
	elif op== 'min':
		W = np.minimum(A, A.transpose())
	elif op =='avg':
		W = (A + A.transpose())/2
	return W

# d-sets function k
def k(S, i, A):
	sum_affs = 0
	for j in range(len(S)):
		if S[j]:
			sum_affs += A[i,j]

	return 1/np.sum(S) * sum_affs

# d-sets function phi
def phi(S,i,j,A):
	return A[i,j] - k(S,i,A)

# d-sets function weight
# w_S{i}
def weight(S, i, A):
	if np.sum(S) == 1:
		return 1
	else:
		R = S.copy()
		R[i] = False #maybe useless
		sum_weights = 0
		for j in range(len(R)):
			if R[j]:
				sum_weights += phi(R,j,i,A) * weight(R, j, A)
		return sum_weights

## optimization function
def f(x, A):
	return np.dot(x.T, np.dot(A, x))

def optimizier(eps, x, A):
	while (eps > 1e-15):
		p = f(x,A)
		x = np.multiply(x, np.dot(A,x)) / np.dot(x, np.dot(A,x))
		n = f(x,A)
		eps = abs(n-p)
	return x
## iteratively finds vector x which maximizes f
def vector_climb(A, allowed, num_nodes, original_A, thres=1e-5):
	# x = np.random.uniform(0,1,num_nodes)
	x =  np.ones(num_nodes) * 1/num_nodes
	# x = 1/num_nodes
	x = np.multiply(x, allowed) 
	# x = 0 if not allowed
	eps = 10
	# n = 10
	x = optimizier(eps, x,  A)

	groups = x > thres
	# one member is valid if has a proper weighted affinity
 

	for i in range(num_nodes):
		if not allowed[i]:
			if weight(groups,i,original_A) > 0.0:
				return []
	return groups


# Finds vectors x of people which maximize f. Then removes those people and repeats
# Stop if:
# 1. No affinities among existing nodes
# 2. No valid groups can be found
# 3. Only one node is allowed
def iterate_climb_learned(A, num_nodes):
	allowed = np.ones(num_nodes)
	groups = []

	original_A = A.copy()
	while (np.sum(allowed) > 1):
		A[allowed == False] = 0
		A[:,allowed == False] = 0
		if (np.sum(np.dot(allowed,A)) == 0):
			break
		x = vector_climb(A, allowed, num_nodes, original_A, thres=1e-5)
		if len(x) == 0:
			break
		groups.append(x)
		allowed = np.multiply(x == False, allowed)
	return groups
	
def dominant_set_extraction(A,num_nodes,op='min'):
	A_= symmetrize_A(A, op)
	# print("symmetric A:", A_)
	groups = iterate_climb_learned(A_,num_nodes)
	return groups

# Groups according to the algorithm in "Recognizing F-Formations in the Open World"
# https://ieeexplore.ieee.org/abstract/document/8673233
def naive_group(predictions, frame, n_people, thres, n_features):
	groups = []

	A = learned_affinity(n_people, predictions, frame, n_features)
	A = np.random.randn(n_people, n_people)
	A = A > .5
	for i in range(n_people):
		A[i, i] = True

	A = 1 * A
	while np.sum(A) > 0:
		most_overlap = -float("inf")
		pos = (-1, -1)

		for i in range(n_people-1):
			B_i = A[i]
			for j in range(i+1, n_people):
				B_j = A[j]
				overlap = B_i.dot(B_j)
				
				if overlap > most_overlap:
					most_overlap = overlap
					pos = (i, j)
		if most_overlap <= 0:
			break

		group = (A[pos[0]] + A[pos[1]]) > .5
		groups.append(group)
		for i in range(n_people):
			if group[i]:
				A[i, :] = 0
				A[:, i] = 0

	return groups

