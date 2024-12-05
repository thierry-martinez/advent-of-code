import sys
left_list = []
right_list = []
for line in sys.stdin:
    left, right = line.split()
    left_list.append(int(left))
    right_list.append(int(right))
distance = sum(abs(left - right) for left, right in zip(sorted(left_list), sorted(right_list)))
print(f"Distance: {distance}")
frequences = {}
for right in right_list:
    frequences[right] = frequences.get(right, 0) + 1
similarity = sum(left * frequences.get(left, 0) for left in left_list)
print(f"Similarity: {similarity}")
