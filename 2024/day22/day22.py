import sys

initial_secret_numbers = [int(line.strip()) for line in sys.stdin]

def mix(secret_number, value):
    return secret_number ^ value

assert mix(42, 15) == 37

def prune(secret_number):
    return secret_number % 16777216

assert prune(100000000) == 16113920

def next_secret_number(secret_number):
    result = secret_number * 64
    secret_number = mix(result, secret_number)
    secret_number = prune(secret_number)
    result = secret_number // 32
    secret_number = mix(result, secret_number)
    secret_number = prune(secret_number)
    result = secret_number * 2048
    secret_number = mix(result, secret_number)
    secret_number = prune(secret_number)
    return secret_number

assert next_secret_number(123) == 15887950
assert next_secret_number(15887950) == 16495136

def part1(initial_secret_numbers):
    s = 0
    for secret_number in initial_secret_numbers:
        for _ in range(2000):
            secret_number = next_secret_number(secret_number)
        s += secret_number
    return s

print(f"Part 1: {part1(initial_secret_numbers)}")

def part2(initial_secret_numbers):
    all_sequences = {}
    for secret_number in initial_secret_numbers:
        buyer_sequences = {}
        sequence = []
        previous_price =  secret_number % 10
        for _ in range(2000):
            secret_number = next_secret_number(secret_number)
            new_price = secret_number % 10
            changes = new_price - previous_price
            previous_price = new_price
            sequence.append(changes)
            if len(sequence) > 4:
                sequence.pop(0)
            if len(sequence) == 4:
                sequence_tuple = tuple(sequence)
                previous = buyer_sequences.get(sequence_tuple)
                if previous is None:
                    buyer_sequences[sequence_tuple] = new_price
        for sequence, price in buyer_sequences.items():
            all_sequences[sequence] = all_sequences.get(sequence, 0) + price
    return max(all_sequences.items(), key=lambda p: p[1])[1]

print(f"Part 2: {part2(initial_secret_numbers)}")
