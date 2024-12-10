from __future__ import annotations
from dataclasses import dataclass
import itertools
import sys
data = sys.stdin.read().strip()

@dataclass(slots=True)
class Checksum:
    position: int = 0
    checksum: int = 0

    def add(self, id, length):
        self.checksum += id * (2 * self.position + length - 1) * length // 2
        self.position += length

initial, *tail = map(int, data)
blocks = list(enumerate(itertools.batched(tail, 2), start=1))
last_id, (last_free, last_length) = blocks.pop()
checksum = Checksum(position=initial)
while len(blocks) >= 1:
    current_id, (current_free, current_length) = blocks.pop(0)
    while current_free > 0 and last_length > 0:
        count = min(current_free, last_length)
        checksum.add(last_id, count)
        current_free -= count
        last_length -= count
        if last_length == 0:
            if len(blocks) == 0:
                break
            last_id, (last_free, last_length) = blocks.pop()
    checksum.add(current_id, current_length)
checksum.add(last_id, last_length)
print(f"Part 1: {checksum.checksum}")

@dataclass(slots=True)
class Block:
    id: int
    free: int
    length: int
    previous: Block
    next: Block

initial, *tail = map(int, data)

first = Block(0, 0, initial, None, None)
previous = first
for id, (free, length) in enumerate(itertools.batched(tail, 2), start=1):
    block = Block(id, free, length, previous, None)
    previous.next = block
    previous = block

while block:
    target = first
    while target.free < block.length:
        if target.id == block.id:
            target = None
            break
        target = target.next
    previous = block.previous
    if target:
        if target.id != block.id:
            next = block.next
            block.previous.next = next
            if next:
                next.previous = block.previous
                next.free += block.free + block.length
            target.previous.next = block
            block.previous = target.previous
            target.previous = block
            block.next = target
            target.free -= block.length
        elif block.next:
            block.next.free += block.free
        block.free = 0
    block = previous

checksum = Checksum(position=initial)
block = first
while block.next:
    block = block.next
    checksum.position += block.free
    checksum.add(block.id, block.length)
print(f"Part 2: {checksum.checksum}")
    

#blocks = list(enumerate(itertools.batched(tail, 2), start=1))
#checksum = Checksum(position=initial)
#for i in range(len(blocks)):
#    block = blocks[i]
#    if block is None:
#        continue
#    id, (free, length) = block
#    for other in reversed(blocks[i + 1:]):
#        if other is None:
#            continue
#        other_id, (other_free, other_length) = other
#        if other_length <= free:
#            blocks[other_id - 1] = None
#            print((other_id, other_length))
#            checksum.add(other_id, other_length)
#            free -= other_length
#    print((id, length))
#    checksum.add(id, length)
#    checksum.position += free
#print(f"Part 2: {checksum.checksum}")
#i = len(blocks) - 1
#max_id = len(blocks)
#previous_blocks = list(range(0, len(blocks)))
#next_blocks = list(range(1, len(blocks)))
#while i >= 0:
#    id, (free, length) = blocks[i]
#    if id > max_id:
#        i -= 1
#        continue
#    max_id -= 1
#    try:
#        target = next(j for j in range(i + 1) if blocks[j][1][0] >= length)
#    except StopIteration:
#        i -= 1
#        continue
#    #blocks.pop(i)
#
#    if i < len(blocks):
#        next_id, (next_free, next_length) = blocks[i]
#        blocks[i] = next_id, (free + length + next_free, next_length)
#    blocks.insert(target, (id, (0, length)))
#    target_id, (target_free, target_length) = blocks[target + 1]
#    blocks[target + 1] = target_id, (target_free - length, target_length)
#checksum = Checksum(position=initial)
#for id, (free, length) in blocks:
#    checksum.position += free
#    checksum.add(id, length)
#print(f"Part 2: {checksum.checksum}")
