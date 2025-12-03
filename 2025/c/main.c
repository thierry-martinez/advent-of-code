#include <stdio.h>

int main(int argc, char **argv) {
  char direction;
  unsigned int count;
  unsigned int count_zero = 0;
  unsigned int dial = 50;
  while (scanf("%c%d\n", &direction, &count) == 2) {
    int new_dial;
    if (direction == 'L') {
      new_dial = dial - count;
    }
    else {
      new_dial = dial + count;
    }
    new_dial = new_dial % 100;
    if (new_dial < 0) {
      new_dial += 100;
    }
    if (new_dial == 0) {
      count_zero += 1;
    }
    dial = new_dial;
  }
  printf("Answer: %d\n", count_zero);
}
