/* game24 - Solves a game 24 scenario
 * Copyright (C) 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
 * Copyright (C) 2020 Tom Couperus <tcouperus@hotmail.nl>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
  number_count = 4,
  ops_count = number_count - 1,
  all_count = number_count + ops_count
};

enum OperatorKind { op_add, op_sub, op_mul, op_div };

struct Operator {
  enum OperatorKind kind;
  unsigned char lhs, rhs;
};
enum NodeKind { node_number, node_operator };
struct Node {
  enum NodeKind kind;
  union {
    int n;
    struct Operator op;
  } v;
};

typedef struct Node SyntaxTree[all_count];
typedef struct {
  int num;
  bool valid;
} EvalResult;

static EvalResult makeNumber(int number) {
  return (EvalResult){.num = number, .valid = true};
}
static EvalResult makeInvalid() {
  return (EvalResult){.num = -1, .valid = false};
}

static EvalResult evalSyntaxTree(const SyntaxTree tree,
                                 const struct Node *curNode) {
  switch (curNode->kind) {
  case node_number:
    return makeNumber(curNode->v.n);
  case node_operator: {
    EvalResult lhs = evalSyntaxTree(tree, tree + curNode->v.op.lhs),
               rhs = evalSyntaxTree(tree, tree + curNode->v.op.rhs);
    if (!lhs.valid || !rhs.valid) {
      return makeInvalid();
    }
    switch (curNode->v.op.kind) {
    case op_add:
      return makeNumber(lhs.num + rhs.num);
    case op_sub:
      return makeNumber(lhs.num - rhs.num);
    case op_mul:
      return makeNumber(lhs.num * rhs.num);
    case op_div:
      return rhs.num != 0 ? makeNumber(lhs.num / rhs.num) : makeInvalid();
    }
  }
  }
  __builtin_unreachable();
}

static void printSyntaxTreeImpl(const SyntaxTree tree,
                                const struct Node *curNode) {
  static const char opChars[4] = {'+', '-', '*', '/'};
  switch (curNode->kind) {
  case node_number:
    printf("%d", curNode->v.n);
    break;
  case node_operator:
    putchar('(');
    printSyntaxTreeImpl(tree, tree + curNode->v.op.lhs);
    printf(" %c ", opChars[curNode->v.op.kind]);
    printSyntaxTreeImpl(tree, tree + curNode->v.op.rhs);
    putchar(')');
  }
}
static void printSyntaxTree(const SyntaxTree tree, const struct Node *curNode) {
  printSyntaxTreeImpl(tree, curNode);
  putchar('\n');
}

static void incrementOperators(enum OperatorKind ops[ops_count]) {
  ++ops[0];
  if (ops[0] != op_div + 1)
    return;
  ops[0] = op_add;
  ++ops[1];
  if (ops[1] != op_div + 1)
    return;
  ops[1] = op_add;
  ++ops[2];
  if (ops[2] != op_div + 1)
    return;
  ops[2] = op_add;
}

static void swap(char *a, char *b) {
  int c = *a;
  *a = *b;
  *b = c;
}

static int compareOps(const enum OperatorKind lhs[ops_count],
                      const enum OperatorKind rhs[ops_count]) {
  for (int i = 0; i < ops_count; ++i) {
    if (lhs[i] != rhs[i]) {
      return 0;
    }
  }
  return 1;
}

static void iterateAllSyntaxTrees(const int numbers[4],
                                  void (*callback)(const SyntaxTree tree,
                                                   const struct Node *root,
                                                   void *data),
                                  void *data) {
  SyntaxTree tree;
  static const enum OperatorKind finalOps[ops_count] = {op_div, op_div, op_div};
  enum OperatorKind ops[ops_count] = {op_add, op_add, op_add};

  for (; !compareOps(ops, finalOps); incrementOperators(ops)) {
#define FOR_VAR(name, top) for (int name = 0; name < top; ++name)
#define FOR_OPERAND(name, top)                                                 \
  FOR_VAR(name##_lhs, top) FOR_VAR(name##_rhs, top - 1)
    FOR_OPERAND(first, 4) FOR_OPERAND(second, 3) FOR_OPERAND(third, 2) {
      // Setup the tree
      for (int i = 0; i < 4; ++i) {
        tree[i] = (struct Node){.kind = node_number, {.n = numbers[i]}};
      }
      for (int i = 0; i < 3; ++i) {
        tree[i + 4] =
            (struct Node){.kind = node_operator, {.op = {ops[i], -1, -1}}};
      }
      char itab[7] = {0, 1, 2, 3, 4, 5, 6};
      int arenaRight = 4;
      int curNode = 4;
      struct Node *curOperator = tree + 4;
      curOperator->v.op.lhs = itab[first_lhs];
      swap(itab + first_lhs, itab + --arenaRight);
      curOperator->v.op.rhs = itab[first_rhs];
      swap(itab + first_rhs, itab + curNode++);
      ++curOperator;
      curOperator->v.op.lhs = itab[second_lhs];
      swap(itab + second_lhs, itab + --arenaRight);
      curOperator->v.op.rhs = itab[second_rhs];
      swap(itab + second_rhs, itab + curNode++);
      ++curOperator;
      curOperator->v.op.lhs = itab[third_lhs];
      swap(itab + third_lhs, itab + --arenaRight);
      curOperator->v.op.rhs = itab[third_rhs];
      swap(itab + third_rhs, itab + curNode++);
      ++curOperator;
      callback(tree, tree + 6, data);
    }
  }
}

static void findCommutativeOperator(SyntaxTree tree, struct Node *current);
static void findAdjacentNodes(SyntaxTree tree, struct Node *current, int opKind,
                              unsigned char ***arrIdxPtr);

static void analyzeCommutativeOperand(SyntaxTree tree, unsigned char *nodeIdx,
                                      int opKind, unsigned char ***arrIdxPtr) {
  if (tree[*nodeIdx].kind == node_number) {
    *(*arrIdxPtr)++ = nodeIdx;
  } else if (tree[*nodeIdx].v.op.kind == opKind) {
    findAdjacentNodes(tree, &tree[*nodeIdx], opKind, arrIdxPtr);
  } else {
    *(*arrIdxPtr)++ = nodeIdx;
    findCommutativeOperator(tree, &tree[*nodeIdx]);
  }
}

static void findAdjacentNodes(SyntaxTree tree, struct Node *current, int opKind,
                              unsigned char ***arrIdxPtr) {
  analyzeCommutativeOperand(tree, &current->v.op.lhs, opKind, arrIdxPtr);
  analyzeCommutativeOperand(tree, &current->v.op.rhs, opKind, arrIdxPtr);
}

static void swapBubbleSort(unsigned char **pos1, unsigned char **pos2) {
  unsigned char temp = **pos1;
  **pos1 = **pos2;
  **pos2 = temp;
}

static void bubbleSort(unsigned char **first, unsigned char **last) {
  bool swapped = false;
  while (!swapped) {
    swapped = true;
    for (unsigned char **pos = first + 1; pos != last; ++pos) {
      if (**pos < **(pos - 1)) {
        swapBubbleSort(pos, pos - 1);
        swapped = false;
      }
    }
  }
}

static unsigned char **findNullPointer(unsigned char **first,
                                       unsigned char **last) {
  while (first != last) {
    if (*first != NULL) {
      first++;
    } else {
      break;
    }
  }
  return first;
}

static void findCommutativeOperator(SyntaxTree tree, struct Node *current) {
  if (current->kind == node_number) {
    return;
  }
  if (current->v.op.kind == op_add || current->v.op.kind == op_mul) {
    unsigned char *operandIdxPtrs[ops_count * 2] = {NULL};
    unsigned char **operandIdxPtrsIndex = operandIdxPtrs;
    findAdjacentNodes(tree, current, current->v.op.kind, &operandIdxPtrsIndex);
    bubbleSort(operandIdxPtrs,
               findNullPointer(operandIdxPtrs, operandIdxPtrs + ops_count * 2));
  } else {
    findCommutativeOperator(tree, tree + current->v.op.lhs);
    findCommutativeOperator(tree, tree + current->v.op.rhs);
  }
}

static void canonicalizeTree(SyntaxTree tree, struct Node *root) {
  findCommutativeOperator(tree, root);
}

static char *linearSearch(char *start, char goal) {
  while (*start != goal) {
    start++;
  }
  return start;
}

// Linux kernel inspired compile time assert
#define STATIC_ASSERT(e) (void)(sizeof(struct { int : -!!(e); }))

static uint16_t hashTree(SyntaxTree tree, struct Node *root) {
  union hashTree {
    struct repr {
      unsigned int first_kind : 2;
      unsigned int second_kind : 2;
      unsigned int third_kind : 2;
      unsigned int first_left : 2;
      unsigned int first_right : 2;
      unsigned int second_left : 2;
      unsigned int second_right : 1;
      unsigned int third_left : 1;
    } bits;
    uint16_t hash;
  } hashTree;
  STATIC_ASSERT(sizeof(hashTree.bits) == sizeof(hashTree.hash));
  char itab[7] = {0, 1, 2, 3, 4, 5, 6};
  int arenaRight = 4;
  int curNode = 4;
  struct Node *curOperator = tree + 4;
  hashTree.bits.first_kind = curOperator->kind;
  hashTree.bits.first_left = linearSearch(itab, curOperator->v.op.lhs) - itab;
  swap(itab + hashTree.bits.first_left, itab + --arenaRight);
  hashTree.bits.first_right = linearSearch(itab, curOperator->v.op.rhs) - itab;
  swap(itab + hashTree.bits.first_right, itab + curNode++);
  ++curOperator;
  hashTree.bits.second_kind = curOperator->kind;
  hashTree.bits.second_left = linearSearch(itab, curOperator->v.op.lhs) - itab;
  swap(itab + hashTree.bits.second_left, itab + --arenaRight);
  hashTree.bits.second_right = linearSearch(itab, curOperator->v.op.rhs) - itab;
  swap(itab + hashTree.bits.second_right, itab + curNode++);
  ++curOperator;
  hashTree.bits.third_kind = curOperator->kind;
  hashTree.bits.third_left = linearSearch(itab, curOperator->v.op.lhs) - itab;
  swap(itab + hashTree.bits.third_left, itab + --arenaRight);
  return hashTree.hash;
}

struct SharedState {
  uint16_t *seenTrees;
  size_t size, capacity;
};

static uint16_t *upperBound(uint16_t *first, uint16_t *last, uint16_t hash) {
  while (first + 1 < last) {
    uint16_t *mid = first + (last - first) / 2;
    if (*mid == hash) {
      return mid;
    } else if (*mid < hash) {
      first = mid;
    } else {
      last = mid;
    }
  }
  if (*first <= hash) {
    return first + 1;
  } else {
    return first;
  }
}

static void insert(uint16_t hash, uint16_t *pos, struct SharedState *state) {
  if (state->size == state->capacity) {
    state->capacity *= 2;
    state->seenTrees =
        realloc(state->seenTrees, sizeof(uint16_t) * state->capacity);
  }
  memmove(pos + 1, pos, state->seenTrees + state->size - pos);
  *pos = hash;
}

static void checkAndPrintCallback(const SyntaxTree tree,
                                  const struct Node *root, void *data) {
  const EvalResult res = evalSyntaxTree(tree, root);
  if (res.valid && res.num == 24) {
    SyntaxTree copy;
    memcpy(&copy, tree, sizeof(SyntaxTree));
    struct Node *rootCopy = copy + all_count - 1;
    canonicalizeTree(copy, rootCopy);
    uint16_t hash = hashTree(copy, rootCopy);
    struct SharedState *state = data;
    uint16_t *pos =
        upperBound(state->seenTrees, state->seenTrees + state->size, hash);
    if (*pos != hash) {
      printSyntaxTree(copy, rootCopy);
      insert(hash, pos, state);
    }
  }
}

int main() {
  int numbers[number_count];
  for (int i = 0; i < number_count; ++i) {
    const int code = scanf("%d", numbers + i);
    if (code != 1) {
      fprintf(stderr, "error: Input is malformed, scanf() returned %d\n", code);
      return 1;
    }
  }
  struct SharedState state =
      (struct SharedState){.seenTrees = NULL, .size = 0, .capacity = 0};
  iterateAllSyntaxTrees(numbers, checkAndPrintCallback, &state);
  return 0;
}
