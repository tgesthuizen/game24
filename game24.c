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
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#define CANT_REACH __builtin_unreachable();
#else
#define NORETURN
#define CANT_REACH
#endif

static NORETURN void handleOutOfMemory() {
  fputs("System is out of memory, aborting", stderr);
  abort();
}

static void *xmalloc(size_t size) {
  void *ptr = malloc(size);
  if (ptr) {
    return ptr;
  }
  handleOutOfMemory();
}

static void *xrealloc(void *ptr, size_t size) {
  ptr = realloc(ptr, size);
  if (ptr) {
    return ptr;
  }
  handleOutOfMemory();
}

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
  union NodeValue {
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
  CANT_REACH
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
  for (size_t i = 0; i < ops_count; ++i) {
    ++ops[i];
    if (ops[i] != op_div + 1) {
      return;
    }
    ops[i] = op_add;
  }
}

static void swap_impl(void *a, void *b, void *restrict c, size_t size) {
  memcpy(c, a, size);
  memmove(a, b, size);
  memcpy(b, c, size);
}
#define swap(a, b)                                                             \
  swap_impl(                                                                   \
      (a), (b),                                                                \
      (char[sizeof(*(a)) == sizeof(*(b)) ? (ptrdiff_t)sizeof(*(a)) : -1]){0},  \
      sizeof(*(a)));

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
    FOR_OPERAND(first, number_count)
    FOR_OPERAND(second, number_count - 1) FOR_OPERAND(third, number_count - 2) {
      // Setup the tree
      for (int i = 0; i < number_count; ++i) {
        tree[i] = (struct Node){.kind = node_number, {.n = numbers[i]}};
      }
      for (int i = 0; i < ops_count; ++i) {
        tree[i + 4] =
            (struct Node){.kind = node_operator, {.op = {ops[i], -1, -1}}};
      }
      char itab[all_count] = {0, 1, 2, 3, 4, 5, 6};
      int arenaRight = number_count;
      int curNode = number_count;
      struct Node *curOperator = tree + number_count;
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
      callback(tree, tree + all_count - 1, data);
    }
  }
}

static void findCommutativeOperator(SyntaxTree tree, struct Node *current);
static void findAdjacentNodes(SyntaxTree tree, struct Node *current, int opKind,
                              unsigned char ***arrIdxPtr);

static void analyzeCommutativeOperand(SyntaxTree tree, unsigned char *nodeIdx,
                                      enum OperatorKind opKind,
                                      unsigned char ***arrIdxPtr) {
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

static void bubbleSort(unsigned char **first, unsigned char **last) {
  bool swapped = false;
  while (!swapped) {
    swapped = true;
    for (unsigned char **pos = first + 1; pos != last; ++pos) {
      if (**pos < **(pos - 1)) {
        swap(*pos, *(pos - 1));
        swapped = false;
      }
    }
  }
}

#define MAKE_LINEAR_SEARCH(name, type)                                         \
  static type *name(type *first, type *last, type target) {                    \
    while (first != last && *first != target) {                                \
      ++first;                                                                 \
    }                                                                          \
    return first;                                                              \
  }
MAKE_LINEAR_SEARCH(findPointer, unsigned char *)

static void findCommutativeOperator(SyntaxTree tree, struct Node *current) {
  if (current->kind == node_number) {
    return;
  }
  if (current->v.op.kind == op_add || current->v.op.kind == op_mul) {
    unsigned char *operandIdxPtrs[ops_count * 2] = {NULL};
    unsigned char **operandIdxPtrsIndex = operandIdxPtrs;
    findAdjacentNodes(tree, current, current->v.op.kind, &operandIdxPtrsIndex);
    bubbleSort(
        operandIdxPtrs,
        findPointer(operandIdxPtrs, operandIdxPtrs + ops_count * 2, NULL));
  } else {
    struct Node *lhs = tree + current->v.op.lhs,
                *rhs = tree + current->v.op.rhs;
    if (lhs > rhs) {
      swap(lhs, rhs);
      swap(&current->v.op.lhs, &current->v.op.rhs);
    }
    findCommutativeOperator(tree, lhs);
    findCommutativeOperator(tree, rhs);
  }
}

static void canonicalizeTree(SyntaxTree tree, struct Node *root) {
  findCommutativeOperator(tree, root);
}

MAKE_LINEAR_SEARCH(findUChar, unsigned char)

static uint16_t hashTree(const SyntaxTree tree) {
  static const unsigned char offsets[ops_count * 3] = {0,  6, 8,  2, 10,
                                                       12, 4, 13, 14};
  const unsigned char *curOffset = offsets;
  uint16_t result = 0;
#define PLACE_BITS(bits) result |= ((unsigned char)(bits)) << *curOffset++;
  unsigned char itab[all_count] = {0, 1, 2, 3, 4, 5, 6};
  int arenaRight = number_count;
  int curNode = number_count;
  for (const struct Node *curOperator = tree + number_count,
                         *end = tree + all_count;
       curOperator != end; ++curOperator) {
    PLACE_BITS(curOperator->v.op.kind);
    unsigned char *const lhs =
        findUChar(itab, itab + all_count, curOperator->v.op.lhs);
    PLACE_BITS(lhs - itab);
    swap(lhs, itab + --arenaRight);
    unsigned char *const rhs =
        findUChar(itab, itab + all_count, curOperator->v.op.rhs);
    PLACE_BITS(rhs - itab);
    swap(rhs, itab + curNode++);
  }
  return result;
}

struct SharedState {
  uint16_t *seenTrees;
  size_t size, capacity;
};

static uint16_t *upperBound(uint16_t *first, uint16_t *last, uint16_t hash) {
  size_t sizeLeft = last - first;
  while (sizeLeft > 0) {
    size_t step = sizeLeft / 2;
    uint16_t *it = first + step;
    if (hash > *it) {
      first = ++it;
      sizeLeft -= step + 1;
    } else {
      sizeLeft = step;
    }
  }
  return first;
}

static void insert(uint16_t hash, uint16_t *pos, struct SharedState *state) {
  if (state->size == state->capacity) {
    state->capacity *= 2;
    const size_t offset = pos - state->seenTrees;
    state->seenTrees =
        xrealloc(state->seenTrees, sizeof(uint16_t) * state->capacity);
    pos = state->seenTrees + offset;
  }
  const size_t movesize = (state->seenTrees + state->size) - pos;
  memmove(pos + 1, pos, movesize * sizeof(uint16_t));
  *pos = hash;
  ++state->size;
}

static void checkAndPrintCallback(const SyntaxTree tree,
                                  const struct Node *root, void *data) {
  const EvalResult res = evalSyntaxTree(tree, root);
  if (res.valid && res.num == 24) {
    SyntaxTree copy;
    memcpy(&copy, tree, sizeof(SyntaxTree));
    struct Node *rootCopy = copy + all_count - 1;
    canonicalizeTree(copy, rootCopy);
    uint16_t hash = hashTree(copy);
    struct SharedState *state = data;
    uint16_t *end = state->seenTrees + state->size,
             *pos = upperBound(state->seenTrees, end, hash);
    if (pos == end || *pos != hash) {
      printSyntaxTree(copy, rootCopy);
      insert(hash, pos, state);
    }
  }
}

enum { initial_cache_size = 32 };

int main() {
  int numbers[number_count];
  for (int i = 0; i < number_count; ++i) {
    const int code = scanf("%d", numbers + i);
    if (code != 1) {
      fprintf(stderr, "error: Input is malformed, scanf() returned %d\n", code);
      return 1;
    }
  }
  struct SharedState state = (struct SharedState){
      .seenTrees = xmalloc(sizeof(uint16_t) * initial_cache_size),
      .size = 0,
      .capacity = initial_cache_size};
  iterateAllSyntaxTrees(numbers, checkAndPrintCallback, &state);
  free(state.seenTrees);
  return 0;
}
