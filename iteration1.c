#include <assert.h>
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
      return (rhs.num != 0 && lhs.num % rhs.num == 0)
                 ? makeNumber(lhs.num / rhs.num)
                 : makeInvalid();
    }
  }
  }
  CANT_REACH
}

static const char opChars[4] = {'+', '-', '*', '/'};
static void printSyntaxTreeImpl(const SyntaxTree tree,
                                const struct Node *curNode) {
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
      sizeof(*(a)))

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

static void checkAndPrintCallback(const SyntaxTree tree,
                                  const struct Node *root, void *data) {
  const EvalResult res = evalSyntaxTree(tree, root);
  if (res.valid && res.num == 24) {
    printSyntaxTree(tree, root);
    ++*((int *) data);
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
  int count = 0;
  iterateAllSyntaxTrees(numbers, checkAndPrintCallback, &count);
  if (!count) {
    puts("No solutions!");
  }
  return 0;
}
