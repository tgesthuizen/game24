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
  char lhs, rhs;
};
enum NodeKind { node_number, node_operator };
struct Node {
  enum NodeKind kind;
  union {
    int n;
    struct Operator op;
  } v;
};

typedef struct Node SyntaxTree[7];

static void incrementOperators(enum OperatorKind *ops) {
  ++(*ops);
  while (*ops == op_div) {
    *ops++ = op_add;
    ++(*ops);
  }
}

static void swap(char *a, char *b) {
  int c = *a;
  *a = *b;
  *b = c;
}

int evalSyntaxTree(const SyntaxTree tree, const struct Node *curNode) {
  switch (curNode->kind) {
  case node_number:
    return curNode->v.n;
  case node_operator: {
    int lhs = evalSyntaxTree(tree, tree + curNode->v.op.lhs),
        rhs = evalSyntaxTree(tree, tree + curNode->v.op.rhs);
    switch (curNode->v.op.kind) {
    case op_add:
      return lhs + rhs;
    case op_sub:
      return lhs - rhs;
    case op_mul:
      return lhs * rhs;
    case op_div:
      return lhs / rhs;
    }
  }
  }
}

const char opChars[4] = {'+', '-', '*', '/'};

void printSyntaxTree(const SyntaxTree tree, const struct Node *curNode) {
  switch (curNode->kind) {
  case node_number:
    printf("%d", curNode->v.n);
    break;
  case node_operator:
    putchar('(');
    printSyntaxTree(tree, tree + curNode->v.op.lhs);
    printf(" %c ", opChars[curNode->v.op.kind]);
    printSyntaxTree(tree, tree + curNode->v.op.rhs);
    putchar(')');
  }
}

void iterateAllSyntaxTrees(const int numbers[4],
                           void (*callback)(const SyntaxTree tree,
                                            const struct Node *root)) {
  SyntaxTree tree;
  static const enum OperatorKind finalOps[3] = {op_div, op_div, op_div};
  enum OperatorKind ops[3] = {op_add, op_add, op_add};

  for (; memcmp(ops, finalOps, sizeof(ops)) != 0; incrementOperators(ops)) {
#define FOR_VAR(name, top) for (int name = 0; name < top; ++name)
#define FOR_OPERAND(name, top)                                                 \
  FOR_VAR(name##_lhs, top) FOR_VAR(name##_rhs, top - 1)
    FOR_OPERAND(first, 4) FOR_OPERAND(second, 3) FOR_OPERAND(third, 2) {
      // Setup the tree
      for (int i = 0; i < 4; ++i) {
        tree[i] = (struct Node){.kind = node_number, {.n = numbers[i]}};
      }
      for (int i = 0; i < 3; ++i) {
        tree[i + 4] = (struct Node){.kind = node_operator, {.op = ops[i]}};
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
      callback(tree, tree + 6);
    }
  }
}

static void checkAndPrintCallback(const SyntaxTree tree, const struct Node *root) {
  if (evalSyntaxTree(tree, root) == 42) {
    printSyntaxTree(tree, root);
  }
}

int main(int argc, char **argv) {
  int numbers[number_count];
  for (int i = 0; i < number_count; ++i) {
    const int code = scanf("%d", numbers + i);
    if (code != 1) {
      fprintf(stderr, "error: Input is malformed, scanf() returned %d\n", code);
      return 1;
    }
  }
  iterateAllSyntaxTrees(numbers, checkAndPrintCallback);
}
