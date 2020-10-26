#define main xmain
#include "../game24.c"

#undef main

static struct Node makeNum(int n) {
  return (struct Node){.kind = node_number, .v = n};
}

static struct Node makeOp(enum OperatorKind k, unsigned char lhs,
                          unsigned char rhs) {
  return (struct Node){
      .kind = node_operator,
      .v = (union NodeValue){
          .op = (struct Operator){.kind = k, .lhs = lhs, .rhs = rhs}}};
}

static bool equalOperators(struct Operator *lhs, struct Operator *rhs) {
  return lhs->kind == rhs->kind && lhs->lhs == rhs->lhs && lhs->rhs == rhs->rhs;
}

static bool equalNodes(struct Node *lhs, struct Node *rhs) {
  if (lhs->kind != rhs->kind) {
    return false;
  }
  switch (lhs->kind) {
  case node_number:
    return lhs->v.n == rhs->v.n;
  case node_operator:
    return equalOperators(&lhs->v.op, &rhs->v.op);
  }
}

static bool equalSyntaxTree(SyntaxTree lhs, SyntaxTree rhs) {
  for (int i = 0; i < all_count; ++i) {
    if (!equalNodes(lhs + i, rhs + i)) {
      return false;
    }
  }
  return true;
}

static void printFullTree(SyntaxTree tree) {
  printSyntaxTree(tree, tree + all_count - 1);
}

static void checkTreeCannonIsEqual(SyntaxTree lhs, SyntaxTree rhs) {
  SyntaxTree lhsCopy, rhsCopy;
  memcpy(lhsCopy, lhs, sizeof(SyntaxTree));
  memcpy(rhsCopy, rhs, sizeof(SyntaxTree));
  canonicalizeTree(lhs, lhs + all_count - 1);
  canonicalizeTree(rhs, rhs + all_count - 1);
  if (!equalSyntaxTree(lhs, rhs)) {
    printf("%s: %d: Expected ", __FILE__, __LINE__);
    printFullTree(lhsCopy);
    printf(" to be equal to ");
    printFullTree(rhsCopy);
    printf(" in their canonicalized from (");
    printFullTree(lhs);
    printf(" vs. ");
    printFullTree(rhs);
    printf(")\n");
  }
}

static void checkTreeStaysUnchanged(SyntaxTree tree) {
  SyntaxTree copy;
  memcpy(copy, tree, sizeof(SyntaxTree));
  canonicalizeTree(copy, copy + all_count - 1);
  if (!equalSyntaxTree(tree, copy)) {
    printf("%s: %d: Tree changed in an unexpected manor: ", __FILE__, __LINE__);
    printFullTree(tree);
    printf(" -> ");
    printFullTree(copy);
    putchar('\n');
  }
}

int main() {
#define THE_NUMS makeNum(1), makeNum(2), makeNum(3), makeNum(4)
  checkTreeCannonIsEqual(
      (SyntaxTree){THE_NUMS, makeOp(op_add, 3, 2), makeOp(op_add, 1, 4),
                   makeOp(op_add, 0, 5)},
      (SyntaxTree){THE_NUMS, makeOp(op_add, 2, 1), makeOp(op_add, 3, 4),
                   makeOp(op_add, 0, 5)});
  checkTreeCannonIsEqual(
      (SyntaxTree){THE_NUMS, makeOp(op_add, 0, 1), makeOp(op_sub, 2, 3),
                   makeOp(op_div, 4, 5)},
      (SyntaxTree){THE_NUMS, makeOp(op_add, 1, 0), makeOp(op_sub, 2, 3),
                   makeOp(op_div, 4, 5)});
  checkTreeStaysUnchanged((SyntaxTree){THE_NUMS, makeOp(op_sub, 0, 1),
                                       makeOp(op_sub, 2, 3),
                                       makeOp(op_sub, 4, 5)});
}
