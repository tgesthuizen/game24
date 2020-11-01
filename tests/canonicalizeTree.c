#define main xmain
#include "../game24.c"

#undef main

#include "common.inc"
int result = 0;

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
    debugPrintTree(lhsCopy);
    printf(" -> ");
    debugPrintTree(lhs);
    printf(" vs. \n");
    debugPrintTree(rhsCopy);
    printf(" -> ");
    debugPrintTree(rhs);
    putchar('\n');
    result = 1;
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
    result = 1;
  }
}

int main() {
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
  checkTreeCannonIsEqual(
      (SyntaxTree){THE_NUMS, makeOp(op_add, 0, 1), makeOp(op_add, 4, 2),
                   makeOp(op_mul, 3, 5)},
      (SyntaxTree){THE_NUMS, makeOp(op_add, 1, 2), makeOp(op_add, 4, 0),
                   makeOp(op_mul, 3, 5)});
  // In f593971 the following trees were considered equal.
  // (4 * (6 * (7 / 5)))
  // ((4 * 6) * (7 / 5))
  checkTreeCannonIsEqual(
      (SyntaxTree){makeNum(4), makeNum(5), makeNum(6), makeNum(7),
                   makeOp(op_div, 3, 1), makeOp(op_mul, 2, 4),
                   makeOp(op_mul, 0, 5)},
      (SyntaxTree){makeNum(4), makeNum(5), makeNum(6), makeNum(7),
                   makeOp(op_div, 3, 1), makeOp(op_mul, 0, 2),
                   makeOp(op_mul, 4, 5)});
  // In 2deb10b the following trees were considered equal.
  // ((1 + 3) * (2 + 4))
  // ((2 + 4) * (1 + 3))
  checkTreeCannonIsEqual(
      (SyntaxTree){THE_NUMS, makeOp(op_add, 0, 2), makeOp(op_add, 3, 1),
                   makeOp(op_mul, 4, 5)},
      (SyntaxTree){THE_NUMS, makeOp(op_add, 1, 3), makeOp(op_add, 0, 2),
                   makeOp(op_mul, 5, 4)});
  checkTreeStaysUnchanged((SyntaxTree){THE_NUMS, makeOp(op_sub, 0, 1),
                                       makeOp(op_sub, 2, 3),
                                       makeOp(op_sub, 4, 5)});
  return result;
}
