/* In some cases the program prints, with this defenition of canonicalizeTree
*  two commutativily equal equations.
*  Example with input '4 5 6 7':
*  ((6 - 4) * (5 + 7)) and
*  ((5 + 7) * (6 - 4)) */


#define canonicalizeTree xcanonTree
#include "../game24.c"
#undef canonicalizeTree

static void canonicalizeTree(SyntaxTree tree, struct Node *root) {
  findCommutativeOperator(tree, root - tree);
}
