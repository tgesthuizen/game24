#define main xmain
#include "../game24.c"

#undef main

#include "common.inc"
#include "countTreeNodes.inc"

static int validateTree(const SyntaxTree tree, const char *where, int *ret) {
  struct CountState state = {.ops = 0, .nums = 0};
  recurseCountTree(tree, tree + all_count - 1, &state);
  if (state.nums == number_count && state.ops == ops_count) {
    return 0;
  }
  printf("%s: %d: Tree has %d ops and %d nums %s\n", __FILE__, __LINE__,
         state.ops, state.nums, where);
  debugPrintTree(tree);
  *ret = 1;
  return 1;
}

static void callback(const SyntaxTree tree, const struct Node *root,
                     void *data) {
  validateTree(tree, "before canonicalization", data);
  SyntaxTree copy;
  memcpy(copy, tree, sizeof(SyntaxTree));
  canonicalizeTree(copy, copy + all_count - 1);
  if (validateTree(copy, "after canonicalization", data)) {
    printf("%s: %d: From: ", __FILE__, __LINE__);
    debugPrintTree(tree);
  }
}

int main() {
  int res = 0;
  iterateAllSyntaxTrees((int[4]){1, 2, 3, 4}, callback, &res);
  return res;
}
