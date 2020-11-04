#define main xmain
#include "../game24.c"

#undef main

#include "common.inc"

struct CountState {
  unsigned char ops;
  unsigned char nums;
};

static void recurseCountTree(SyntaxTree tree, struct Node *cur,
                             struct CountState *state) {
  switch (cur->kind) {
  case node_number:
    ++state->nums;
    break;
  case node_operator:
    ++state->ops;
    recurseCountTree(tree, tree + cur->v.op.lhs, state);
    recurseCountTree(tree, tree + cur->v.op.rhs, state);
    break;
  }
}

static void callback(const SyntaxTree tree, const struct Node *root,
                     void *data) {
  SyntaxTree copy;
  memcpy(copy, tree, sizeof(SyntaxTree));
  canonicalizeTree(copy, copy + all_count - 1);
  struct CountState state = {.ops = 0, .nums = 0};
  recurseCountTree(copy, copy + all_count - 1, &state);
  if (state.nums != number_count || state.ops != ops_count) {
    printf("%s: %d: Tree has %d ops and %d nums after canonicalization!\n",
           __FILE__, __LINE__, state.ops, state.nums);
    debugPrintTree(tree);
    fputs("-> ", stdout);
    debugPrintTree(copy);
    *((int *) data) = 1;
  }
}

int main() {
  int res = 0;
  iterateAllSyntaxTrees((int[4]){1, 2, 3, 4}, callback, &res);
  return res;
}
