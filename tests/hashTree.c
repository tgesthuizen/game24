#define main xmain
#include "../game24.c"

#undef main

#include "common.inc"

static void checkSameTreeAlwaysHashesTheSame(const SyntaxTree x) {
  if (hashTree(x) != hashTree(x)) {
    printf("%s: %d: Hash algorithm is not deterministic for tree ", __FILE__,
           __LINE__);
    printFullTree(x);
    putchar('\n');
  }
}

static void checkCallback(const SyntaxTree x, const struct Node *root,
                          void *data) {
  (void)root;
  (void)data;
  checkSameTreeAlwaysHashesTheSame(x);
}

int main() {
  int nums[number_count] = {1, 2, 3, 4};
  iterateAllSyntaxTrees(nums, &checkCallback, NULL);
}
