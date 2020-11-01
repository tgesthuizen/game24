/* In some cases the program prints, with this defenition of canonicalizeTree
*  two commutativily equal equations.
*  Example with input '4 5 6 7':
*  ((6 - 4) * (5 + 7)) and
*  ((5 + 7) * (6 - 4)) */


#define canonicalizeTree xcanonTree
#define checkAndPrintCallback xCAPCallback
#define main xmain
#include "../game24.c"
#undef canonicalizeTree
#undef checkAndPrintCallback
#undef main

static void canonicalizeTree(SyntaxTree tree, struct Node *root) {
  findCommutativeOperator(tree, root - tree);
}

static void checkAndPrintCallback(const SyntaxTree tree,
                                  const struct Node *root, void *data) {
  const EvalResult res = evalSyntaxTree(tree, root);
  if (res.valid && res.num == 24) {
    SyntaxTree copy;
    memcpy(&copy, tree, sizeof(copy));
    struct Node *const rootCopy = copy + all_count - 1;
    canonicalizeTree(copy, rootCopy);
    const uint16_t hash = hashTree(copy);
    struct SharedState *state = data;
    uint16_t *end = state->seenTrees + state->size,
             *pos = upperBound(state->seenTrees, end, hash);
    if (pos == end || *pos != hash) {
#ifdef DEBUG_PRINT
      printf("-------------------------\n");
      debugPrintTree(tree);
      debugPrintTree(copy);
      printf("Found hash %d\n", (int)hash);
#endif
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
  struct SharedState state = (struct SharedState){
      .seenTrees = xmalloc(sizeof(uint16_t) * initial_cache_size),
      .size = 0,
      .capacity = initial_cache_size};
  iterateAllSyntaxTrees(numbers, checkAndPrintCallback, &state);
  free(state.seenTrees);
  return 0;
}
