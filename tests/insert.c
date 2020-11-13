#define main xmain
#include "../iteration2.c"

#undef main

int result = 0;

static void checkTreesAreSorted(struct SharedState *state) {
  if (state->size < 2) {
    return;
  }
  for (uint16_t *cur = state->seenTrees + 1,
                *end = state->seenTrees + state->size;
       cur != end; ++cur) {
    if (*cur <= *(cur - 1)) {
      printf("Elements %d and %d aren't sorted: %d vs %d\n",
             (int)(cur - state->seenTrees - 1), (int)(cur - state->seenTrees),
             (int)*(cur - 1), (int)*cur);
      result = 1;
    }
  }
}

static void checkGenericInsert() {
  struct SharedState state = (struct SharedState){
      .seenTrees = xmalloc(sizeof(uint16_t) * initial_cache_size),
      .size = 0,
      .capacity = initial_cache_size};
  enum { data_size = 14 };
  static const uint16_t data[data_size] = {9749, 8533,  1557,  6421, 4501,
                                           8725, 5653,  8725,  6677, 10325,
                                           8789, 13461, 10389, 2517};
  for (int i = 0; i < data_size; ++i) {
    uint16_t *const insertionPoint =
        upperBound(state.seenTrees, state.seenTrees + state.size, data[i]);
    if (*insertionPoint != data[i]) {
      insert(data[i], insertionPoint, &state);
    }
    checkTreesAreSorted(&state);
    if (result) {
      break;
    }
  }
  free(state.seenTrees);
}

int main() {
  checkGenericInsert();
  return result;
}
