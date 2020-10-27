#define main xmain
#include "../game24.c"

#undef main

static bool checkTreesAreSorted(struct SharedState *state) {
  if (state->size < 2) {
    return true;
  }
  bool result = true;
  for (uint16_t *cur = state->seenTrees + 1,
                *end = state->seenTrees + state->size;
       cur != end; ++cur) {
    if (*cur <= *(cur - 1)) {
      printf("Elements %d and %d aren't sorted: %d vs %d\n",
             (int)(cur - 1 - state->seenTrees), (int)(cur - state->seenTrees),
             (int)*(cur - 1), (int)*cur);
      result = false;
    }
  }
  return result;
}

int main() {
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
    insert(data[i], insertionPoint, &state);
    if (!checkTreesAreSorted(&state)) {
      break;
    }
  }
  free(state.seenTrees);
}
