#define main xmain
#include "../game24.c"

#undef main

#include "common.inc"

int result = 0;

static void checkSameTreeAlwaysHashesTheSame(const SyntaxTree x) {
  if (hashTree(x) != hashTree(x)) {
    printf("%s: %d: Hash algorithm is not deterministic for tree ", __FILE__,
           __LINE__);
    printFullTree(x);
    putchar('\n');
    result = 1;
  }
}

static void checkCallback(const SyntaxTree x, const struct Node *root,
                          void *data) {
  (void)root;
  (void)data;
  checkSameTreeAlwaysHashesTheSame(x);
}

enum { ring_buffer_size = 64 };

struct ring_buffer {
  size_t filled;
  uint16_t data[ring_buffer_size];
};

#define MIN(a, b) ((a) <= (b) ? (a) : (b))

static void checkGeneratedTreesDontHashTheSameCallback(const SyntaxTree x,
                                                       const struct Node *root,
                                                       void *data) {
  (void)root;
  const uint16_t hash = hashTree(x);
  struct ring_buffer *buffer = data;
  for (uint16_t *cur = buffer->data,
                *end = cur + MIN(ring_buffer_size, buffer->filled);
       cur != end; ++cur) {
    if (*cur == hash) {
      printf("%s: %d: Two trees hashes clash. Their hashes are %d and %d. The "
             "latter is ",
             __FILE__, __LINE__, (int)*cur, (int)hash);
      printFullTree(x);
      putchar('\n');
      result = 1;
    }
  }
  memmove(buffer->data + 1, buffer->data,
          MIN(buffer->filled, ring_buffer_size - 1));
  *buffer->data = hash;
  buffer->filled = MIN(buffer->filled + 1, ring_buffer_size);
}

int main() {
  int nums[number_count] = {1, 2, 3, 4};
  iterateAllSyntaxTrees(nums, &checkCallback, NULL);
  struct ring_buffer buffer = (struct ring_buffer){.filled = 0, .data = {0}};
  iterateAllSyntaxTrees(nums, &checkGeneratedTreesDontHashTheSameCallback,
                        &buffer);
  return result;
}
