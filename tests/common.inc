static struct Node makeNum(int n) {
  return (struct Node){.kind = node_number, .v = {n}};
}

static struct Node makeOp(enum OperatorKind k, unsigned char lhs,
                          unsigned char rhs) {
  return (struct Node){
      .kind = node_operator,
      .v = (union NodeValue){
          .op = (struct Operator){.kind = k, .lhs = lhs, .rhs = rhs}}};
}

static bool equalOperators(const struct Operator *lhs, const struct Operator *rhs) {
  return lhs->kind == rhs->kind && lhs->lhs == rhs->lhs && lhs->rhs == rhs->rhs;
}

static bool equalNodes(const struct Node *lhs, const struct Node *rhs) {
  if (lhs->kind != rhs->kind) {
    return false;
  }
  switch (lhs->kind) {
  case node_number:
    return lhs->v.n == rhs->v.n;
  case node_operator:
    return equalOperators(&lhs->v.op, &rhs->v.op);
  }
  CANT_REACH
}

static bool equalSyntaxTree(const SyntaxTree lhs, const SyntaxTree rhs) {
  for (int i = 0; i < all_count; ++i) {
    if (!equalNodes(lhs + i, rhs + i)) {
      return false;
    }
  }
  return true;
}

static void printFullTree(const SyntaxTree tree) {
  printSyntaxTree(tree, tree + all_count - 1);
}

#define THE_NUMS makeNum(1), makeNum(2), makeNum(3), makeNum(4)