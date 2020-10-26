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

static bool equalOperators(struct Operator *lhs, struct Operator *rhs) {
  return lhs->kind == rhs->kind && lhs->lhs == rhs->lhs && lhs->rhs == rhs->rhs;
}

static bool equalNodes(struct Node *lhs, struct Node *rhs) {
  if (lhs->kind != rhs->kind) {
    return false;
  }
  switch (lhs->kind) {
  case node_number:
    return lhs->v.n == rhs->v.n;
  case node_operator:
    return equalOperators(&lhs->v.op, &rhs->v.op);
  }
  __builtin_unreachable();
}

static bool equalSyntaxTree(SyntaxTree lhs, SyntaxTree rhs) {
  for (int i = 0; i < all_count; ++i) {
    if (!equalNodes(lhs + i, rhs + i)) {
      return false;
    }
  }
  return true;
}

static void printFullTree(SyntaxTree tree) {
  printSyntaxTree(tree, tree + all_count - 1);
}
