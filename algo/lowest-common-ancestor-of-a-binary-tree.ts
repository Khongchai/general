class TreeNode {
  val: number;
  left: TreeNode | null;
  right: TreeNode | null;
  constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
    this.val = val === undefined ? 0 : val;
    this.left = left === undefined ? null : left;
    this.right = right === undefined ? null : right;
  }
}

// Original submission: not breaking everything down enough,
// you were checking for the left and right value as well.

// When breaking a problem down, make sure to break it to the smallest part, in this case,
// the root itself.

// Refer to the next one.
function lowestCommonAncestor(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null
): TreeNode | null {
  const rootVal = root?.val;
  const rightVal = root?.right?.val;
  const leftVal = root?.left?.val;

  // where [max, min], length === 2
  const maxMinFromPQ = p!.val > q!.val ? [p, q] : [q, p];
  const _max = Math.max(rootVal!, rightVal ?? -Infinity, leftVal ?? -Infinity);
  const _min = Math.min(rootVal!, rightVal ?? Infinity, leftVal ?? Infinity);

  if (maxMinFromPQ[0]?.val === _max && maxMinFromPQ[1]?.val === _min) {
    return root;
  }

  const leftBranch: TreeNode | null = lowestCommonAncestor(
    root?.left ?? null,
    p,
    q
  );

  return leftBranch ?? lowestCommonAncestor(root?.right ?? null, p, q);
}

function lowestCommonAncestor2ndIteration(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null
): TreeNode | null {
  if ([p, q].includes(root) || !root) return root;

  const left = lowestCommonAncestor(root.left, p, q);
  const right = lowestCommonAncestor(root.right, p, q);

  if (left && right) return root;
  else if (left) return left;
  else return right;
}
