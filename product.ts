const dfs = (
  i: number,
  groups: string[][],
  out: string[],
  path: string[],
  depth = 0,
): void => {
  if (i === groups.length) {
    out.push(path.join(" AND "));
    return;
  }
  for (const s of groups[i]) {
    console.log(`${depth}, ${s}`);
    path.push(s);
    dfs(i + 1, groups, out, path, depth + 1);
    path.pop();
  }
};

function product(groups: string[][]): string[] {
  const out: string[] = [];
  const path: string[] = [];
  if (groups.length > 0) dfs(0, groups, out, path);
  return out;
}

console.log(
  product([
    ["a", "b"],
    ["c", "d"],
    ["e", "f"],
  ]),
);
