function compose<A, B, C>({
  g,
  f,
}: {
  g: (value: A) => B;
  f: (value: B) => C;
}) {
  return (value: A) => f(g(value));
}

// number -> string

function add(tuple: [number, number]) {
  return tuple[0] + tuple[1];
}

const addAndStringify = compose<[number, number], number, string>({
  f: String,
  g: add,
});

addAndStringify([2, 3]);
