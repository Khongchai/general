const then = performance.now();
for (let i = 0; i < 100000000; i++) {
  for (let j = 0; j < 20; j++) {}
}

const now = performance.now();
const diff = now - then;

console.log(diff);
