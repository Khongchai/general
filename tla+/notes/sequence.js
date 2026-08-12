// 1. Define the base set S (the set of tuples)
const S = [
  ["d1", 0],
  ["d1", 1],
];

function* generateSeq(baseSet) {
  yield [];
  let previousLengthSequences = [[]];

  while (true) {
    const currentLengthSequences = [];

    for (const seq of previousLengthSequences) {
      for (const item of baseSet) {
        const newSeq = [...seq, item];
        yield newSeq;
        currentLengthSequences.push(newSeq);
      }
    }

    previousLengthSequences = currentLengthSequences;
  }
}

const seqGenerator = generateSeq(S);

for (let i = 0; i < 1000; i++) {
  const element = seqGenerator.next().value;
  console.log(
    `Element ${i.toString().padStart(2, " ")}:`,
    JSON.stringify(element),
  );
}
