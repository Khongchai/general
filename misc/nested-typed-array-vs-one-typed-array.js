// @ts-check

const randomData = new Float32Array(2048 * 100);

const nestedArraySize = 2048;

const nestedTypedArray = new Array(randomData.length / nestedArraySize).fill(
  new Float32Array()
);

const oneTypedArray = new Float32Array(randomData.length);

console.time("oneTypedArray");

oneTypedArray.set(randomData);

console.timeEnd("oneTypedArray");

console.time("nestedTypedArray");

for (let i = 0; i < nestedTypedArray.length; i++) {
  nestedTypedArray[i] = randomData.subarray(
    i * nestedArraySize,
    (i + 1) * nestedArraySize
  );
}

console.timeEnd("nestedTypedArray");
