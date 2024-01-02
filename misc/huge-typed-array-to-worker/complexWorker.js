const store = new Float32Array(4096000);

const stuff = [];

onmessage = function (e) {
  let buffer = e.data.value.value.value;
  //   store.set(buffer); // this is slow, just use the array.
  stuff.push(buffer);
};
