// @ts-check

const store = new Float32Array(4096000);

const stuff = [];

onmessage = function (e) {
  var buffer = e.data;
  store.set(buffer);
  stuff.push(e.data);
};
