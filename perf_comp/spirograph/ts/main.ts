main();

function main() {
  const then = performance.now();

  const result = calcLines(10000, 0.0, 0.0006283185307179586, 100.0);

  const now = performance.now();

  const elapsed = now - then;

  console.log("Sample results: " + result[0], result[2], result[100]);
  console.log("result length: " + result.length);
  console.log("Time elapsed in calcLines is: " + elapsed + "ms");
}

function calcLines(
  points: number,
  theta: number,
  step: number,
  rodLength: number
): number[] {
  const arr: number[] = [];
  let firstTime = true;
  let prevPoint = [0, 0];
  let newPoint = [0, 0];

  let parsedData = [
    [300.0, 100.0, 0.0, 1.0],
    [100.0, 50.0, 1.0, 5.11],
    [50.0, 25.0, 1.0, 5.11],
    [25.0, 50.0, 0.0, 7.0],
  ];

  for (let i = 0; i < points; i++) {
    computeEpitrochoid(parsedData, theta, rodLength, newPoint);

    if (firstTime) {
      firstTime = false;
    } else {
      arr.push(prevPoint[0], prevPoint[1], newPoint[0], newPoint[1]);
    }

    prevPoint[0] = newPoint[0];
    prevPoint[1] = newPoint[1];

    theta += step;
  }

  return arr;
}

function computeEpitrochoid(
  data: number[][],
  theta: number,
  rodLength: number,
  newPoint: number[]
) {
  if (data.length < 2) {
    throw new Error("Provide at least 2 cycloids");
  }

  for (let i = 0; i < data.length; i++) {
    const d = data[i];
    newPoint[0] +=
      (d[0] + d[1]) * Math.cos(theta * d[3] - Math.PI * 0.5 * d[2]);
    newPoint[1] +=
      (d[0] + d[1]) * Math.sin(theta * d[3] + Math.PI * 0.5 * d[2]);
  }

  newPoint[0] += rodLength * Math.cos(theta);
  newPoint[1] += rodLength * Math.sin(theta);
}
