function main() {
  const then = performance.now();

  calcLines(1000001, 0.0, 0.0006283185307179586, 100.0);

  const now = performance.now() - then;

  console.log("Time elapsed in calcLines is: " + now + "ms");
}

function calcLines(
  points: number,
  theta: number,
  step: number,
  rodLength: number
): number[] {
  const arr = [];
  let firstTime = true;
  let prevPoint = [];
  let newPoint = [];

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

    prevPoint = newPoint;

    theta += step;
  }

  return arr;
}

const PI = Math.PI;
const cos = Math.cos;
const sin = Math.sin;

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
    newPoint[0] += (d[0] + d[1]) * cos(theta * d[3] - PI * 0.5 * d[2]);
    newPoint[1] += (d[0] + d[1]) * sin(theta * d[3] + PI * 0.5 * d[2]);
  }
}
