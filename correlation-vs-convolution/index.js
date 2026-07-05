// @ts-check

const l = 1024;
const sin = new Float32Array(Array.from({ length: l }, (_, i) => Math.sin(i)));

const cos = new Float32Array(Array.from({ length: l }, (_, i) => Math.cos(i)));

/**
 * @param {Float32Array[]} arrs
 */
function dot(...arrs) {
  const length = arrs[0]?.length;
  if (!length) {
    throw new Error("invalid array size");
  }
  if (arrs.some((x) => x.length !== length)) {
    throw new Error("Arrays size don't match");
  }
  let s = 0;
  for (let i = 0; i < length; i++) {
    s += arrs.reduce((prev, cur) => cur[i] * prev, 1);
  }
  return s;
}

/**
Before any sliding, compute a single number: Σ sin[i] · cos[i] and Σ sin[i] · sin[i].

Predict first: which one is near zero, which one is near 512? Why 512 and not 1024? (Hint: what's the average value of sin²?)

This is the core insight: correlation at one lag is just a dot product. "How much do these signals point in the same direction?"
 */
{
  const sinCos = dot(sin, cos);
  const sinSin = dot(sin, sin);
  console.info(`sin * cos = ${sinCos} ; sin * sin = ${sinSin}`);
}

/**
    Experiment 2: the sliding loop — correlation
    Now write a loop over lags n from, say, −20 to +20. For each lag, compute the dot product of sin[m] against cos[m + n] (decide what to do when m + n falls off the end — for now just skip those terms).

    Predict first:

    sin(x) = cos(x − π/2). So at what lag n should cos "line up" with sin and the correlation peak?
    Your samples are 1 radian apart, so π/2 ≈ 1.57 samples. But your lags are integers. What will the peak look like?
    Then plot (or just console.log the array and eyeball it). You should discover something pretty: the correlation as a function of lag is itself a sinusoid. Worth sitting with why — try expanding sin(m)·cos(m+n) with a product-to-sum identity if you want the analytical answer.
 */
{
  const lags = { from: -20, to: 20 };
  const outLength = lags.to - lags.from;
  const out = new Float32Array(outLength);

  for (let n = lags.from; n < lags.to; n++) {
    let s = 0;
    for (let m = 0; m < l; m++) {
      const input = m;
      const kernel = (m + n) % l;
      s += sin[input] * /** @type {number} */ (cos.at(kernel));
    }
    out[n] = s;
  }
  console.info(`corr: ${out}`);
}

/**
Experiment 3: same loop, flipped index — convolution
Copy your loop, change g[m + n] to g[n − m]. Compare the outputs.

Predict first, using symmetry:

cos is an even function: flipping it does nothing. So conv(f, cos) vs corr(f, cos) — same or different?
sin is odd: flipping negates it. So how should conv(cos, sin) relate to corr(cos, sin)?
This is the punchline of the whole topic: for symmetric kernels, convolution and correlation are identical — which is why CNNs "convolve" but actually implement correlation and nobody cares.
 */

/**
Experiment 4: what are they each for
Two mini-projects to feel the difference in purpose:

Correlation = finding things. Make a noisy signal: noise[i] = sin[i + 37] + (Math.random() - 0.5) * 2. Correlate it against clean sin. Can you recover the 37-sample shift from the peak location? (This is sonar, GPS, template matching.)
Convolution = filtering. Convolve your noisy signal with the tiny kernel [0.25, 0.25, 0.25, 0.25]. Look at values before/after. What happened to the noise? Why does flipping not matter here but would matter if the kernel were [0.7, 0.2, 0.1] (an "echo" — most energy first)?

 */
