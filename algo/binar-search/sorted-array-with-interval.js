/**
 *
 * @param {number} inMin
 * @param {number} inMax
 * @param {number} outMin
 * @param {number} outMax
 * @param {number} value
 * @returns number
 */
function remap(inMin, inMax, outMin, outMax, value) {
  return ((value - inMin) * (outMax - outMin)) / (inMax - inMin) + outMin;
}

/**
 *
 * Binary search time path for point that is less than or equal to the ratio
 *
 * @typedef {number} ratio
 */
function applyTimePathRate(ratio) {
  /**
   *
   * @return {[number ,number]} interval
   */
  function getInterval() {
    let low = 0;
    let high = audioMetadata.timePath.length - 1;
    let midVal = 0;
    while (low <= high) {
      midVal = audioMetadata.timePath[Math.floor((low + high) / 2)];
      if (midVal < ratio) {
        low = midVal + 1;
      } else if (midVal > ratio) {
        high = midVal - 1;
      } else {
        return [midVal, midVal];
      }
    }
    return [high, low];
  }

  const interval = getInterval();
  const [low, high] = interval;
  return remap(
    0,
    1,
    audioMetadata.timePath[low],
    audioMetadata.timePath[high],
    ratio
  );
}

const testCases = [
  {
    case: [1, 2, 3, 4, 5, 6, 7],
    input: 2.5,
    result: 0.5,
  },
  {
    case: [0.5, 0.5, 0.5, 0.5, 0.5, 0.5],
    input: 0.5,
    result: 0.5,
  },
  { case: [0.123, 0.432, 0.597, 0.999], input: 0.5, result: 0.5 },
];
