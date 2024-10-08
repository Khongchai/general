// [1,5,9,1,5,9]
// k = 2
// t = 3
// Output: true, expect: false

/**
 * 1 to zeroth bucket
 * 5 goes to 1st bucket
 * 9 goes to 2nd bucket
 */

function containsNearbyAlmostDuplicate(nums: number[], k: number, t: number) {
  if (t < 0) return false;

  const nLength = nums.length;
  const bucket = {};
  const bucketSize = t + 1;
  const floor = Math.floor;
  const abs = Math.abs;
  for (let i = 0; i < nLength; i++) {
    const bucketNo = floor(nums[i] / bucketSize);
    // There's no need to store multiple values in the bucket, because
    // that is when we return true. So this is a bit like, half-way bucket-sorting.
    if (bucket[bucketNo] != undefined) return true;

    if (
      bucket[bucketNo - 1] != undefined &&
      abs(nums[i] - bucket[bucketNo - 1]) < bucketSize
    ) {
      return true;
    }

    if (
      bucket[bucketNo + 1] != undefined &&
      abs(nums[i] - bucket[bucketNo + 1]) < bucketSize
    ) {
      return true;
    }

    bucket[bucketNo] = nums[i];

    // One of the constraints we have is that abs(i - j) <= k,
    // where i is the index of the first integer, and j the second.
    //
    // To keep them less than k, we can use sliding window and cut out the index that is "outdated",
    // or "expired".
    if (i >= k) {
      // i / bucketSize gets us the bucket number of the integer i.
      // (i - k) / bucketSize, then, gets us the bucket number of the integer i - k,
      // where i - k is the index of the integer that is out of the window.
      delete bucket[floor(nums[i - k] / bucketSize)];
    }
  }

  return false;
}
