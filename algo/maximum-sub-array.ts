/**
 * Input: nums = [-2,1,-3,4,-1,2,1,-5,4]
 * Output: 6
 * Explanation: [4,-1,2,1] has the largest sum = 6.
 */
// Naive, actually kind of prefer this one.
function maxSubArray(nums: number[]): number {
  let max = nums[0];
  let sum = 0;
  for (let i = 0; i < nums.length; i++) {
    sum += nums[i];
    if (sum > max) max = sum;
    if (sum < 0) sum = 0;
  }
  return max;
}
//www.geeksforgeeks.org/largest-sum-contiguous-subarray/
function maxSubArrayKadane(nums: number[]): number {
  let maxSoFar = nums[0];
  let maxEndingHere = nums[0];

  for (let i = 1; i < nums.length; i++) {
    maxEndingHere = Math.max(nums[i] + maxEndingHere, nums[i]);
    maxSoFar = Math.max(maxEndingHere, maxSoFar);
  }

  return maxSoFar;
}
