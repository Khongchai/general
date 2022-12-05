// With indices as values
function containsNearbyDuplicate(nums: number[], k: number) {
  const arr: number[] = [];
  const abs = Math.abs;

  for (let i = 0, length = nums.length; i < length; i++) {
    if (arr[nums[i]] != undefined) {
      const j = arr[nums[i]];

      if (abs(i - j) <= k) return true;
    }

    arr[nums[i]] = i;
  }

  return false;
}

// https://medium.com/outco/how-to-solve-sliding-window-problems-28d67601a66
// With sliding window.
/**
 * function containsNearbyDuplicate(nums: number[], k: number): boolean {
    if (!k) return false;
    const set = new Set<number>();
    for (let i = 0; i < nums.length; i++) {
        if (set.has(nums[i])) {
            return true;
        } else {
            if (set.size === k) set.delete(nums[i - k]);
            set.add(nums[i]);
        }
    }
    return false;
};
 */
