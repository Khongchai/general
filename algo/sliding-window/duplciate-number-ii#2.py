"""
  Given an integer array nums and an integer k, return true if there are 
  two distinct indices i and j in the array such that nums[i] == nums[j] and 
  abs(i - j) <= k.
 """
 # Simplest solution :)
def containsNearbyDuplicateII(nums, k):
  s = set()
  for i in range(len(nums)):
    if nums[i] in s: return True

    s.add(nums[i])
    
    if i >= k: s.remove(nums[i - k])


    
