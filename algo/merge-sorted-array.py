"""
Do not return anything, modify nums1 in-place instead.
 
A few cases to help visualize.
[4, 5, 6, 0, 0, 0] & [1, 2, 3]
[1, 2, 3, 0, 0, 0] & [4, 5, 6];
[1, 2, 3, 0, 0, 0] & [2, 5, 6];

# the "and m" case prevents negative indices like with the first case above.

"""
def merge(nums1, m, nums2, n):
    while(n):
       if nums1[m-1] > nums2[n-1] and m:
           nums1[m+n-1] = nums1[m-1]
           m-=1
       else :
           nums1[m+n-1] = nums2[n-1]
           n-=1

    print(nums1)

merge([1, 2, 3, 0, 0, 0], 3, [2, 5, 6], 3)
merge([4, 5, 6, 0, 0, 0], 3, [1, 2, 3], 3)
