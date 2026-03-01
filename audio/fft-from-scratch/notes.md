TODO 

- [ ] Understand FFT.
    - [ ] Ask AI to quiz u
    - [ ] Should be able to understand 
- [ ] wtf is wrong with the visualization.
- [ ] Inverse DFT?
- [ ] Inverse FFT...
- [ ] Map from frequency domain back to SOUND.


Reason radix 2 / 4 / 8 ... for all even numbers are a thing because only even-number root of unity have a perfect "mirror" image and therefore can be exploited.

The "mirror" thing however is not what makes fft that fast. As we can see from the `dftBetter` function in `fft.html`, it helps makes things faster, but not as fast as fft. That is all attributed to divide and conquer strategy, which we can be algebraically derived.