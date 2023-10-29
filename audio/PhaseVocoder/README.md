# Draft II

# Notes for managing buffer for interpolation-based, time-stretching vocoder with the web audio API for real-time processing

Web audio API has an audio block of size 128 samples, so working with hopsize of 128 is the easiest. However, let's also explore hopsize of 512 samples as well.

# 128

## 1.

Our ideal frame size is 2048 samples. To interpolate between two frames, we'd need 2048 + 128 length buffer to store the data we need for each resampling for both `input` and `output`. Call this 2048 + 128 a `synthesisFrame`.

## 2.

Create two array views on the `synthesisFrame`. These two views `left` and `right` are both of size 2048.

`leftFrame` holds pointers to 2048 samples, from 0 - 2048. 

`rightFrame` holds pointers to 2048 samples, from 128 - 2176.

The two views do not contain their own copy of the data, they just hold pointers to the data in `synthesisFrame`.

`analysisFrameToSend.left` holds a copy of `analysisFrame.left` data, and `analysisFrameToSend.right` holds a copy of `analysisFrame.right` data. These two arrays are passed to the vocoder to process.

## 3 

### Optimized, no delay
_(optional, get it to work first, 50 millisecs delay for 44.1khz might not be that bad...)_

Pre-fill the synthesisFrame with the first 2048 + 128 samples before the audio start, and start the playback at 2048 + 128 sample.

### Naive

We buffer for the first 2048 + 128 samples. Basically, no sound is played for the first 2048 + 128 samples. We just buffer them. We start the playback at 2048 + 128 sample.

## 4.

When the playback begins, the browser will send a stream of 128 sample-sized audio block. We hold on to that 128 samples. We copy everything from `analysisFrame` to `analysisFrameToSend` and then, **in place**, remove first 128 samples from `analysisFrame` and put the new 128 samples to the end of it.

## 5. 

Send `newFrameToSend` and `previousFrame` to the vocoder. The vocoder interpolates between these two. We tell the vocoder to store the output into the `outputs` array the browser gave us (passed to `process` method of `AudioWorkletProcessor`).

## 6. 

Copy everything from `outputs` to `previousFrame` and send the first 128 samples to the browser.

## 7. 
Keep going until we reach the end of the track. When we reach the end, we'll receive 0s from the process method. No special handling needed because `interpolate(0, x)` where `x` is any non-zero number is `x` (check again, not sure).

# 512 (or whatever, that's not 128)

While 128 is good, we end up doing a lot of resampling, maybe even more that necessary. I can't really hear any difference between hop size of 512 and 128, and 512 gives me better performance, so I'll take 512. Unfortunately, we need some extra buffering to make 512 possible.

## 1.

Same as previous step 1, but to interpolate between two frame,s we now need 2048 + 512.

## 2.

Create two arrays, both of size 2048

`previousFrame` holds the processed frequency data of the first 2048 samples from 0 - 2048.
 
`newFrame` holds the unprocessed 2048 samples from 512 - 2560.

`newFrameToSend` holds a copy of `newFrame` data for the vocoder to process.

## 3.

Same as previous step 3.

## 4.

Same as previuos step 4. 

## 5.

We perform this only if we have processed 512 samples already. We need a counter `c` to help keep track. `c` is calculated as `c = (c + webAudioBlockSize) % 512`. If `c` is 0, we send `newFrameToSend` and `previousFrame` to the vocoder, else we skip the vocoder and go to step 6.

## 6.

Don't copy, just send the first `c + 128` samples from output to the browser if `c` is not 0. If `c` is 0, copy everything from `outputs` to `previousFrame` and send the first 128 samples to the browser.

## 7.

Same as previous step 7.


# Edge Cases

- When audio is shorter than 2048 samples, just play the audio as-is.







