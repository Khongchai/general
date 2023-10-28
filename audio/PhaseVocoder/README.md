# Draft I

# Notes for managing buffer for interpolation-based, time-stretching vocoder with the web audio API for real-time processing

Web audio API has an audio block of size 128 samples, so working with hopsize of 128 is the easiest. However, let's also explore hopsize of 512 samples as well.

# 128

Before we begin, for a playbackRate ratio `R`, make sure the audio destination accept `originalBufferLength` * R. Otherwise the browser might stop before, or after we are done processing.

## 1.

Our ideal frame size is 2048 samples. To interpolate between two frames, we'd need 2048 + 128 length buffer to store the data we need for each resampling.

## 2.

Create two views for the data inside the the prepared buffer of length 2048 + 128, or `frameSize` _(aka analysis frame)_ + `hopSize` = `synthesisFrame`. The first one is 0 - 2048, and the second, 2048 - 2176. This can be achieved with .subArray(), you will get the pointers to those places in memory. Let these two arrays be `frameA` and `frameB`.

## 3.

Pre-fill `frameSize` + `hopSize` with audio data, then make sure to start the playback not at the beginning of an audio track, but at `frameSize` + `hopSize`.

## 4.

When the playback begins, the vocoder will be fed a stream of 128 sample-sized audio block. That 128 samples will actually be processed in the future, we send the existing synthesis frame to the vocoder, but not directly. We prepare it by creating 2 views of the array called frameA and frameB. The vocoder will then interpolate between these two frames.

## 5.

After the resampling, we remove the first item from the analysis frame, and put the new 128 samples to the end of it. We do this in-place. No new array creation. In js, this can be achieved with `copyWithin`.

## 6.

The removed item from the previous step is the one we'll pass to audio out in web audio API each iteration. 

## 7. 

Keep going until we reach the end of the track. When we reach the end, we'll receive 0s from the process method. No special handling needed because `interpolate(0, x)` where `x` is any non-zero number is `x`.








