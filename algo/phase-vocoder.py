import numpy as np
import matplotlib.pyplot as plt

# Create a simple sine wave
fs = 44100  # Sampling rate
t = np.linspace(0., 1., fs)  # 1 second of audio
freq = 5  # 5 Hz
original_signal = np.sin(2 * np.pi * freq * t)

# Plot original signal
plt.figure()
plt.title('Original Signal')
plt.plot(original_signal)
plt.xlabel('Sample')
plt.ylabel('Amplitude')
plt.grid(True)

# Perform simple "phase vocoder" time stretching (double the length)
stretch_factor = 3
new_length = int(len(original_signal) * stretch_factor)
stretched_signal = np.interp(
    np.linspace(0., 1., new_length), np.linspace(0., 1., len(original_signal)), original_signal)

# Plot stretched signal
plt.figure()
plt.title('Stretched Signal')
plt.plot(stretched_signal)
plt.xlabel('Sample')
plt.ylabel('Amplitude')
plt.grid(True)

plt.show()
