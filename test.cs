        public bool prepare_FFT_frames(double t1, double t2) // tag1, tag2
        {
            if(disableAudio)
                return true;
            int f1 = (int)(t1 * ratio), f2 = (int)(t2 * ratio) + 1;

            if (!src.loadBufBetween(f1, f2 + 4)) // load wave into circular memory
                return false;
            for (int i = f1; i <= f2; i++)
                if (!fft_frame(i))
                    return false;
            return true;
        }