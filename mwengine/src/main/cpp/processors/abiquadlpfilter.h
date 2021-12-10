//
// Created by Sylvain on 2021-04-20.
// from: https://www.musicdsp.org/en/latest/Filters/259-simple-biquad-filter-from-apple-com.html
//

#ifndef MWENGINEEXPLORE_ABIQUADLPFILTER_H
#define MWENGINEEXPLORE_ABIQUADLPFILTER_H

#include "baseprocessor.h"

namespace MWEngine {
    class ABiquadLPFilter : public BaseProcessor
    {
    public:
        ABiquadLPFilter( float aCutoffFrequency, float aResonance, float aMinFreq, float aMaxFreq, int numChannels );
        ABiquadLPFilter();

        ~ABiquadLPFilter();

        void setCutoff( float frequency );
        float getCutoff();
        void setResonance( float resonance );
        float getResonance();

        void process( AudioBuffer* sampleBuffer, bool isMonoSource );


    protected:
        float _cutoff;
        float _resonance;
        float _minFreq;
        float _maxFreq;

        // used internally

        float SAMPLE_RATE;
        int amountOfChannels;
        int N;
        SAMPLE_TYPE k;
        SAMPLE_TYPE a0;
        SAMPLE_TYPE a1;
        SAMPLE_TYPE a2;
        SAMPLE_TYPE b1;
        SAMPLE_TYPE b2;
        SAMPLE_TYPE c1;
        SAMPLE_TYPE c2;
        SAMPLE_TYPE c3;
        SAMPLE_TYPE output;

        SAMPLE_TYPE* in1;
        SAMPLE_TYPE* in2;
        SAMPLE_TYPE* out1;
        SAMPLE_TYPE* out2;

        SAMPLE_TYPE* mem;

    private:
        void init( float cutoff );
        void calculateParameters();
    };
} // E.O namespace MWEngine

#endif //MWENGINEEXPLORE_ABIQUADLPFILTER_H
