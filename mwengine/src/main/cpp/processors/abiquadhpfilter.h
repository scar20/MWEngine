//
// Created by Sylvain on 2021-04-20.
// from: https://www.musicdsp.org/en/latest/Filters/259-simple-biquad-filter-from-apple-com.html
//


#ifndef MWENGINE_ABIQUADHPFILTER_H
#define MWENGINE_ABIQUADHPFILTER_H

#include "baseprocessor.h"

namespace MWEngine {
    class ABiquadHPFilter : public BaseProcessor
    {
    public:
        ABiquadHPFilter( float aCutoffFrequency, float aResonance, float aMinFreq, float aMaxFreq, int numChannels );
        ABiquadHPFilter();
        ~ABiquadHPFilter();

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

    private:
        void init( float cutoff );
        void calculateParameters();
    };
} // E.O namespace MWEngine


#endif //MWENGINE_ABIQUADHPFILTER_H
